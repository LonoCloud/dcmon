#!/usr/bin/env nbb
(ns dcmon
  (:require [clojure.string :as S]
            [clojure.pprint :refer [pprint]]
            [promesa.core :as P]
            [cljs-bean.core :refer [->clj]]
            [reagent.core :as reagent]
            ["fs" :as fs]
            ["util" :refer [promisify]]
            ["stream" :as stream]
            ["neodoc" :as neodoc]
            ["ink" :refer [render Text Box]]
            ["js-yaml" :as yaml]
            ["dockerode$default" :as Docker]))

(def usage "
Usage:
  dcmon [options] <project> <checks-file>...

Options:
  --verbose              Verbose output [env: VERBOSE]
  --keep-running         Don't exit when done setting is reached
  --log-file LOG-FILE    Output all events to LOG-FILE
  --show-events EVTS     Output events to the screen
                         EVTS is comma separate list of events types or 'all'
  --verbose-events       Show full docker events
  --no-tui               Disable TUI (ink/React) visual representation
  --timeout TIMEOUT      Timeout after TIMEOUT seconds
                         (exit with error if not in finished state)
")

(def WAIT-EXEC-SLEEP 200)
(def TICK-PERIOD 500 #_200)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument processing
(defn clean-opts [arg-map]
  (reduce (fn [o [a v]]
            (let [k (keyword (S/replace a #"^[-<]*([^>]*)[>]*$" "$1"))]
              (assoc o k (or (get o k) v))))
          {} arg-map))

(defn parse-opts [argv]
  (-> usage
      (neodoc/run (clj->js {:optionsFirst true
                            :smartOptions true
                            :argv argv}))
      js->clj
      clean-opts))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General utility functions

(def slurp-buf (promisify (.-readFile fs)))

;; async
(defn wait-exec [exec]
  (P/create
    (fn [resolve reject]
      (let [check-fn (atom nil)
            exec-cb (fn [err data]
                      (if err (reject err)
                        (if (.-Running data)
                          (js/setTimeout @check-fn WAIT-EXEC-SLEEP)
                          (resolve data))))]
        (reset! check-fn (fn []
                           (.inspect exec exec-cb)))
        (@check-fn)))))

;; async
(defn docker-exec [container command options]
  (P/let [cmd (if (string? command)
                ["sh" "-c" command]
                command)
          opts (merge {:AttachStdout true :AttachStderr true}
                      options
                      {:Cmd cmd})
          exec (.exec container (clj->js opts))
          stream (.start exec)
          stdout (atom [])
          stderr (atom [])
          stdout-stream (doto (stream/PassThrough.)
                          (.on "data" #(swap! stdout conj %)))
          stderr-stream (doto (stream/PassThrough.)
                          (.on "data" #(swap! stderr conj %)))
          _ (-> (.-modem container)
                (.demuxStream stream stdout-stream stderr-stream))
          data (wait-exec exec)
          stdout (S/join "" (map #(.toString % "utf8") @stdout))
          stderr (S/join "" (map #(.toString % "utf8") @stderr))
          result (assoc (->clj data) :Stdout stdout :Stderr stderr)]
    ;;(pprint result)
    result))

(defn obj->str [obj]
  (js/JSON.stringify (clj->js obj)))

(defn log-regex [regexes]
  (let [regexes (if (empty? regexes) [nil] regexes)
        regexes (map-indexed #(str "(?<c" %1 ">" (or %2 "^\b$") ")") regexes)]
    (js/RegExp. (str "^(20[0-9-]+T[0-9:.]+Z)\\s+.*("
                     (S/join "|" regexes) ")") "m")))

(defn log-regex-match [re s]
  (when-let [match (.match s re)]
    (let [ts (second match)
          cindex (-> match
                     .-groups
                     js/Object.entries
                     (->> (filter second)) ;; matching groups
                     first                 ;; first matching group
                     first                 ;; just the group name
                     (.substr 1)           ;; trim the prefix
                     js/parseInt)]         ;; numeric
      {:ts ts :cindex cindex})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI (ink/reagent) and Logging

;; {:settings   CONFIG-SETTINGS
;;  :services   {S-NAME {C-IDX {:id C-ID
;;                              :log-stream LOG-STREAM
;;                              :log-lines COUNT-OF-LOG-LINES
;;                              :log-regex COMBINED-LOG-REGEX
;;                              :checks [{:id CHEKCK-ID
;;                                        :done? DONE?
;;                                        :regex CHECK-REGEX-STR}
;;                                       {:id CHEKCK-ID
;;                                        :done? DONE?
;;                                        :cmd CMD
;;                                        :deps {S-NAME CHECK-ID}
;;                                        :exec EXEC-PROMISE
;;                                        :result EXEC-RESULT}]}}}
;;  :containers {C-ID INSPECT-DATA}
;;  :checks     SERVICE-CHECKS
;;  :log-file-stream LOG-FILE-STREAM}
(defonce ctx (reagent/atom {}))

;; https://github.com/vadimdemedes/ink
;; https://css-tricks.com/almanac/properties/f/flex-direction/
(def WIDTHS {:service 15
             :status 7
             :log-lines 5
             :check-id 10})

(defn service-color [status]
  (get {"created" "yellow"
        "running" "green"
        "exited"  "red"} status "grey"))

(defn log-color [service-status {:keys [done? result exec] :as check}]
  (cond
    (not= "running" service-status) "grey"
    done?                           "green"
    (= 0 (:ExitCode result))        "green"
    exec                            "yellow"
    :else                           "black"))

(defn service-dom []
  (let [bar (fn [k] [:> Box {:key k :width 1}
                     [:> Text {:color "grey"} "|"]])
        {:keys [settings services containers]} @ctx
        {:keys [check-len]} settings
        sorted-services (sort-by (fn [[s d]] [(count (get-in d [1 :checks])) s])
                                 services)]
    [:> Box {:flexDirection "column"}

     ;; Header Row
     [:> Box {:flexDirection "row"}
      (bar 1)
      [:> Box {:width (:service WIDTHS)}
       [:> Text { :color "blue"} "Service"]]
      (bar 2)
      [:> Box {:width (:status WIDTHS)}
       [:> Text { :color "blue"} "Status"]]
      (bar 3)
      [:> Box {:width (:log-lines WIDTHS)}
       [:> Text { :color "blue"} "Logs"]]
      (bar 4)
      [:> Box {:width (-> (:check-id WIDTHS) inc (* check-len) dec)}
       [:> Text { :color "blue"} "Checks"]]
      (bar 5)]

     ;; Data Rows
     (for [[service cstates] sorted-services
           [cidx {:keys [id log-lines checks] :as cstate}] cstates
           :let [{Name :Name {Status :Status} :State} (get containers id)
                 cname (str (name service) "_" cidx)
                 svc-color (service-color Status)]]
       [:> Box {:key (str service "/" cidx) :flexDirection "row"}
        (bar 1)
        [:> Box {:width (:service WIDTHS)}
         [:> Text {:color svc-color :wrap "truncate"} cname]]
        (bar 2)
        [:> Box {:width (:status WIDTHS)}
         [:> Text {:color svc-color :wrap "truncate"} (or Status "unknown")]]
        (bar 3)
        [:> Box {:width (:log-lines WIDTHS)}
         [:> Text {:wrap "truncate"} log-lines]]
        (bar 4)
        (for [check-idx (range check-len)
              :let [{:keys [id] :as check} (get checks check-idx)]]
          (list
            [:> Box {:key check-idx :width (:check-id WIDTHS)}
             (if check
               [:> Text {:color (log-color Status check) :wrap "truncate"} id]
               [:> Text {} " "])]
            (bar (str service "/" cidx "/" check-idx))))])]))

;; async
(defn event-logger [kind data]
  (let [{:keys [log-file-stream settings]} @ctx
        {:keys [show-events]} settings
        {:keys [service cidx finished]} data
        ;; TODO: should be timestamp from log match
        ts (or (:ts data) (.toISOString (js/Date.)))]
    (when (or (get show-events kind) (= #{:all} show-events))
      (println (str ts " " kind " "  data)))
    (when log-file-stream
      ;; Returns a promise
      (.write log-file-stream (str ts " " kind " " data "\n")))))

;; async
(defn event [kind data]
  (P/let [res (event-logger kind data)]
    ;; cases where we exit after event-logger completes
    (condp = kind
      :finished (when-not (-> @ctx :settings :keep-running)
                  (js/process.exit 0))

      :timeout (js/process.exit (:exit-code data))

      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

(defn deps-fulfilled? [services deps]
  (and
    (every? #(get services %) (keys deps))
    (every?
      identity
      (for [[service check-id] deps
            [cidx {:keys [checks]}] (get services service)]
        (every? :done? (filter #(= check-id (:id %)) checks))))))

(defn tick [docker]
  (js/setTimeout #(tick docker) TICK-PERIOD)
  (let [{:keys [settings services containers]} @ctx
        {:keys [keep-running finished start-time timeout]} settings
        cur-time (js/Date.)]
    (when (and finished
               (not (:fired? finished))  ;; only once
               (deps-fulfilled? services finished))
      (swap! ctx assoc-in [:settings :finished :fired?] true)
      (event :finished {:finished finished}))

    (when (and timeout
               (>= (- cur-time start-time) (* 1000 timeout)))
      (event :timeout {:exit-code (if (:fired? finished) 0 1)}))

    ;; For each service, container indx, and command check,
    ;; if it's ready to run (deps fulfilled) and no existing
    ;; command, launch a command check
    (doseq [[service svc-states] services
            [cidx {container-id :id checks :checks}] svc-states
            :let [{{:keys [Status]} :State} (get containers container-id)]
            :when (= "running" Status)
            [check-idx {:keys [cmd done? deps exec]}] (map-indexed vector checks)
            :when (and cmd
                       (not done?)
                       (not exec)
                       (deps-fulfilled? services deps))]
      (let [container (.getContainer docker container-id)
            exec (docker-exec container cmd {})]
        (event :start-exec {:service service :cidx cidx :cmd cmd})
        (swap! ctx assoc-in [:services service cidx :checks check-idx :exec] exec)
        (P/let [result-raw exec
                result (->clj result-raw)]
          (event :finish-exec {:service service :cidx cidx
                               :cmd cmd :exec-code (:ExitCode result)})
          (swap! ctx update-in [:services service cidx :checks check-idx]
                 #(-> %
                      (dissoc :exec)
                      (assoc :result result)
                      (assoc :done? (= 0 (:ExitCode result))))))))
    ))

(defn docker-log-handler [service cidx chnk]
  (let [{:keys [services]} @ctx
        {:keys [checks log-lines log-regex]} (get-in services [service cidx])
        log (.toString chnk "utf8")
        match (log-regex-match log-regex log)
        new-checks (if match
                     (let [{:keys [ts cindex]} match
                           check (get checks cindex)]
                       (if (not (:done? check))
                         (do
                           (event :log-match {:service service :cidx cidx
                                              :check check :ts ts})
                           (assoc-in checks [cindex :done?] true))
                         checks))
                     checks)]
    (swap! ctx update-in [:services service cidx]
           merge {:log-lines (inc log-lines)
                  :checks new-checks})))

(defn clear-checks [service cidx]
  (swap! ctx update-in [:services service cidx :checks]
         #(vec (map (fn [cs] (dissoc cs :done? :result :exec)) %))))

(defn ensure-service
  "Creates service/cidx definition if one doesn't exist that container
  the combined log regex and other default vaules set.
  Returns ctx (updated with service/cidx)"
  [service cidx]
  (swap! ctx #(if (get-in % [:services service cidx])
                %
                (let [checks (get-in % [:checks service])]
                  (assoc-in % [:services service cidx]
                            {:id nil
                             :log-lines 0
                             :log-regex (log-regex (map :regex checks))
                             :checks (vec checks)})))))

;; async: resolves to ctx
(defn init-container [service cidx container log-handler]
  (let [{:keys [services]} @ctx
        old-stream (get-in services [service cidx :log-stream])
        log-stream (doto (stream/PassThrough.)
                     (.on "data" log-handler))]
    (event :container-init {:service service :cidx cidx :id (.-id container)
                            :recreate-stream? (if old-stream true false)})
    (when old-stream
      (.destroy old-stream)
      (swap! ctx update-in [:services service cidx] merge {:log-stream nil
                                                           :log-lines 0})
      (clear-checks service cidx))
    (P/let [stream (.logs container #js {:follow true
                                         :stdout true
                                         :stderr true
                                         :timestamps true})]
      (-> (.-modem container)
          (.demuxStream stream log-stream log-stream))
      (.on stream "end" #(.end log-stream "!stop!"))
      (swap! ctx assoc-in [:services service cidx :log-stream] stream))))

;; async
(defn update-container [docker id start?]
  (P/catch
    (P/let [container (.getContainer docker id)
            inspect-raw (.inspect container)
            inspect (->clj inspect-raw)
            status (-> inspect :State :Status)
            service (keyword (-> inspect :Config :Labels :com.docker.compose.service))
            cidx (js/parseInt (-> inspect :Config :Labels :com.docker.compose.container-number))
            {:keys [services]} (ensure-service service cidx)]
      ;;(prn :updating :service service :cidx cidx
      ;;     :state (-> inspect :State :Status) :start? start? :id id)
      (swap! ctx #(-> %
                      (assoc-in [:containers id] inspect)
                      (assoc-in [:services service cidx :id] id)))
      (when (not= "running" status)
        (clear-checks service cidx))
      (if start?
        (init-container service cidx container
                        (partial docker-log-handler service cidx))
        (event :container-update {:service service :cidx cidx :id id
                                  :status status})))
    (fn [err]
      (if (.-statusCode err)
        (event :docker-error {:id id :status-code (.-statusCode err)})
        (do
          (event :error {:id id :error (->clj err)})
          (prn :error :id id :error (->clj err)))))))

;; async
(defn docker-event-handler [opts docker evt-buf]
  (let [{:keys [verbose-events]} opts
        evt (->clj (js/JSON.parse (.toString evt-buf "utf8")))
        status (:status evt)
        start? (= "start" status)]
    (when (not= "exec_" (.substr status 0 5))
      (if verbose-events
        (event :docker-event evt)
        (event :docker-event (select-keys evt [:id :status])))
      (update-container docker (:id evt) start?))))

(P/let [opts (parse-opts (or *command-line-args* (clj->js [])))
        {:keys [project show-events no-tui timeout]} opts
        show-events (when show-events
                      (set (map #(keyword (second (re-find #":*(.+)" %)))
                                (S/split show-events #"[, ]"))))
        timeout (when-let [timeout (:timeout opts)] (js/parseInt timeout))
        log-file-stream (when-let [log-file (:log-file opts)]
                          (.createWriteStream fs log-file #js {:flags "w"}))
        checks-bufs (P/all (for [f (:checks-file opts)] (slurp-buf f)))
        checks-cfgs (map #(->clj (.load yaml %)) checks-bufs)
        checks-cfg (reduce (fn [cfg {:keys [settings checks]}]
                             (-> cfg
                                 (update :settings merge settings)
                                 (update :checks merge checks)))
                           {:settings {} :checks {}}
                           checks-cfgs)
        check-len (apply max (map count (vals (:checks checks-cfg))))
        settings (merge (:settings checks-cfg)
                        opts
                        {:show-events show-events
                         :timeout timeout
                         :start-time (js/Date.)
                         :check-len check-len})
        proj-filter {:label [(str "com.docker.compose.project=" project)]}
        container-filter (obj->str proj-filter)
        event-filter (obj->str (merge proj-filter {:type ["container"]}))
        docker (Docker.)
        containers (.listContainers docker (clj->js {:all true
                                                     :filters container-filter}))
        event-obj (.getEvents docker (clj->js {:filters event-filter}))]

  (.on event-obj "data" (partial docker-event-handler opts docker))

  (reset! ctx {:settings settings
               :services {}
               :containers {}
               :checks (:checks checks-cfg)
               :log-file-stream log-file-stream})
  ;;(pprint @ctx)
  ;;(prn :show-events show-events)
  ;;(js/process.exit 1)

  (event :monitor-start {:settings settings})

  (doseq [container containers]
    (update-container docker (.-Id container) true))

  (when-not no-tui
    (render (reagent/as-element [service-dom])))

  (tick docker))
