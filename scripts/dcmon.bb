#!/usr/bin/env nbb
(ns dcmon
  (:require [clojure.string :as S]
            [clojure.pprint :refer [pprint]]
            [promesa.core :as P]
            [cljs-bean.core :refer [->clj]]
            [reagent.core :as reagent]
            ["fs" :as fs]
            ["child_process" :as child-process]
            ["util" :refer [promisify]]
            ["stream" :as stream]
            ["neodoc" :as neodoc]
            ["ink" :refer [render Text Box]]
            ["js-yaml" :as yaml]
            ["dockerode$default" :as Docker]))

(def usage "
Usage:
  dcmon [options] <CHECKS-FILE>

Options:
  --verbose              Verbose output [env: VERBOSE]
  -p --project PROJECT   Compose project name [env: PROJECT]
  --keep-running         Don't exit when done setting is reached
  --log-file LOG-FILE    Output events to LOG-FILE
  --show-events          Output events to the screen
  --only-events          Only output events to screen (no TUI)
  --timeout TIMEOUT      Timeout after TIMEOUT seconds
                         (exit with error if not in finished state)
")

(def WAIT-EXEC-SLEEP 200)
(def TICK-PERIOD 1000 #_200)

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

(def exec (promisify (.-exec child-process)))
(def slurp-buf (promisify (.-readFile fs)))

;; async
(defn compose-project []
  (P/let [res (exec "docker-compose --verbose logs --tail=0")
          stderr (:stderr (->clj res))
          project (-> (re-seq #"'com.docker.compose.project=([^']*)'" stderr)
                      first
                      second)]
    project))

;; async
(defn compose-config []
  (P/let [res (exec "docker-compose config")]
    (js->clj (.load yaml (:stdout (->clj res))) :keywordize-keys true)))

;; async
(defn wait-exec [exec]
  (P/create
    (fn [resolve reject]
      (let [check-fn (atom nil)
            exec-cb (fn [err data]
                      ;;(prn :exec-cb :err err
                      ;;     :Running (.-Running data)
                      ;;     :ExitCode (.-ExitCode data)
                      ;;     :Pid (.-Pid data))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI (ink/reagent) and Logging

;; {:services   {S-NAME {C-IDX {:id C-ID
;;                              :log-stream LOG-STREAM
;;                              :log-lines COUNT-OF-LOG-LINES
;;                              :checks [{:id CHEKCK-ID
;;                                        :done? DONE?
;;                                        :regex REGEX-MATCH}
;;                                       {:id CHEKCK-ID
;;                                        :done? DONE?
;;                                        :cmd CMD
;;                                        :deps {S-NAME CHECK-ID}
;;                                        :exec EXEC-PROMISE
;;                                        :result EXEC-RESULT}]}}}
;;  :containers {C-ID INSPECT-DATA}}
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
      (bar 4)]

     ;; Data Rows
     (for [[service cstates] sorted-services
           [cidx {:keys [id log-lines checks] :as cstate}] cstates
           :let [;;_ (prn :cidx cidx :cstate cstate)
                 {Name :Name {Status :Status} :State} (get containers id)
                 svc-color (service-color Status)]]
       [:> Box {:key (str service "/" cidx) :flexDirection "row"}
        (bar 1)
        [:> Box {:width (:service WIDTHS)}
         [:> Text {:color svc-color :wrap "truncate"} (name service)]]
        (bar 2)
        [:> Box {:width (:status WIDTHS)}
         [:> Text {:color svc-color} (or Status "unknown")]]
        (bar 3)
        [:> Box {:width (:log-lines WIDTHS)}
         [:> Text {} log-lines]]
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
  (let [{:keys [log-stream settings]} @ctx
        {:keys [show-events?]} settings
        {:keys [service cidx finished]} data
        ;; TODO: should be timestamp from log match
        ts (.toISOString (js/Date.))]
    (when show-events?
      (println (str ts " " kind " "  data)))
    (when log-stream
      ;; Returns a promise
      (.write log-stream (str ts " " kind " " data "\n")))))

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
  (every?
    identity
    (for [[service check-id] deps
          [cidx {:keys [checks]}] (get services service)]
      (every? :done? (filter #(= check-id (:id %)) checks)))))

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
                      (assoc :done? (= 0 (:ExitCode result))))))))))

(defn docker-log-handler [service cidx chnk]
  (let [{:keys [services]} @ctx
        {:keys [checks log-lines]} (get-in services [service cidx])
        log (.toString chnk "utf8")
        ;;_ (prn :log log)
        new-checks (doall
                     (for [{:keys [id regex] :as check} checks]
                       (if (not regex)
                         check
                         (if-let [match (.match log regex)]
                           (let [check (assoc check :done? true)]
                             (event :log-match {:service service :cidx cidx
                                                :check check :match (js->clj match)})
                             check)
                           check))))]
    ;;(prn :log :servie service :cidx cidx log)
    (swap! ctx update-in [:services service cidx]
           merge {:log-lines (inc log-lines)
                  :checks (vec new-checks)})))

(defn clear-checks [service cidx]
  (swap! ctx update-in [:services service cidx :checks] 
         #(vec (map (fn [cs] (dissoc cs :done? :result :exec)) %))))

;; async
(defn init-container [service cidx container log-handler]
  (let [{:keys [services]} @ctx
        old-stream (get-in services [service cidx :log-stream])
        log-stream (doto (stream/PassThrough.)
                     (.on "data" log-handler))]
    (when old-stream
      ;;(prn :DESTROY-STREAM :id (.-id container) :service service :cidx cidx)
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
      ;;(prn :CREATE-LOG-STREAM :id (.-id container) :service service :cidx cidx)
      (swap! ctx assoc-in [:services service cidx :log-stream] stream)
      true)))

;; async
(defn update-container [docker id start?]
  (let [{:keys [services containers]} @ctx]
    (P/catch
      (P/let [container (.getContainer docker id)
              inspect-raw (.inspect container)
              inspect (->clj inspect-raw)
              status (-> inspect :State :Status)
              service (keyword (-> inspect :Config :Labels :com.docker.compose.service))
              cidx (js/parseInt (-> inspect :Config :Labels :com.docker.compose.container-number))]
        (when (contains? services service)
          ;;(prn :updating :service service :cidx cidx
          ;;     :state (-> inspect :State :Status) :start? start? :id id)
          (swap! ctx #(-> %
                          (assoc-in [:containers id] inspect)
                          (assoc-in [:services service cidx :id] id)))
          (when (not= "running" status)
            (clear-checks service cidx))
          (when start?
            (event :container-init {:service service :cidx cidx
                                    :status status :id id})
            (init-container service cidx container
                            (partial docker-log-handler service cidx)))))
      (fn [err]
        ;;(prn :statuscode (.-statusCode err))
        (if (= 404 (.-statusCode err))
          (swap! ctx assoc-in [:containers id :State :Status] "unknown")
          (prn :error :id id :error (->clj err)))))))

;; async
(defn dc-event-handler [docker evt-buf]
  (let [evt (->clj (js/JSON.parse (.toString evt-buf "utf8")))
        start? (= "start" (:status evt))]
    ;;(prn :event :evt evt :start? start?)
    (update-container docker (:id evt) start?)))

(defn init-services
  "Add checks to each service (defined in compose config)
  while transforming regex strings into real regex objects
  and adding default values."
  [services checks]
  (into {}
        (for [[service {:keys [scale]}] services]
          [service
           (into {}
                 (for [cidx (range 1 (inc (or scale 1)))]
                   [cidx
                    (let [schecks (for [check (get checks service)
                                        :let [{:keys [regex] :as check} check]]
                                    (if regex
                                      (assoc check :regex (js/RegExp. regex "mg"))
                                      check))]
                      {:id nil
                       :log-lines 0
                       :checks (vec schecks)})]))])))


(P/let [opts (parse-opts (or *command-line-args* (clj->js [])))
        checks-buf (slurp-buf (:CHECKS-FILE opts))
        show-events? (or (:show-events opts) (:only-events opts))
        timeout (when-let [timeout (:timeout opts)] (js/parseInt timeout))
        log-stream (when-let [log-file (:log-file opts)]
                    (.createWriteStream fs log-file #js {:flags "w"}))
        checks-cfg (->clj (.load yaml checks-buf))
        check-len (apply max (map count (vals (:checks checks-cfg))))
        settings (merge (:settings checks-cfg)
                        opts
                        {:check-len check-len
                         :show-events? show-events?
                         :start-time (js/Date.)
                         :timeout timeout})
        config (compose-config)
        project (or (:project opts) (compose-project))
        proj-filter {:label [(str "com.docker.compose.project=" project)]}
        container-filter (obj->str proj-filter)
        event-filter (obj->str (merge proj-filter {:type ["container"]}))
        docker (Docker.)
        containers (.listContainers docker (clj->js {:all true
                                                     :filters container-filter}))
        event-obj (.getEvents docker (clj->js {:filters event-filter}))]

  (.on event-obj "data" (partial dc-event-handler docker))
  ;;(prn :opts opts)
  ;;(prn :project project)
  ;;(prn :services services)
  ;;(pprint services)
  ;;(prn :containers (map :Id (->clj containers)))
  ;;(prn :check-len check-len)

  (reset! ctx {:settings settings
               :log-stream log-stream
               :services (init-services (:services config)
                                        (:checks checks-cfg))
               :containers {}})
  ;;(pprint @ctx)

  (event :monitor-start {:settings settings})

  (doseq [container containers]
    (update-container docker (.-Id container) true))

  (when-not (:only-events opts)
    (render (reagent/as-element [service-dom])))

  (tick docker))
