#!/usr/bin/env nbb
(ns dcmon.core
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
            #_["dockerode$default" :as Docker]))
;; workaround (shadow-cljs issue?)
(def Docker (js/require "dockerode"))

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

(set! *warn-on-infer* false)

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

(defn wait-exec
  "[Async] Wait for docker exec to complete and when complete resolve
  to result of inspecting successful exec or reject with exec error."
  [exec]
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

(defn docker-exec
  "[Async] Exec a command in a container and wait for it to complete
  (using wait-exec). Resolves to exec data with additional :Stdout and
  and :Stderr keys."
  [container command options]
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

(defn log-regex
  "Construct a combined log regex using a sequence of regex strings.
  The combined regex starts with a ISO data string followed by
  anything followed by an alternation of the regex strings. Each
  sub-regex string is is given a named group 'cIDX' where IDX is the
  numeric index into the original regex-strs sequence."
  [regex-strs]
  (let [regex-strs (if (empty? regex-strs) [nil] regex-strs)
        regex-strs (map-indexed #(str "(?<c" %1 ">" (or %2 "^\b$") ")")
                                regex-strs)]
    (js/RegExp. (str "^(20[0-9-]+T[0-9:.]+Z)\\s+.*("
                     (S/join "|" regex-strs) ")") "m")))

(defn log-regex-match
  "Takes a log-regex (generated using log-regex) and a string to match
  against and returns '{:ts ts :cindex cindex}' where 'ts' is the date
  string and 'cindex' is the the index of the sub-regex string that
  matches."
  [lre s]
  (when-let [match (.match s lre)]
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

(defn event-logger
  "[Async] Takes an event kind and event data and conditional prints
  it out (depending on show event configuration) and also write it to
  the log stream (if set). If the event data contains a ':ts' key then
  this is used as the timestamp otherwise the current time is used."
  [kind data]
  (let [{:keys [log-file-stream settings]} @ctx
        {:keys [show-events]} settings
        {:keys [service cidx finished ts]} data
        ;; TODO: should be timestamp from log match
        ts (.toISOString (if ts (js/Date. ts) (js/Date.)))]
    (when (or (get show-events kind) (= #{:all} show-events))
      (println (str ts " " kind " "  data)))
    (when log-file-stream
      ;; Returns a promise
      (.write log-file-stream (str ts " " kind " " data "\n")))))

(defn event
  "[Async] Handles an event. First the event kind and data is logged
  and then if the kind of event is an completion event then the
  process exits with an error code depending on the reason for the
  exit and the whether the finished condition for the checks has been
  reached. "
  [kind data]
  (P/let [res (event-logger kind data)]
    ;; cases where we exit after event-logger completes
    (condp = kind
      :finish (when-not (-> @ctx :settings :keep-running)
                (js/process.exit 0))

      :timeout (js/process.exit (:exit-code data))

      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

(defn deps-fulfilled?
  "Checks a deps map against the services map. For each service key in
  the deps map, it checks to see if the check ID (the value) has been
  completed successfully in the services map. If all deps have been
  completed successfully then return true, otherwise false."
  [services deps]
  (and
    (every? #(get services %) (keys deps))
    (every?
      identity
      (for [[service check-id] deps
            [cidx {:keys [checks]}] (get services service)]
        (every? :done? (filter #(= check-id (:id %)) checks))))))

(defn tick
  "Periodic worker. Checks for the finished and timeout conditions and
  exits if so. Also launches command checks for any command checks
  that have all their deps fullfilled, that are themselves
  unfullfilled, and that don't already have a command check running."
  [docker]
  (js/setTimeout #(tick docker) TICK-PERIOD)
  (let [{:keys [settings services containers]} @ctx
        {:keys [keep-running finished start-time timeout]} settings
        cur-time (js/Date.)]
    (when (and finished
               (not (:fired? finished))  ;; only once
               (deps-fulfilled? services finished))
      (swap! ctx assoc-in [:settings :finished :fired?] true)
      (event :finish {:finished finished}))

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
            [check-idx {:keys [cmd done? deps exec] :as check}] (map-indexed vector checks)
            :when (and cmd
                       (not done?)
                       (not exec)
                       (deps-fulfilled? services deps))]
      (let [container (.getContainer docker container-id)
            exec (docker-exec container cmd {})]
        (event :start-exec {:service service :cidx cidx :cmd cmd})
        (swap! ctx assoc-in [:services service cidx :checks check-idx :exec] exec)
        (P/let [result-raw exec
                result (->clj result-raw)
                exit-code (:ExitCode result)
                check (-> check
                          (dissoc :exec)
                          (assoc :result result)
                          (assoc :done? (= 0 exit-code)))  ]
          (event :finish-exec {:service service :cidx cidx
                               :check check})
          (when (= 0 exit-code)
            (event :check-done {:service service :cidx cidx
                                :check (dissoc check :result)}))
          (swap! ctx assoc-in [:services service cidx :checks check-idx] check))))))

(defn docker-log-handler
  "Handle docker container log messages. Count the number of received
  logs for this container. If the log message matches a log check then
  mark it done."
  [service cidx chnk]
  (let [{:keys [services containers]} @ctx
        {:keys [id checks log-lines log-regex]} (get-in services [service cidx])
        log (.toString chnk "utf8")
        match (log-regex-match log-regex log)
        new-checks (if match
                     (let [{:keys [ts cindex]} match
                           check (get checks cindex)
                           {{:keys [Status]} :State} (get containers id)]
                       (when (= "running" Status)
                         (event :log-match {:service service :cidx cidx
                                            :check check :ts ts}))
                       (if (and (not (:done? check)) (= "running" Status))
                         (do
                           (event :check-done {:service service :cidx cidx :ts ts
                                               :check (assoc check :done? true)})
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
  the combined log regex and other default vaules set.  Returns ctx
  (updated with service/cidx)"
  [service cidx]
  (swap! ctx #(if (get-in % [:services service cidx])
                %
                (let [checks (get-in % [:checks service])]
                  (assoc-in % [:services service cidx]
                            {:id nil
                             :log-lines 0
                             :log-regex (log-regex (map :regex checks))
                             :checks (vec checks)})))))

(defn init-container
  "[Async] Configure log monitoring for a newly detected or newly
  running container. Resolves to the updated state of ctx."
  [service cidx container status log-handler]
  (let [{:keys [services]} @ctx
        old-stream (get-in services [service cidx :log-stream])
        log-stream (doto (stream/PassThrough.)
                     (.on "data" log-handler))]
    (when (= "running" status)
      (event :container-running {:service service :cidx cidx :id (.-id container)
                                 :recreate-stream? (if old-stream true false)}))
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

(defn update-container
  "[Async] A container we are tracking has changed state. Inspect the
  container and update the ':services' and ':containers' keys in ctx.
  If the container is not running the clear the check results for that
  service and container index. If the container is new or newly
  detected, then initialize its state."
  [docker id init?]
  (P/catch
    (P/let [container (.getContainer docker id)
            inspect-raw (.inspect container)
            inspect (->clj inspect-raw)
            status (-> inspect :State :Status)
            service (keyword (-> inspect :Config :Labels :com.docker.compose.service))
            cidx (js/parseInt (-> inspect :Config :Labels :com.docker.compose.container-number))
            {:keys [services]} (ensure-service service cidx)]
      (swap! ctx #(-> %
                      (assoc-in [:containers id] inspect)
                      (assoc-in [:services service cidx :id] id)))
      (when (not= "running" status)
        (clear-checks service cidx))
      (if init?
        (init-container service cidx container status
                        (partial docker-log-handler service cidx))
        (event :container-update {:service service :cidx cidx :id id
                                  :status status})))
    (fn [err]
      (if (.-statusCode err)
        (event :docker-error {:id id :status-code (.-statusCode err)})
        (do
          (event :error {:id id :error (->clj err)})
          (prn :error :id id :error (->clj err)))))))

(defn docker-event-handler
  "[Async] Log and handle docker container events."
  [opts docker evt-buf]
  (let [{:keys [verbose-events]} opts
        evt-str (.toString evt-buf "utf8")
        evt (try (->clj (js/JSON.parse evt-str))
                 (catch :default e nil
                   (event :docker-error {:invalid-event evt-str})
                   nil))
        status (:status evt)
        init? (= "start" status)]
    (when (not= "exec_" (.substr status 0 5))
      (if verbose-events
        (event :docker-event evt)
        (event :docker-event (select-keys evt [:id :status])))
      (update-container docker (:id evt) init?))))

(defn -main [& argv]
  (P/let [opts (parse-opts (or argv #js []))
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

    (.on js/process "SIGINT" #(js/process.exit 130))

    (event :start {:settings settings})

    (doseq [container containers]
      (update-container docker (.-Id container) true))

    (when-not no-tui
      (render (reagent/as-element [service-dom])))

    (tick docker)))
