(ns com.gfredericks.lib-7607
  "Trying to write nice generic search controllers."
  (:require [com.gfredericks.lib-7607.managers :refer [job done? run report id]]))

(defn ^:private update [m k f & args] (apply update-in m [k] f args))

;; Okay so here's my new plan.
;;   1) Finish the prime factoring tests and get them to pass
;;   2) Move all the search manager code to isomorphism.search.management or so
;;   3) Refactor it to multimethods, maps with a :type key, and a decent type
;;      hierarchy. Move as much of the non-process functionality in there as we
;;      can.



;; TODO:
;; - count total results found?
;; - result buckets
;; - jobs shouldn't have to be collections of things
;; - track how long threads have been running?
;;   I guess that doesn't apply very well when e.g. there are
;;   many more threads than cores.

;; It's not awesome yet but it works!
;;
;; What are the abstractions/protocols that we want?
;;
;; The job/result management stuff should be definable
;; statelessly. A protocols and some defrecords would
;; be an interesting approach
;;
;; Styles of searches we want:
;;   - Check random instances of things
;;   - Hill climbing
;;   - Huge search tree
;;   - Infinite search "tree"


(declare info)

(defmethod print-method ::search-state
  [a sw]
  (doto sw
    (.write "#<Searcher ")
    (.write (pr-str (info @a)))
    (.write ">")))

(def default-opts
  {:thread-count 4
   :result-size 10})

;;
;; Start worker code
;;

(defmacro me [] `(Thread/currentThread))

(defn current-job
  [state]
  (get-in state [:thread-jobs (me)]))

(defn get-job
  "Returns nil if there are none available."
  [state-atom]
  {:pre [(not (current-job @state-atom))]}
  ;; this could be more optimal about pauses
  (swap! state-atom
         (fn [{:keys [search-manager] :as m}]
           (if-let [[job search-manager'] (job search-manager)]
             (-> m
                 (assoc :search-manager search-manager')
                 (assoc-in [:thread-jobs (me)] job))
             m)))
  (or (current-job @state-atom)
      (if (-> @state-atom :search-manager done?)
        nil
        (do (Thread/sleep 100)
            (recur state-atom)))))

(defn crash
  [state throwable]
  (let [me' (me)]
    (swap! state
           #(-> %
                (update :threads dissoc me')
                (update :thread-jobs dissoc me')
                (update :crashed-threads conj
                        {:throwable throwable
                         :job (get-in % [:thread-jobs me'])})))))

(defn do-job
  [state-atom job]
  (let [x (run job)]
    (swap! state-atom
           (fn [state]
             (-> state
                 (update :search-manager report (id job) x)
                 (update :thread-jobs dissoc (me)))))))

(defn should-work?
  [state]
  (and (get-in state [:threads (me)])
       (not (done? (:search-manager state)))))

(defn worker
  [state-atom]
  (while (should-work? @state-atom)
    (try
      (when-let [job (get-job state-atom)]
        (do-job state-atom job))
      (catch Throwable t
        (crash state-atom t)
        (throw t))))
  ;; pausing or done
  (swap! state-atom update :threads dissoc (me)))

;;
;; End worker code
;;


(defn add-thread
  [state]
  (let [t (Thread. (bound-fn [] (worker state)))]
    (swap! state #(update % :threads assoc t true))
    (.start t)))

(defn resume
  [state]
  (letfn [(low? [{:keys [thread-count threads]}]
            (< (count threads) thread-count))]
    (while (low? @state)
      (add-thread state))))

(defn searcher
  [search-manager opts]
  (let [{:keys [result-size thread-count]} (merge default-opts opts)

        state (atom {:thread-count thread-count
                     :threads {}
                     :thread-jobs {}
                     :search-manager search-manager})]
    (alter-meta! state assoc :type ::search-state)
    (resume state)
    state))

(defn info
  [{:keys [search-manager crashed-threads total domain threads results] :as m}]
  (cond-> {:running-threads (count threads)
           :done? (done? search-manager)}
          (seq crashed-threads)
          (assoc :crashed-thread-count (count crashed-threads))))

(defn pause
  [state]
  (swap! state update :threads
         (fn [m]
           (zipmap (keys m) (repeat false))))
  :ok)

(defn set-thread-count
  [state-atom n]
  (swap! state-atom assoc :thread-count n)
  (let [threads-running (-> @state-atom :threads count)]
    (cond (< threads-running n)
          (resume state-atom)

          (> threads-running n)
          (swap! state-atom
                 (fn [m]
                   (let [threads-to-kill (->> (:threads m)
                                              (filter val)
                                              (map key)
                                              (drop n))]
                     (assoc m :threads
                            (reduce #(assoc %1 %2 false)
                                    (:threads m)
                                    threads-to-kill))))))))
