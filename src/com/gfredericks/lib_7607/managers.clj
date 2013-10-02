(ns com.gfredericks.lib-7607.managers
  (:import java.util.UUID))

;;
;; ## Prelude
;;

(defn ^:private update [m k f & args] (apply update-in m [k] f args))
(defn derives
  "Derive a type from several parents at once (in the global hierarchy)."
  [tag & parents]
  (doseq [parent parents] (derive tag parent)))

(defn first-arg-type [x & more] (:type x))

(defmulti -job
  "Returns a [job new-search-manager] if there is a job available."
  first-arg-type)

(defmulti -report
  "Returns a [job new-search-manager]"
  first-arg-type)

(defmulti done?
  first-arg-type)

(defmulti results
  first-arg-type)

(defn job
  [m]
  (when-not (done? m)
    (if-let [[job m'] (-job m)]
      (let [id (or (:id job) (UUID/randomUUID))
            job (assoc job :id id)]
        [job (update m' :jobs assoc id job)]))))

(defn report
  [m job-id result]
  {:pre [(contains? (:jobs m) job-id)]}
  (-> m
      (update :jobs dissoc job-id)
      (cond-> (not (done? m))
              (-report job-id result))))


(defprotocol IJob ;; do we really?
  ;; The only use I know of for these is for idempotence...
  (id [_] "Returns a UUID")
  ;; or just invoke from IFn??
  (run [_]))

(defprotocol IResultTracker
  (add-result [_ x] "Submits a result to maybe be added to the collection."))

(extend-protocol IResultTracker
  clojure.lang.IPersistentCollection
  (add-result [me x] (conj me x)))


(defn ^:private search-manager
  [type & kvs]
  (assoc (apply hash-map kvs) :type type))

(defn search-manager?
  [x]
  (and (map? x)
       (isa? (:type x) ::search-manager)))



(defrecord RandomGuessJob [id data checker]
  IJob
  (id [_] id)
  (run [_] (checker data)))

(defmethod results ::first-result-quitter [m] (:result m))
(defmethod done? ::first-result-quitter [m] (contains? m :result))
(defmethod -report ::first-result-quitter
  [m job-id result]
  (cond-> m
          (and result (not (done? m)))
          (assoc :result result)))

(derives ::random-guess ::search-manager ::first-result-quitter)

(defmethod -job ::random-guess
  [{:keys [generator checker] :as me}]
  (when-not (done? me)
    [(RandomGuessJob. (UUID/randomUUID) (generator) checker)
     me]))

;; TODO: better docstring?
(defn random-guess-search-manager
  "Creates a search manager that repeatedly generates random instances
   until it finds one that satisfies. Generator should be a function
   that generates random instances of the problem (quickly), and
   checker should be the workhorse that returns non-nil if it's found
   something. Quits as soon as if finds a single result."
  [generator checker]
  (search-manager ::random-guess
                  :generator generator
                  :checker checker))

(defrecord SimpleJob [id x func]
  IJob
  (id [_] id)
  (run [_] (func x)))

(defn make-simple-job
  [sm input func]
  (let [job (SimpleJob. nil input func)]
    [job sm]))

(derive ::lazy-seq ::search-manager)

(defmethod -job ::lazy-seq
  [{:keys [coll func] :as sm}]
  (if-let [[x & xs] (seq coll)]
    (-> sm
        (assoc :coll xs)
        (make-simple-job x func))))

(defmethod done? ::lazy-seq
  [{:keys [coll jobs]}]
  (and (empty? coll) (empty? jobs)))

(defmethod -report ::lazy-seq
  [me job-id result]
  (update me :results add-result result))

(defmethod results ::lazy-seq [me] (:results me))

(defn lazy-seq-search-manager
  ([coll func]
     (lazy-seq-search-manager coll func []))
  ([coll func result-holder]
     (search-manager ::lazy-seq
                     :coll coll
                     :func func
                     :results result-holder)))

(derive ::lazy-seq-first-result ::lazy-seq)

(defmethod done? ::lazy-seq-first-result
  [{:keys [coll jobs results]}]
  (or results (and (empty? coll) (empty? jobs))))

(defmethod -report ::lazy-seq-first-result
  [{:keys [results] :as me} job-id result]
  (cond-> me
          (and (nil? results) result)
          (assoc :results result)))

(defn lazy-seq-first-result-manager
  "A SearchManager that maps over a lazy seq and short-circuits with the first
   non-nil result."
  [coll func]
  (search-manager ::lazy-seq-first-result
                  :coll coll
                  :func func))

(defmethod done? ::iterator
  [me]
  (not (contains? me :sm)))
(defmethod -job ::iterator
  [{sm :sm, :as me}]
  (when-not (done? me)
    (if-let [[job sm'] (job sm)]
      [job (assoc me :sm sm')])))
(defmethod -report ::iterator
  [{:keys [sm func], :as me} job-id result]
  (if (contains? (:jobs sm) job-id)
    (let [sm' (report sm job-id result)]
      (if (done? sm')
        (let [sm-or-result (func (results sm'))]
          (if (search-manager? sm-or-result)
            (assoc me :sm sm-or-result)
            (-> me
                (dissoc :sm)
                (assoc :results sm-or-result))))
        (assoc me :sm sm')))
    ;; in this case it is presumably from an older inner search-manager
    me))
(defmethod results ::iterator
  [me]
  (if (done? me)
    (:results me)))

(defn iterator-search-manager
  [sm func]
  (search-manager ::iterator
                  :sm sm
                  :func func))

(derive ::map-quick-reducing ::search-manager)

(defmethod done? ::map-quick-reducing
  [{:keys [jobs coll]}]
  (and (empty? jobs) (empty? coll)))

(defmethod -job ::map-quick-reducing
  [{:keys [coll map] :as me}]
  (if-let [[x & xs] coll]
    (-> me
        (assoc :coll xs)
        (make-simple-job x map))))

(defmethod results ::map-quick-reducing [m] (:x m))

(defmethod -report ::map-quick-reducing
  [{:keys [reduce] :as me} job-id result]
  (update me :x reduce result))

(defn map-quick-reducing-search-manager
  "Assumes the map function is costly and the reduce function is quick."
  [map-f reduce-f x coll]
  (search-manager ::map-quick-reducing
                  :map map-f
                  :reduce reduce-f
                  :x x
                  :coll coll))

(comment
  (defrecord HillClimbingSearchManager [generator edges edge-taker scorer result-holder]
    ISearchManager
    ;; Man alive this feels messy...
    ;;
    ;; How is this not mixed up in the result holder?
    (job [me]
      (if-let [[e & es] (-> me :current-edges seq)]
        (let [new-job-id (UUID/randomUUID)]
          [(SimpleJob. new-job-id e (fn []
                                      (let [neighbor (-> me
                                                         (:current)
                                                         (edge-taker e))]
                                        [(scorer neighbor) neighbor])))
           (-> me
               (assoc :current-edges es)
               (update :current-jobs conj new-job-id))])
        (if (empty? (:current-jobs me))
          (let [current (generator)
                current-edges (edges current)]
            (-> me
                (assoc :current current,
                       :current-score (scorer current)
                       :current-edges current-edges,
                       :best-result nil
                       :current-jobs #{})
                (job)))
          (throw (ex-info "I don't know how to do this yet" {:me me})))))
    ;; Don't we want to wait till all the results get back and then
    ;; pick the best one?
    (report [me job-id result]
      (let [[score neighbor] result
            [score' neighbor'] (:best-result me)]
        (cond-> me
                (or (nil? score') (> score score'))
                (assoc :best-result result)

                (= #{job-id} (:current-jobs me))
                ;; this is where we figure out whether to generate a new
                ;; instance or not.
                ;;
                ;; Could we code this to do just one instance and have a
                ;; higher-order search-manager that does repetition?
                ;;
                ;; On that note, could THIS search-manager be an HOSM
                ;; based on the single step??????????!!
                ))
      ))

  (defn hill-climbing-search-manager
    [generator neighbors scorer result-holder]
    (HillClimbingSearchManager. generator neighbors scorer result-holder)))
