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
      (update :report-count inc)
      (cond-> (not (done? m))
              (-report job-id result))))


(defprotocol IJob ;; do we really?
  ;; The only use I know of for these is for idempotence...
  (id [_] "Returns a UUID")
  ;; or just invoke from IFn??
  (run [_]))

(defmulti add-result
  "Multimethod to add a result to a collection."
  (fn [coll res] (type coll)))

(defmethod add-result clojure.lang.IPersistentCollection
  [coll x]
  (conj coll x))

(defn ^:private search-manager
  [type & kvs]
  (assoc (apply hash-map kvs)
    :type type
    :report-count 0))

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

;;
;; A ::delegator is a search-manager all of whose jobs come from a
;; nested search-manager. The main thing that distinguishes delegators
;; is what they do when the nested search-manager is done.
;;
;; Delegators are expected to define a method for finished-delegatee
;;

(defmulti finished-delegatee
  "Docstring."
  first-arg-type)

(defmethod done? ::delegator
  [me]
  (not (contains? me :nested-search-manager)))

(defmethod -job ::delegator
  [{sm :nested-search-manager, :as me}]
  (when-not (done? me)
    (if-let [[job sm'] (job sm)]
      [job (assoc me :nested-search-manager sm')])))

(defmethod -report ::delegator
  [{:keys [nested-search-manager] :as me} job-id result]
  (if (contains? (:jobs nested-search-manager) job-id)
    (let [sm (report nested-search-manager job-id result)]
      (if (done? sm)
        (finished-delegatee me sm)
        (assoc me :nested-search-manager sm)))
    ;; in this case it is presumably from an older :nested-search-manager
    me))

;;
;; ::iterator
;;


(derives ::iterator ::search-manager ::delegator)

(defmethod finished-delegatee ::iterator
  [{:keys [func] :as me} sm]
  (let [sm-or-result (func sm)]
    (if (search-manager? sm-or-result)
      (assoc me :nested-search-manager sm-or-result)
      (-> me
          (dissoc :nested-search-manager)
          (assoc :results sm-or-result)))))

(defmethod results ::iterator
  [me]
  (if (done? me)
    (:results me)))

(defn iterator-search-manager
  [sm func]
  (search-manager ::iterator
                  :nested-search-manager sm
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

;;
;; ::hill-climbing
;;

(defmethod add-result ::best-result-keeper
  [coll x]
  (if (empty? coll)
    (conj coll x)
    (let [[[data score]] coll
          [data' score'] x]
      (if (neg? (compare score score'))
        (assoc coll 0 x)
        coll))))

;; TODO: Serializability!
(def best-result-keeper
  (with-meta
    []
    {:type ::best-result-keeper}))

(defn hill-climbing-search-manager
  [start neighbors scorer]
  (letfn [(make-lazy-sm [[data score :as x]]
            (assoc
                (lazy-seq-search-manager
                 (neighbors data)
                 (juxt identity scorer)
                 best-result-keeper)
              :x x))]
    (iterator-search-manager
     (make-lazy-sm [start (scorer start)])
     (fn [nested]
       (let [[data score] (:x nested)
             [[data' score']] (results nested)]
         (if (> score' score)
           (make-lazy-sm [data' score'])
           [data score]))))))
