(ns com.gfredericks.lib-7607.managers
  (:require [com.gfredericks.lib-7607.results :refer [add-result best-result-keeper single-result]]
            [com.gfredericks.lib-7607.util
             :refer [update derives type-kw-of-first-arg]])
  (:import java.util.UUID))

;;
;; ## Prelude
;;

;;
;; Goal: the only things that are questionably serializable are
;; user-supplied functions.
;;

(defmulti -job
  "Returns a [job new-search-manager] if there is a job available."
  type-kw-of-first-arg)

(defmulti -report
  "Returns a [job new-search-manager]"
  type-kw-of-first-arg)

(defmulti done?
  type-kw-of-first-arg)

(defmulti results
  type-kw-of-first-arg)

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

(defn ^:private search-manager
  [type & kvs]
  (assoc (apply hash-map kvs)
    :type type
    :report-count 0))

(defn search-manager?
  [x]
  (and (map? x)
       (isa? (:type x) ::search-manager)))


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

;; TODO: should we drop nil results??
(defn lazy-seq-search-manager
  "A search manager that creates a job for every item in the given
   collection, where the job consists of calling the given function.
   The return value of the function will be added to the
   result-holder, which defaults to a vector."
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
  type-kw-of-first-arg)

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
             [data' score'] (-> nested results single-result)]
         (if (> score' score)
           (make-lazy-sm [data' score'])
           [data score]))))))


;;
;; Randomness
;;

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
;;       checker is a bad name since the result is actually whatever
;;       the checker returns.
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


(derives ::repeatedly ::search-manager ::delegator)
(defmethod results ::repeatedly [m] (:results m))
(defmethod done? ::repeatedly [_] false)
(defmethod finished-delegatee ::repeatedly
  [me inner-sm]
  (-> me
      (update :results add-result (results inner-sm))
      (update :searches-finished inc)
      (assoc :nested-search-manager ((:generator me)))))

(defn repeatedly-search-manager
  "generator is a function that returns a search-manager. Results is
   a container that will be passed the final `results` of each run of
   the inner search manager."
  [generator results]
  (search-manager ::repeatedly
                  :generator generator
                  :nested-search-manager (generator)
                  :searches-finished 0
                  :results results))
