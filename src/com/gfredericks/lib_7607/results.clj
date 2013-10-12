(ns com.gfredericks.lib-7607.results
  "Specialized data structures for holding results of searches."
  (:refer-clojure :exclude [type])
  (:require [com.gfredericks.lib-7607.util :refer [update]]))

;; TODO: print-these with metadata? or let that be set elsewhere?
;;
;; I'm starting to lean toward doing this with only maps, and explicit
;; :type entries (rather than on the metadata). Solves the
;; serialization problems and is less magical.

(defn ^:private type
  [coll]
  (or (if (map? coll) (:type coll))
      (class coll)))

(defmulti add-result
  "Multimethod to add a result to a collection."
  (fn [coll res] (type coll)))

(defmethod add-result clojure.lang.IPersistentCollection
  [coll x]
  (conj coll x))

(defmulti results-seq type)

(defn single-result
  "Given a result holder, checks that there is exactly one result and
  returns it."
  [m]
  (let [[x :as xs] (results-seq m)]
    (assert (= 1 (count xs)) "single-result requires exactly one result")
    x))


(defmethod add-result ::best-result-keeper
  [coll x]
  (if-let [[data score] (:result coll)]
    (let [[data' score'] x]
      (if (neg? (compare score score'))
        (assoc coll :result x)
        coll))
    (assoc coll :result x)))

(defmethod results-seq ::best-result-keeper [m] (if (contains? m :result) (list (:result m))))

(def best-result-keeper
  "A vector that expects entries like [data score] and will only keep
   the best result. Earlier entries win tiebreaks."
  {:type ::best-result-keeper})


(defmethod add-result ::grouper-by
  [{:keys [group-fn empty] :as m} x]
  (let [group-key (group-fn x)]
    (update-in m [:results group-key]
               (fnil add-result empty)
               x)))

(defn grouper-by
  [group-fn empty-nested-coll]
  {:type ::grouper-by
   :group-fn group-fn
   :empty empty-nested-coll
   :results {}})


(deftype SampledCollection [max-size actual-size set]
  Object
  (hashCode [_] (.hashCode set))
  (toString [me] (pr-str (seq me)))

  clojure.lang.Seqable
  (seq [_] (map second set))

  clojure.lang.IPersistentCollection
  (count [_] (count set))
  (cons [_ x]
    (let [r (rand)
          set' (cond-> set
                       (or (< (count set) max-size)
                           (> r (ffirst set)))
                       (conj [r x]))
          set'' (if (> (count set') max-size)
                  (disj set' (first set'))
                  set')]
      (SampledCollection.
       max-size
       (inc actual-size)
       set'')))
  (empty [_]
    (SampledCollection. max-size 0 (empty set)))
  (equiv [me o]
    ;; eh whatever
    (.equiv (seq me) o)))

(defn sampled-collection
  ([max-size]
     (SampledCollection. max-size 0 (sorted-set-by #(compare (first %1) (first %2)))))
  ([max-size & xs] (into (sampled-collection max-size) xs)))
