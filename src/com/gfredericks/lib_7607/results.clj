(ns com.gfredericks.lib-7607.results
  "Specialized data structures for holding results of searches.")


(defmulti add-result
  "Multimethod to add a result to a collection."
  (fn [coll res] (type coll)))

(defmethod add-result clojure.lang.IPersistentCollection
  [coll x]
  (conj coll x))

(defmethod add-result ::best-result-keeper
  [coll x]
  (if (empty? coll)
    (conj coll x)
    (let [[[data score]] coll
          [data' score'] x]
      (if (neg? (compare score score'))
        (assoc coll 0 x)
        coll))))

(def best-result-keeper
  (with-meta
    []
    {:type ::best-result-keeper}))
