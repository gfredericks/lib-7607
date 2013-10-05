(ns com.gfredericks.lib-7607.results
  "Specialized data structures for holding results of searches.")

;; TODO: print-these with metadata? or let that be set elsewhere?

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
  "A vector that expects entries like [data score] and will only keep
   the best result. Earlier entries win tiebreaks."
  (with-meta
    []
    {:type ::best-result-keeper}))


(defmethod add-result ::grouper-by
  [m x]
  (let [group-key ((-> m meta :group-fn) x)
        group (or (get m group-key)
                  (-> m meta :empty-nested-coll))]
    (assoc m group-key (add-result group x))))

(defn grouper-by
  [group-fn empty-nested-coll]
  (with-meta
    {}
    {:type ::grouper-by
     :group-fn group-fn
     :empty-nested-coll empty-nested-coll}))


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
