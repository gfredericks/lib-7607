(ns com.gfredericks.lib-7607.results
  "Specialized data structures for holding results of searches.")


(defmulti add-result
  "Multimethod to add a result to a collection."
  (fn [coll res] (type coll)))
