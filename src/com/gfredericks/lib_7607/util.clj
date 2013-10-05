(ns com.gfredericks.lib-7607.util)

(defn update [m k f & args] (apply update-in m [k] f args))

(defn derives
  "Derive a type from several parents at once (in the global hierarchy)."
  [tag & parents]
  (doseq [parent parents] (derive tag parent)))

(defn type-kw-of-first-arg
  "Returns the :type entry in the first argument."
  [x & more]
  (:type x))
