(ns com.gfredericks.lib-7607.pprint
  "A tweaked version of fipp.edn."
  (:require [fipp.printer :as printer :refer (defprinter)]))

(declare pretty)

(defprotocol IPretty
  (-pretty [x]))

(defn system-id [obj]
  (Integer/toHexString (System/identityHashCode obj)))

(defn pretty-map [m]
  (let [kvps (for [[k v] (sort-by (comp str key) m)]
               [:span (pretty k) " " (pretty v)])
        doc [:group "{" [:align (interpose [:span "," :line] kvps)]  "}"]]
    (if (instance? clojure.lang.IRecord m)
      [:span "#" (-> m class .getName) doc]
      doc)))

(extend-protocol IPretty

  nil
  (-pretty [x]
    [:text "nil"])

  java.lang.Object
  (-pretty [x]
    [:text (pr-str x)])

  clojure.lang.IPersistentVector
  (-pretty [v]
    [:group "[" [:align (interpose :line (map pretty v))] "]"])

  clojure.lang.ISeq
  (-pretty [s]
    [:group "(" [:align (interpose :line (map pretty s))] ")"])

  clojure.lang.IPersistentMap
  (-pretty [m]
    (pretty-map m))

  clojure.lang.IPersistentSet
  (-pretty [s]
    [:group "#{" [:align (interpose :line (map pretty s)) ] "}"])

  ;;TODO figure out how inheritence is resolved...
  clojure.lang.IRecord
  (-pretty [r]
    (pretty-map r))

  clojure.lang.Atom
  (-pretty [a]
    [:span "#<Atom@" (system-id a) " " (pretty @a) ">"])

  java.util.concurrent.Future
  (-pretty [f]
    (let [value (if (future-done? f)
                  (pretty @f)
                  ":pending")]
      [:span "#<Future@" (system-id f) " " value ">"]))

  ;TODO clojure.lang.PersistentQueue, lots more stuff too

  )

(defn pretty [x]
  (if (= (type x) (class x))
    (-pretty x)
    (let [sw (java.io.StringWriter.)]
      (print-method x sw)
      [:span (str sw)])))

(defprinter pprint pretty
  {:width 110})
