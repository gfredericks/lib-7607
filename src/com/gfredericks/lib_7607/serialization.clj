(ns com.gfredericks.lib-7067.serialization
  "Utilities for creatively serialized things.")

(deftype MetadSeq [m ^clojure.lang.ISeq s]
  clojure.lang.IMeta
  (meta [_] m)
  clojure.lang.ISeq
  (seq [me] (when (seq s) me))
  (count [me] (count s))
  (empty [me] (with-meta () m))
  (equiv [me o] (= s o))

  (first [_] (.first s))
  (next [_] (.next s))
  (more [_] (.more s))
  (cons [me o] (clojure.lang.RT/cons o me)))

(defn droppingly-printable-lazy-seq
  ([sequence orig-form] (droppingly-printable-lazy-seq sequence orig-form 0))
  ([sequence orig-form drop-num]
     (MetadSeq.
      {:type ::droppingly
       ::orig-form orig-form
       ::drop-num drop-num}
      (lazy-seq
       (if-let [[x & xs] (seq sequence)]
         (cons x (droppingly-printable-lazy-seq xs orig-form (inc drop-num))))))))

(defmacro droppingly-printably
  [expr]
  `(droppingly-printable-lazy-seq ~expr '~expr))

(defmethod print-method ::droppingly
  [seq ^java.io.Writer w]
  (let [{orig-form ::orig-form, drop-num ::drop-num} (meta seq)]
    (.write w "#=(drop ")
    (.write w (str drop-num " "))
    (print-method orig-form w)
    (.write w ")")))
