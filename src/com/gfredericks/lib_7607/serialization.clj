(ns com.gfredericks.lib-7607.serialization
  "Utilities for creatively serialized things."
  (:refer-clojure :exclude [defn partial])
  (:require [clojure.core :as core]))

;;
;; Lazy Seq serialization
;;

(deftype MetadSeq [m ^clojure.lang.ISeq s]
  clojure.lang.Sequential
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

(core/defn droppingly-printable-lazy-seq
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

;;
;; fn serialization
;;

(defmethod print-method ::prints-as-tagged-literal
  [x ^java.io.Writer w]
  (let [{:keys [print-tag print-data]} (meta x)]
    (doto w
      (.write "#")
      (.write (str print-tag))
      (.write " ")
      (.write (str (force print-data))))))

(core/defn read-var
  "Used by the #cereal/var data reader. Expects a fully qualified symbol."
  [sym]
  (let [ns-sym (symbol (namespace sym))]
    (when-not (find-ns ns-sym) (require ns-sym)))
  @(resolve sym))

(derive ::serializable-defn ::prints-as-tagged-literal)

(core/defn serializablize
  [a-var]
  (let [{ns-ob :ns, name-sym :name} (meta a-var)]
    (alter-var-root a-var vary-meta assoc
                    :type ::serializable-defn
                    :print-tag 'cereal/var
                    :print-data (symbol (name (.getName ns-ob)) (name name-sym)))))

(defmacro defn
  "Same format as clojure.core/defn, but the defined function will
  print as a #cereal/var form and will read in by dereffing the var."
  [& defn-args]
  `(doto (core/defn ~@defn-args)
     (serializablize)))

(core/defn partial
  [f & args]
  (vary-meta (apply core/partial f args) assoc
             :type ::prints-as-tagged-literal
             :print-tag 'cereal/partial
             :print-data (cons f args)))

(core/defn read-partial
  [[f & args]]
  (apply partial f args))

(core/defn juxt
  [& fs]
  (vary-meta (apply core/juxt fs) assoc
             :type ::prints-as-tagged-literal
             :print-tag 'cereal/juxt
             :print-data fs))

(core/defn read-juxt
  [fs]
  (apply juxt fs))
