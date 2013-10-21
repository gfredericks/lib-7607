(ns com.gfredericks.lib-7607.serialization
  "Utilities for creatively serialized things."
  (:refer-clojure :exclude [juxt partial])
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

(defn droppingly-printable-lazy-seq
  ([sequence orig-form] (droppingly-printable-lazy-seq sequence orig-form 0))
  ([sequence orig-form drop-num]
     (MetadSeq.
      {:type ::droppingly
       ::print-tag 'cereal/drop
       ::print-data (vector drop-num orig-form)}
      (lazy-seq
       (if-let [[x & xs] (seq sequence)]
         (cons x (droppingly-printable-lazy-seq xs orig-form (inc drop-num))))))))

(defmacro droppingly-printably
  [expr]
  `(droppingly-printable-lazy-seq ~expr '~expr))

(derive ::droppingly ::prints-as-tagged-literal)

(defn read-drop
  [[n form]]
  (droppingly-printable-lazy-seq (drop n (eval form)) form n))

;;
;; fn serialization
;;

(defmethod print-method ::prints-as-tagged-literal
  [x ^java.io.Writer w]
  (let [{print-tag ::print-tag
         print-data ::print-data} (meta x)]
    (doto w
      (.write "#")
      (.write (str print-tag))
      (.write " ")
      (.write (str (force print-data))))))

(defn try-to-load-sym
  [sym]
  (if-let [ns-name (namespace sym)]
    (let [ns-sym (symbol ns-name)]
      (try (require ns-sym)
           (resolve sym)
           (catch java.io.FileNotFoundException e nil)))))

;; Macro for doing an IFn deftype that defines all 22 invoke clauses
;; for you.
(defmacro def-ifn-type
  [name proxy-name fields & clauses]
  `(deftype ~name ~fields
     clojure.lang.IFn
     ~@(for [i (range 22)
             :let [args (repeatedly i #(gensym "args"))]]
         (list `invoke
               (vec (list* 'this args))
               (list* '.invoke proxy-name args)))
     (applyTo [me# arglist#] (.applyTo ~proxy-name arglist#))
     ~@clauses))

(def-ifn-type Var v [^clojure.lang.Var v])

(defmethod print-dup Var
  [^Var x ^java.io.Writer w]
  (print-ctor x (fn [_ _] (print-dup (.v x) w)) w))

(defmethod print-method Var
  [^Var x ^java.io.Writer w]
  (doto w
    (.write "#cereal/var ")
    (.write (subs (str (.v x)) 2))))

(defn read-var
  "Used by the #cereal/var data reader. Expects a fully qualified
  symbol that resolves to any IObj (most likely a function). Returns
  the value of the var with added metadata to allow it to print as a
  #cereal/var tagged literal."
  [sym]
  (let [var (or (resolve sym)
                (try-to-load-sym sym)
                (throw (ex-info (str "Can't resolve #cereal/var " sym) {:sym sym})))
        vmeta (meta var)
        full-sym (symbol (-> vmeta :ns .getName str)
                         (-> vmeta :name str))]
    (Var. var)))

(defn partial
  [f & args]
  (vary-meta (apply core/partial f args) assoc
             :type ::prints-as-tagged-literal
             ::print-tag 'cereal/partial
             ::print-data (cons f args)))

(defn read-partial
  [[f & args]]
  (apply partial f args))

(defn juxt
  [& fs]
  (vary-meta (apply core/juxt fs) assoc
             :type ::prints-as-tagged-literal
             ::print-tag 'cereal/juxt
             ::print-data fs))

(defn read-juxt
  [fs]
  (apply juxt fs))
