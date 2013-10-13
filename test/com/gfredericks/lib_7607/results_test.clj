(ns com.gfredericks.lib-7607.results-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.lib-7607.results :refer :all]))

(defmacro is-> [val & exprs]
  `(let [x# ~val]
     (is (-> x# ~@exprs))
     x#))

(defmacro is=-> [val val' & exprs]
  "First arg should equal second arg after being threaded through exprs."
  `(let [x# ~val]
     (is (= ~val' (-> x# ~@exprs)))
     x#))

(deftest best-result-keeper-test
  (-> best-result-keeper
      (add-result [:foo 3])
      (is=-> [:foo 3] (:result))
      (add-result [:bar 1])
      (is=-> [:foo 3] (:result))
      (add-result [:baz 5])
      (is=-> [:baz 5] (:result))))

(deftest grouper-by-test
  (-> (grouper-by namespace #{})
      (add-result :foo/bar)
      (is=-> {"foo" #{:foo/bar}} (:results))
      (add-result :blam/blosh)
      (is=-> #{:blam/blosh} (get-in [:results "blam"]))
      (add-result :foo/blosh)
      (is=-> #{:foo/bar :foo/blosh} (get-in [:results "foo"]))))

(deftest sampler-test
  (-> (sampler 5)
      (as-> <> (reduce add-result <> (range 1000)))
      (results-seq)
      (is=-> 5 (count))
      (->> (map #(< % 1000)))
      (set)
      (is=-> #{true})))
