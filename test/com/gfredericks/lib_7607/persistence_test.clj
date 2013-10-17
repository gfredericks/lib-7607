(ns com.gfredericks.lib-7607.persistence-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.lib-7607.serialization :refer [droppingly-printably]]
            [com.gfredericks.lib-7607.persistence :as persistence]))

(deftest droppingly-serialization-test
  (let [f (java.io.File/createTempFile "foo" "")
        dropped-seq (droppingly-printably (range 10000))]
    (persistence/write f dropped-seq)
    (is (-> (slurp f)
            (count)
            (< 500))
        "Should serialize the droppingly-printably seq correctly.")))
