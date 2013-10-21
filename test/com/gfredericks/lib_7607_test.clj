(ns com.gfredericks.lib-7607-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.gfredericks.lib-7607 :refer [searcher pause]]
            [com.gfredericks.lib-7607.managers :refer :all]
            [com.gfredericks.lib-7607.results :as results]
            [com.gfredericks.lib-7607.serialization :as cereal]))

;;
;; TODO:
;;   - test serialization!!!!
;;   - test a tree search
;;   - test some sort of infinite thing?
;;

(defn ^:private run-search
  "Runs the search manager and returns the results. If milliseconds is
  given, runs for that long and then pauses. Otherwise runs until done."
  [sm & [milliseconds]]
  (let [now #(System/currentTimeMillis)
        a (searcher sm {})
        normal-timeout (+ (now) (or milliseconds 1000000))
        error-timeout (+ (now) 5000)]
    (while (and (< (now) normal-timeout)
                (or (-> @a :search-manager done? not)
                    (-> @a :threads count pos?)))
      (when (> (now) error-timeout)
        (pause a)
        (throw (ex-info "Timed out during test!" {:searcher a})))
      (when-let [data (-> @a :crashed-threads rand-nth)]
        (pause a)
        (throw (ex-info (str "Search thread crashed! " (:throwable data))
                        {:info data}
                        (:throwable data))))
      (Thread/sleep 100))
    (pause a)
    (-> @a :search-manager results)))

(defn ^:private round-trip-same-result?
  [sm]
  (let [r1 (-> sm run-search)
        r2 (-> sm pr-str read-string run-search)]
    (= r1 r2)))



(defn easy-search-func
  [[x y]]
  (let [z (* x y)]
    (if (= (str z) (->> z str reverse (apply str)))
      z)))

(def easy-search-sm
  ;; Like http://projecteuler.net/problem=4 but quicker
  ;;
  ;; It's worth noting that the 3-digit version could be sped up by
  ;; doing the inner loop inside a job. But maybe we should wait for
  ;; an abstraction that does this for us?
  (lazy-seq-search-manager
   (for [x (range 10 100), y (range 10 100)] [x y])
   #cereal/var easy-search-func
   (sorted-set)))

(deftest easy-search-test
  (is (= 9009 (-> easy-search-sm run-search rseq first)))
  (testing "Serialization"
    (is (round-trip-same-result? easy-search-sm))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; random-guess-needle ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn random-guess-needle-generator [] (rand-int 100))
(defn random-guess-needle-checker
  [n]
  (if (-> n str reverse (= [\7 \4]))
    n))

(def random-guess-needle-sm
  (random-guess-needle-search-manager
   #cereal/var random-guess-needle-generator
   #cereal/var random-guess-needle-checker))

(deftest random-guess-needle-test
  (is (-> random-guess-needle-sm run-search (= 47)))
  (testing "Serialization"
    (is (round-trip-same-result? random-guess-needle-sm))))


;;;;;;;;;;;;;;;;;;
;; random-guess ;;
;;;;;;;;;;;;;;;;;;

(defn random-guess-gen [] (rand-int 1000))
(defn random-guess-check [x]
  (if (zero? (rem x 3))
    x))
(defn random-guess-group [x] (rem x 18))

(def random-guess-sm
  (random-guess-search-manager
   #cereal/var random-guess-gen
   #cereal/var random-guess-check
   (results/grouper-by #cereal/var random-guess-group [])))

(deftest random-guess-test
  (is (= [0 3 6 9 12 15]
         (-> random-guess-sm
             (run-search 200)
             (:results)
             (keys)
             (sort)))))


;;;;;;;;;;;;;;;;
;; map-reduce ;;
;;;;;;;;;;;;;;;;

(defn map-reduce-map-fn
  [n]
  [n (->> n str (map str) (map read-string) (reduce +))])
(defn map-reduce-reduce-fn
  [a b]
  (max-key first a b))

(def map-reduce-sm
  (map-quick-reducing-search-manager
   #cereal/var map-reduce-map-fn
   #cereal/var map-reduce-reduce-fn
   [0 0]
   (range 1000)))

(deftest map-reduce-test
  (is (-> map-reduce-sm run-search first (= 999)))
  (testing "Serialization"
    (is (round-trip-same-result? map-reduce-sm))))



(defn first-result-func
  [x]
  (when (= "38" (str x)) x))

(def first-result-sm
  (lazy-seq-first-result-manager
   (range 75)
   #cereal/var first-result-func))

(deftest first-result-manager-test
  (is (= 38 (-> first-result-sm run-search)))
  (testing "Serialization"
    (is (round-trip-same-result? first-result-sm))))



(defn iterator-div-checker
  [n d]
  (when (zero? (rem n d)) [(/ n d) [d]]))

(defn iterator-func
  [n primes sm]
  (let [[n ds] (results sm)]
    (if (= 1 n)
      ds
      (lazy-seq-first-result-manager
       primes
       #(when (zero? (rem n %)) [(/ n %) (conj ds %)])))))

(def iterator-sm
  (let [primes [2 3 5 7 11 13 17 19]
        ;; 3 * 3 * 13
        n 117]
    (iterator-search-manager
     (lazy-seq-first-result-manager
      primes
      (cereal/partial #cereal/var iterator-div-checker n))
     (cereal/partial #cereal/var iterator-func n primes))))

(deftest iterator-test
  (is (= [3 3 13] (-> iterator-sm run-search sort)))
  (testing "Serialization"
    ;; doesn't seem to be an easy way to get the results canonized at the moment :/
    (is (= (-> iterator-sm run-search sort)
           (-> iterator-sm pr-str read-string run-search sort)))))



(defn hill-climbing-neighbors
  [[x y]]
  (let [e 1/1000]
    [[(+ x e) y] [(- x e) y] [x (+ y e)] [x (- y e)]]))

(defn hill-climbing-scorer
  [[x y]]
  (letfn [(f [z] (inc (- (* (dec z) (dec z)))))]
    (+ (f x) (f y))))

(def hill-climbing-sm
  (hill-climbing-search-manager
   [(/ (rand-int 2000) 1000)
    (/ (rand-int 2000) 1000)]
   #cereal/var hill-climbing-neighbors
   #cereal/var hill-climbing-scorer))

(deftest hill-climbing-test
  (is (= [1 1]
         (-> hill-climbing-sm
             run-search
             first)))
  (testing "Serialization"
    (is (round-trip-same-result? hill-climbing-sm))))



(defn constantly-random-guess-needle-sm
  []
  random-guess-needle-sm)

(def repeatedly-sm
  (repeatedly-search-manager
   #cereal/var constantly-random-guess-needle-sm
   []))

(deftest repeatedly-test
  (let [s (searcher repeatedly-sm {})]
    (Thread/sleep 250)
    (pause s)
    (let [results (-> @s :search-manager results)]
      (is (vector? results))
      (is (> 50 (count results)))
      (is (= #{47} (set results)))))
  (testing "Serialization"
    (let [s (-> repeatedly-sm
                (pr-str)
                (read-string)
                (searcher {}))]
      (Thread/sleep 250)
      (pause s)
      (is (= #{47} (-> @s :search-manager results (set)))))))


(deftest restart-test
  (let [sm (lazy-seq-search-manager
            (range 10000)
            identity
            #{})
        s (searcher sm {:thread-count 1})]
    (Thread/sleep 100)
    (let [sm' (:search-manager @s)]
      (pause s)
      (while (pos? (count (:threads @s))) (Thread/sleep 10))
      (is (not (done? sm')))
      (is (= (set (range 10000))
             (run-search sm'))))))

(defn slow-identity
  [x]
  (Thread/sleep 5)
  x)

(def slow-search
  (lazy-seq-search-manager
            (range 100)
            #cereal/var slow-identity
            #{}))

(deftest persistence-test
  (let [f (java.io.File/createTempFile "lib-7607-test" "")
        s (searcher slow-search
                    {:thread-count 1
                     :persistence
                     {:interval 35
                      :file f}})
        results #(-> f slurp read-string :results)]
    (Thread/sleep 300)
    (let [nums (results)]
      (Thread/sleep 100)
      (is (-> (results) count (> (count nums)))))
    (while (not (done? (:search-manager @s))) (Thread/sleep 10))
    (Thread/sleep 100)
    (is (= (set (range 100)) (results)))))
