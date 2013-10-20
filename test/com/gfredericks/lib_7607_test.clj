(ns com.gfredericks.lib-7607-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.gfredericks.lib-7607 :refer [searcher pause]]
            [com.gfredericks.lib-7607.managers :refer :all]
            [com.gfredericks.lib-7607.serialization :as cereal]))

;;
;; TODO:
;;   - test serialization!!!!
;;   - test a tree search
;;   - test some sort of infinite thing?
;;

(defn ^:private run-search
  [sm]
  (let [now #(System/currentTimeMillis)
        a (searcher sm {})
        timeout (+ (now) 5000)]
    (while (or (-> @a :search-manager done? not)
               (-> @a :threads count pos?))
      (when (> (now) timeout)
        (pause a)
        (throw (ex-info "Timed out during test!" {:searcher a})))
      (when-let [data (-> @a :crashed-threads rand-nth)]
        (pause a)
        (throw (ex-info (str "Search thread crashed! " (:throwable data))
                        {:info data}
                        (:throwable data))))
      (Thread/sleep 100))
    (-> @a :search-manager results)))

(defn ^:private round-trip-same-result?
  [sm]
  (let [r1 (-> sm run-search)
        r2 (-> sm pr-str read-string run-search)]
    (= r1 r2)))



(cereal/defn easy-search-func
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
   easy-search-func
   (sorted-set)))

(deftest easy-search-test
  (is (= 9009 (-> easy-search-sm run-search rseq first)))
  (testing "Serialization"
    (is (round-trip-same-result? easy-search-sm))))



(cereal/defn random-guess-needle-generator [] (rand-int 100))
(cereal/defn random-guess-needle-checker
  [n]
  (if (-> n str reverse (= [\7 \4]))
    n))

(def random-guess-needle-sm
  (random-guess-needle-search-manager
   random-guess-needle-generator
   random-guess-needle-checker))

(deftest random-guess-needle-test
  (is (-> random-guess-needle-sm run-search (= 47)))
  (testing "Serialization"
    (is (round-trip-same-result? random-guess-needle-sm))))

(cereal/defn map-reduce-map-fn
  [n]
  [n (->> n str (map str) (map read-string) (reduce +))])
(cereal/defn map-reduce-reduce-fn
  [a b]
  (max-key first a b))

(def map-reduce-sm
  (map-quick-reducing-search-manager
   map-reduce-map-fn
   map-reduce-reduce-fn
   [0 0]
   (range 1000)))

(deftest map-reduce-test
  (is (-> map-reduce-sm run-search first (= 999)))
  (testing "Serialization"
    (is (round-trip-same-result? map-reduce-sm))))



(cereal/defn first-result-func
  [x]
  (when (= "38" (str x)) x))

(def first-result-sm
  (lazy-seq-first-result-manager
   (range 75)
   first-result-func))

(deftest first-result-manager-test
  (is (= 38 (-> first-result-sm run-search)))
  (testing "Serialization"
    (is (round-trip-same-result? first-result-sm))))



(cereal/defn iterator-div-checker
  [n d]
  (when (zero? (rem n d)) [(/ n d) [d]]))

(cereal/defn iterator-func
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
      (cereal/partial iterator-div-checker n))
     (cereal/partial iterator-func n primes))))

(deftest iterator-test
  (is (= [3 3 13] (-> iterator-sm run-search sort)))
  (testing "Serialization"
    ;; doesn't seem to be an easy way to get the results canonized at the moment :/
    (is (= (-> iterator-sm run-search sort)
           (-> iterator-sm pr-str read-string run-search sort)))))



(cereal/defn hill-climbing-neighbors
  [[x y]]
  (let [e 1/1000]
    [[(+ x e) y] [(- x e) y] [x (+ y e)] [x (- y e)]]))

(cereal/defn hill-climbing-scorer
  [[x y]]
  (letfn [(f [z] (inc (- (* (dec z) (dec z)))))]
    (+ (f x) (f y))))

(def hill-climbing-sm
  (hill-climbing-search-manager
   [(/ (rand-int 2000) 1000)
    (/ (rand-int 2000) 1000)]
   hill-climbing-neighbors
   hill-climbing-scorer))

(deftest hill-climbing-test
  (is (= [1 1]
         (-> hill-climbing-sm
             run-search
             first)))
  (testing "Serialization"
    (is (round-trip-same-result? hill-climbing-sm))))



(cereal/defn constantly-random-guess-needle-sm
  []
  random-guess-needle-sm)

(def repeatedly-sm
  (repeatedly-search-manager
   constantly-random-guess-needle-sm
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

(cereal/defn slow-identity
  [x]
  (Thread/sleep 5)
  x)

(def slow-search
  (lazy-seq-search-manager
            (range 200)
            slow-identity
            #{}))

(deftest persistence-test
  (let [f (java.io.File/createTempFile "lib-7607-test" "")
        s (searcher slow-search
                    {:thread-count 4
                     :persistence
                     {:interval 35
                      :file f}})
        results #(-> f slurp read-string :results)]
    (Thread/sleep 300)
    (let [nums (results)]
      (Thread/sleep 100)
      (is (-> (results) count (> (count nums)))))
    (while (not (done? (:search-manager @s))) (Thread/sleep 10))
    (Thread/sleep 10)
    (is (= (set (range 200)) (results)))))
