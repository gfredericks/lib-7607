(ns com.gfredericks.lib-7607-test
  (:require [clojure.test :refer [deftest is]]
            [com.gfredericks.lib-7607 :refer [searcher pause]]
            [com.gfredericks.lib-7607.managers :refer :all]))

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
        (throw (ex-info (str "Search thread crashed! " (str (:throwable data)))
                        {:info data}
                        (:throwable data))))
      (Thread/sleep 100))
    (-> @a :search-manager results)))


(deftest easy-search-test  ;; Like http://projecteuler.net/problem=4 but quicker
  ;;
  ;; It's worth noting that the 3-digit version could be sped up by
  ;; doing the inner loop inside a job. But maybe we should wait for
  ;; an abstraction that does this for us?
  (let [sm (lazy-seq-search-manager
            (for [x (range 10 100), y (range 10 100)] [x y])
            (fn [[x y]]
              (let [z (* x y)]
                (if (= (str z) (->> z str reverse (apply str)))
                  z)))
            (sorted-set))]
    (is (= 9009 (-> sm run-search rseq first)))))

(deftest random-guess-test
  (let [sm (random-guess-search-manager
            #(rand-int 100)
            #(if (-> % str reverse (= [\7 \4]))
               %))]
    (is (-> sm run-search (= 47)))))

(deftest map-reduce-test
  (let [sm (map-quick-reducing-search-manager
            (fn [n]
              [n (->> n str (map str) (map read-string) (reduce +))])
            #(max-key first %1 %2)
            [0 0]
            (range 1000))]
    (is (-> sm run-search first (= 999)))))

(deftest first-result-manager-test
  (let [sm (lazy-seq-first-result-manager
            (range 75)
            #(when (= "38" (str %)) %))]
    (is (= 38 (-> sm run-search)))))

(deftest iterator-test
  (let [primes [2 3 5 7 11 13 17 19]
        n 117 ;; 3 * 3 * 13
        sm (iterator-search-manager
            (lazy-seq-first-result-manager
             primes
             #(when (zero? (rem n %)) [(/ n %) [%]]))
            (fn [sm]
              (let [[n ds] (results sm)]
                (if (= 1 n)
                  ds
                  (lazy-seq-first-result-manager
                   primes
                   #(when (zero? (rem n %)) [(/ n %) (conj ds %)]))))))]
    (is (= [3 3 13]
           (-> sm run-search sort)))))


(deftest hill-climbing-test
  (let [f' (fn [z] (inc (- (* (dec z) (dec z)))))
        f (fn [x y] (+ (f' x) (f' y)))
        e 1/1000
        sm (hill-climbing-search-manager
            [(/ (rand-int 2000) 1000)
             (/ (rand-int 2000) 1000)]
            (fn [[x y]]
              [[(+ x e) y] [(- x e) y] [x (+ y e)] [x (- y e)]])
            (fn [[x y]] (f x y)))]
    (is (= [1 1]
           (-> sm
               run-search
               first)))))

(deftest repeatedly-test
  (let [fsm (fn []
              (random-guess-search-manager
               #(rand-int 100)
               #(if (-> % str reverse (= [\7 \4]))
                  %)))
        sm (repeatedly-search-manager
            fsm
            [])
        s (searcher sm {})]
    (Thread/sleep 250)
    (pause s)
    (let [results (-> @s :search-manager results)]
      (is (vector? results))
      (is (> 50 (count results)))
      (is (= #{47} (set results))))))
