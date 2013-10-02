(ns com.gfredericks.lib-7607-test
  (:require [clojure.test :refer [deftest is]]
            [com.gfredericks.lib-7607 :refer [searcher pause]]
            [com.gfredericks.lib-7607.managers :refer :all]))

;;
;; TODO:
;;   - test a tree search
;;   - test a hill-climbing search
;;     - such as sorting!
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
                        {:info data})))
      (Thread/sleep 100))
    (-> @a :search-manager results)))


(deftest easy-search-test
  ;; Like http://projecteuler.net/problem=4 but quicker
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
            (fn [[n ds]]
              (if (= 1 n)
                ds
                (lazy-seq-first-result-manager
                 primes
                 #(when (zero? (rem n %)) [(/ n %) (conj ds %)])))))]
    (is (= [3 3 13]
           (-> sm run-search sort)))))


;;
;; Having a hard time seeing how to not couple the
;; hill-climbing-search-manager with the reporter, since the hill
;; climbing algorithm kind of incorporates a score that the reporter
;; would also want to pay attention to. I guess we could just have the
;; general convention that when scores exist they do so as [data
;; score] tuples. Dunno. That ordering doesn't fit naturally with
;; sorted-sets though. Grr...
;;

#_(deftest hill-climbing-sort-test
  (let [sm (hill-climbing-search-manager
            ;; random instance generator
            (fn [] (shuffle (range 20)))
            ;; option generator
            (fn [coll]
              (for [x (-> coll count dec range)]
                #(-> %
                     (assoc x (get % (inc x)))
                     (assoc (inc x) (get % x)))))
            ;; evaluator
            (fn [coll]
              (->> coll
                   (partition 2 1)
                   (filter (fn [[a b]] (< a b)))
                   (count)))
            ;; result holder
            (best-result-holder))
        a (searcher sm {})]
    (Thread/sleep 5000)
    (is (-> @a
            :search-manager
            :result-holder
            first
            (= (range 20))))))
