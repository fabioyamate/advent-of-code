(ns advent-of-code-2020.day01
  (:use clojure.test)
  (:require [clojure.java.io :as io]))

(defn solve-naive
  ;; just being lazy and using sets here...
  [coll]
  (let [x (first coll)
        y (- 2020 (or x 0))]
    (cond (empty? coll)
          -1

          (contains? (set (rest coll)) y)
          (* x y)

          :else
          (solve-naive (rest coll)))))

(deftest level-1
  (is (= -1 (solve-naive [])))
  (is (= -1 (solve-naive [1])))
  (is (= -1 (solve-naive [1 2])))
  (is (= 2019 (solve-naive [2019 1])))
  (is (= 2019 (solve-naive [0 1 2019])))
  (is (= 514579 (solve-naive [1721 979 366 299 675 1456]))))

(defn compute [f]
  (with-open [rdr (io/reader (io/resource "advent-of-code-2020/day01.in"))]
    (f (mapv #(Integer/parseInt %) (line-seq rdr)))))

;; the input file contains 200 lines
;; if we do a naive permutation it is 200 * 199 * 198 = ~8MM combinations
;; this size is ok to brute force, and if we find one
;; solution we will short circuit the computation

(defn solve-naive-1
  [coll]
  (first
   (for [x coll
         :let [ys (rest coll)]
         y ys
         :when (= 2020 (+ x y))]
     (* x y))))

(defn solve-naive-2
  ;; just using list compreehention for getting all combinations
  [coll]
  (first
   (for [x coll
         :let [ys (rest coll)
               zs (rest ys)]
         y ys
         z zs
         :when (= 2020 (+ x y z))]
     (* x y z))))

(deftest problems
  (is (= 713184 (compute solve-naive)))
  (is (= 713184 (compute solve-naive-1)))
  (is (= 261244452 (compute solve-naive-2))))
