(ns advent-of-code-2019.day01
  (:use clojure.test)
  (:require [clojure.java.io :as io]))

;; https://adventofcode.com/2019/day/1

(defn mass->fuel
  [mass]
  (- (quot mass 3) 2))

(defn mass->fuel-2
  [mass]
  (loop [required-fuel 0
         mass mass]
    (let [fuel (mass->fuel mass)]
      (if-not (pos? fuel)
        required-fuel
        (recur (+ required-fuel fuel) fuel)))))

(deftest level-1
  (is (= 2 (mass->fuel 12)))
  (is (= 2 (mass->fuel 14)))
  (is (= 654 (mass->fuel 1969)))
  (is (= 33583 (mass->fuel 100756))))

(deftest level-2
  (is (= 2 (mass->fuel-2 14)))
  (is (= 966 (mass->fuel-2 1969)))
  (is (= 50346 (mass->fuel-2 100756))))

(defn compute [f]
  (with-open [rdr (io/reader (io/resource "advent-of-code-2019/day01.in"))]
    (transduce
     (comp (map #(Integer/parseInt %))
           (map f))
     +
     (line-seq rdr))))

(compute mass->fuel)
;; => 3167282
(compute mass->fuel-2)
;; => 4748063
