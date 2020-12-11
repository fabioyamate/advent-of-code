(ns advent-of-code-2020.day05
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn decode
  [s]
  (str/replace s #"F|B|R|L" {"F" "0" "B" "1" "R" "1" "L" "0"}))

;; by reading the exercise, you notice that the math is binary
;; then you can conclude it is doing binary representation of number
;;
;; so we just need to represent the number into binary
;;
;; in this case F is 0, B is 1
;; the same applies for R is 1 and B is 0
;;
;; another evidence is that it states that the
;; first 7 position represents, 0-127, which is 2^7
;; last 3 digits, 0-8, which is 2^3
;;
;; the last part makes it easier as it says that the ID
;; is the first part times 8, plus second part.
;;
;; multiply by 8 is basically doing (shift 3)
;;
;; so in summary FFFFFFFLLL = 0000000000
;; BFFFBBFRRR = 1000110111 = 567 base 10
;; FFFBBBFRRR = 0001110111 = 119 base 10

(defn solve-1
  [data]
  (-> data
      (str/split-lines)
      (->> (map decode))
      sort
      last
      (Integer/parseInt 2)))

;; so the solution taken here is to just find seats whose different is 2,
;; then my seat should be half of this two numbers.

(defn solve-2
  [data]
  (-> data
      (str/split-lines)
      (->> (map decode)
           (map #(Integer/parseInt % 2)))
      sort
      (->> (partition 2 1)
           (filter (fn [[x y]] (= 2 (- y x)))))))

(deftest solve-test
  (let [data (slurp (io/resource "advent-of-code-2020/day05.in"))]
    (is (= 911 (solve-1 data)))
    (is (= [[628 630]] (solve-2 data)))))
