(ns advent-of-code-2019.day04
  (:use clojure.test)
  (:require [clojure.java.io :as io]))

;; Day 4 - Secure Container
;; https://adventofcode.com/2019/day/4

(defn digits
  [input]
  (seq (str input)))

(defn only-increasing-digits?
  ;; given the input is too small, doing super naive check
  [input]
  (= (sort (digits input))
     (digits input)))

(defn double-digits?
  [input]
  (< (count (distinct (digits input)))
     6))

(deftest level-1
  (let [valid-password? (every-pred only-increasing-digits?
                                    double-digits?)]
    (is (valid-password? 111111))
    (is (not (valid-password? 223450)))
    (is (not (valid-password? 123789)))))

(->> (range 235741 706948)
     (filter (every-pred only-increasing-digits?
                         double-digits?))
     count)
;; => 1178

(defn exactly-double-digits?
  [input]
  (some #(= 2 %)
        (vals (frequencies (digits input)))))

(deftest level-2
  (let [valid-password? (every-pred only-increasing-digits?
                                    exactly-double-digits?)]
    (is (not (valid-password? 111111)))
    (is (not (valid-password? 223450)))
    (is (not (valid-password? 123789)))
    (is (valid-password? 112233))
    (is (not (valid-password? 123444)))
    (is (valid-password? 111122))
    (is (valid-password? 122345))
    (is (valid-password? 113444))))

(->> (range 235741 706948)
     (filter (every-pred only-increasing-digits?
                         exactly-double-digits?))
     count)
;; => 763
