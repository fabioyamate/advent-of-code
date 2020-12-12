(ns advent-of-code-2020.day06
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input
  [data]
  (str/split data #"\n\n"))

(def sample
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn solve-1
  [data]
  (apply +
         (for [group (map #(re-seq #"[a-z]" %) data)
               :let [freq (frequencies group)]]
           (count freq))))

(deftest solve-1-test
  (is (= 11 (solve-1 (read-input sample))))
  (is (= 6310 (solve-1 (read-input (slurp (io/resource "advent-of-code-2020/day06.in")))))))

(defn solve-2
  [data]
  (apply +
         (for [group (map #(re-seq #"\S+" %) data)]
           (count (apply set/intersection (map set group))))))

(deftest solve-2-test
  (is (= 6 (solve-2 (read-input sample))))
  (is (= 3193 (solve-2 (read-input (slurp (io/resource "advent-of-code-2020/day06.in")))))))
