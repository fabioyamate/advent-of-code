(ns advent-of-code-2020.day02
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

(s/def ::whitespace (s/+ #{\space \- \:}))
(s/def ::num (s/+ #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}))
(s/def ::range
  (s/cat
   :range (s/cat :min ::num
                 :separator ::whitespace
                 :max ::num)))

(s/def ::letter
  (set (seq "abcdefghijklmnopqrstuvwxyz")))

(s/def ::password-policy
  (s/cat
   :range ::range
   :separator ::whitespace
   :letter ::letter))

(s/def ::password (s/+ ::letter))

(s/def ::line
  (s/cat :policy ::password-policy
         :separator ::whitespace
         :passwd ::password))

;; lets drop clojure spec for a while

(def re-line #"(\d+)-(\d+)\s+(\w):\s+(\w+)")

(defn parse-line
  [line]
  (let [[_ min max letter passwd] (re-matches re-line line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :letter (first letter)
     :passwd passwd}))

(deftest parse-line-test
  (is (= {:min 1
          :max 3
          :letter \a
          :passwd "abcde"}
         (parse-line "1-3 a: abcde")))
  (is (= {:min 2
          :max 9
          :letter \c
          :passwd "ccccccccc"}
         (parse-line "2-9 c: ccccccccc"))))

(defn valid-passwd?
  [line]
  (let [{:keys [min max letter passwd]} (parse-line line)
        n (get (frequencies passwd) letter 0)]
    (<= min n max)))

(deftest valid-password?-test
  (is (valid-passwd? "1-3 a: abcdef"))
  (is (not (valid-passwd? "1-3 b: cdefg")))
  (is (valid-passwd? "2-9 c: ccccccccc")))

(defn valid-passwd-2?
  [line]
  (let [{:keys [min max letter passwd]} (parse-line line)
        first-letter (get passwd (- min 1))
        second-letter (get passwd (- max 1))]
    (and (not= first-letter second-letter)
         (or (= letter first-letter)
             (= letter second-letter)))))

(deftest valid-password-2?-test
  (is (valid-passwd-2? "1-3 a: abcdef"))
  (is (not (valid-passwd-2? "1-3 b: cdefg")))
  (is (not (valid-passwd-2? "2-9 c: ccccccccc"))))

(defn input-lines []
  (with-open [rdr (io/reader (io/resource "advent-of-code-2020/day02.in"))]
    (vec (line-seq rdr))))

(deftest solve-test
  (let [lines (input-lines)]
    (is (= 483 (count (filter valid-passwd? lines))))
    (is (= 482 (count (filter valid-passwd-2? lines))))))
