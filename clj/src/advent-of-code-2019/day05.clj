(ns advent-of-code-2019.day05
  (:use clojure.test)
  (:require [clojure.java.io :as io]))

;; https://adventofcode.com/2019/day/2

(defn address
  [memory pointer]
  (get memory pointer))

(defn read-value
  [memory pointer mode]
  (if (= :positional mode)
    (nth memory (address memory pointer))
    (nth memory pointer)))

(defn store-value
  [memory pointer value]
  (assoc memory (address memory pointer) value))

(defn run-instruction
  [f pointer memory modes]
  (store-value memory (+ pointer 3)
               (f (read-value memory (+ pointer 1) (nth modes 0))
                  (read-value memory (+ pointer 2) (nth modes 1)))))

;; TODO apply state-monad pattern here instead of imperative code

(defn mode
  [parameters flag]
  (if (pos? (bit-and parameters flag))
    :immediate
    :positional))

(defn parse-opcode
  [input]
  (let [opcode (mod input 100)
        parameters (quot input 100)]
    (case opcode
      1 [opcode
         (mode parameters 2r1)
         (mode parameters 2r10)
         (mode parameters 2r100)]
      2 [opcode
         (mode parameters 2r1)
         (mode parameters 2r10)
         (mode parameters 2r100)]
      3 [opcode
         (mode parameters 2r1)]
      4  [opcode
          (mode parameters 2r1)]
      99 [opcode])))

(deftest test-parse-opcode
  (is (= [1 :positional :positional :positional]
         (parse-opcode 1)))
  (is (= [1 :positional :immediate :positional]
         (parse-opcode 1001)))
  (is (= [2 :positional :immediate :positional]
         (parse-opcode 1002)))
  (is (= [3 :immediate]
         (parse-opcode 103)))
  (is (= [4 :positional]
         (parse-opcode 4)))
  (is (= [4 :immediate]
         (parse-opcode 104)))
  (is (= [99]
         (parse-opcode 99))))

(defn run
  [input program]
  (loop [pointer 0
         memory program
         outputs []]
    (let [[opcode & modes] (parse-opcode (nth memory pointer))]
      (println (take 20 memory) [opcode modes])
      (case opcode
        1 (recur (+ pointer 4)
                 (run-instruction + pointer memory modes)
                 outputs)
        2 (recur (+ pointer 4)
                 (run-instruction * pointer memory modes)
                 outputs)
        3 (recur (+ pointer 2)
                 (store-value memory
                              (+ pointer 1)
                              input)
                 outputs)
        4 (recur (+ pointer 2)
                 memory
                 (conj outputs (read-value memory (+ pointer 1) (nth modes 0))))
        99 {:outputs outputs
            :memory (clojure.string/join "," memory)}))))

(defn eval-intcode
  [input code]
  (->> (clojure.string/split code #",")
       (mapv #(Integer/parseInt %))
       (run input)))

(deftest level-1
  (is (= {:outputs [0]
          :memory "104,0,99"}
         (eval-intcode 1 "104,0,99")))
  (is (= {:outputs [1]
          :memory "1,0,4,0,99"}
         (eval-intcode 1 "3,0,4,0,99")))
  (is (= {:outputs []
          :memory "1002,4,3,4,99"}
         (eval-intcode 1 "1002,4,3,4,33")))
  (is (= {:outputs []
          :memory "1101,100,-1,4,99"}
         (eval-intcode 1 "1101,100,-1,4,0"))))

(defn read-program
  [program]
  (-> (slurp program)
      (clojure.string/trim)
      (clojure.string/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defn load-and-execute-1
  [input program]
  (let [outputs (:outputs (run 1 (read-program program)))]
    [(every? zero? (butlast outputs))
     (last outputs)]))

(load-and-execute-1 1 (io/resource "advent-of-code-2019/day05.in"))
;; => [true 6069343]
