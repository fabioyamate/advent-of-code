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
      5  [opcode
          (mode parameters 2r1)
          (mode parameters 2r10)]
      6  [opcode
          (mode parameters 2r1)
          (mode parameters 2r10)]
      7  [opcode
          (mode parameters 2r1)
          (mode parameters 2r10)
          (mode parameters 2r100)]
      8  [opcode
          (mode parameters 2r1)
          (mode parameters 2r10)
          (mode parameters 2r100)]
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

        5 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
            (if (not (zero? test-value))
              (recur (read-value memory (+ pointer 2) (nth modes 1))
                     memory
                     outputs)
              (recur (+ pointer 3) memory outputs)))

        6 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
            (if (zero? test-value)
              (recur (read-value memory (+ pointer 2) (nth modes 1))
                     memory
                     outputs)
              (recur (+ pointer 3) memory outputs)))

        7 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
            (recur (+ pointer 4)
                   (store-value memory
                                (+ pointer 3)
                                (if (< test-value1 test-value2) 1 0))
                   outputs))

        8 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
            (recur (+ pointer 4)
                   (store-value memory
                                (+ pointer 3)
                                (if (= test-value1 test-value2) 1 0))
                   outputs))

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
  (let [outputs (:outputs (run input (read-program program)))]
    [(every? zero? (butlast outputs))
     (last outputs)]))

(load-and-execute-1 1 (io/resource "advent-of-code-2019/day05.in"))
;; => [true 6069343]

(deftest level-2
  (testing "position-mode"
    (is (= {:outputs [1]
            :memory "3,9,8,9,10,9,4,9,99,1,8"}
           (eval-intcode 8 "3,9,8,9,10,9,4,9,99,-1,8")))
    (is (= {:outputs [0]
            :memory "3,9,7,9,10,9,4,9,99,0,8"}
           (eval-intcode 8 "3,9,7,9,10,9,4,9,99,-1,8"))))
  (testing "immediate-mode"
    (is (= {:outputs [1]
            :memory "3,3,1108,1,8,3,4,3,99"}
           (eval-intcode 8 "3,3,1108,-1,8,3,4,3,99")))
    (is (= {:outputs [0]
            :memory "3,3,1107,0,8,3,4,3,99"}
           (eval-intcode 8 "3,3,1107,-1,8,3,4,3,99"))))

  (testing "jump-tests"
    (is (= {:outputs [1]
            :memory "3,12,6,12,15,1,13,14,13,4,13,99,1,1,1,9"}
           (eval-intcode 1 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")))
    (is (= {:outputs [1]
            :memory "3,3,1105,1,9,1101,0,0,12,4,12,99,1"}
           (eval-intcode 1 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"))))

  (testing "larger-test"
    (let [test-program "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
      (is (= [999]
             (:outputs (eval-intcode 7 test-program))))
      (is (= [1000]
             (:outputs (eval-intcode 8 test-program))))
      (is (= [1001]
             (:outputs (eval-intcode 9 test-program)))))))

(defn load-and-execute-2
  [input program]
  (let [outputs (:outputs (run input (read-program program)))]
    [(every? zero? (butlast outputs))
     (last outputs)]))

(load-and-execute-2 5 (io/resource "advent-of-code-2019/day05.in"))
;; => [true 3188550]
