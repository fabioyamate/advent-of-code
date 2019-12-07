(ns advent-of-code-2019.day02
  (:use clojure.test)
  (:require [clojure.java.io :as io]))

;; https://adventofcode.com/2019/day/2

(defn address
  [memory pointer]
  (get memory pointer))

(defn read-value
  [memory pointer]
  (nth memory (address memory pointer)))

(defn store-value
  [memory pointer value]
  (assoc memory (address memory pointer) value))

(defn run-opcode
  [f pointer memory]
  (store-value memory (+ pointer 3)
               (f (read-value memory (+ pointer 1))
                  (read-value memory (+ pointer 2)))))

(defn run
  [program]
  (loop [pointer 0
         memory program]
    (let [opcode (nth memory pointer)]
      (case opcode
        1 (recur (+ pointer 4) (run-opcode + pointer memory))
        2 (recur (+ pointer 4) (run-opcode * pointer memory))
        99 memory))))

(defn eval-intcode
  [code]
  (->> (clojure.string/split code #",")
       (mapv #(Integer/parseInt %))
       run
       (clojure.string/join ",")))

(deftest intcode-1
  (is (= "2,0,0,0,99" (eval-intcode "1,0,0,0,99")))
  (is (= "2,3,0,6,99" (eval-intcode "2,3,0,3,99")))
  (is (= "2,4,4,5,99,9801" (eval-intcode "2,4,4,5,99,0")))
  (is (= "30,1,1,4,2,5,6,0,99" (eval-intcode "1,1,1,4,99,5,6,0,99"))))

(defn restore-gravity
  [input noun verb]
  (-> input
      (assoc 1 noun)
      (assoc 2 verb)))

(defn read-program
  [program]
  (-> (slurp program)
      (clojure.string/trim)
      (clojure.string/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(defn load-and-execute-1
  [program]
  (-> (read-program program)
      (restore-gravity 12 2)
      run
      first))

(load-and-execute-1 (io/resource "advent-of-code-2019/day02.in"))
;; => 5098658

(def counter (atom 0))
(defn solve-2
  [program]
  ;; since noun and verb are the args of the first opcode
  ;; they are limited to the program size pointers
  ;; 0 <= noun, verb < (len program)
  ;;
  ;; So the complexity is n^2
  (let [data (read-program program)
        size (count data)
        [noun verb] (first
                     (for [noun (range size)
                           verb (range size)
                           :when (= 19690720
                                    (do (swap! counter inc)
                                        (-> data
                                            (restore-gravity noun verb)
                                            run
                                            first)))]
                       [noun verb]))]
    (+ (* noun 100) verb)))

(comment
  (solve-2 (io/resource "advent-of-code-2019/day02.in"))
  ;; => 5064
  )
