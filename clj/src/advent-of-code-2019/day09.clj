(ns advent-of-code-2019.day09
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async
             :refer [<! >! <!! >!! go go-loop thread chan close! to-chan
                     pipeline pipeline-blocking pipeline-async]]))

(deftest mult-chan-test
  (let [chan1 (async/chan)
        chan2 (async/chan)
        chan3 (async/chan)

        m-ch (async/mult chan1)]

    (async/tap m-ch chan2)
    (async/tap m-ch chan3)

    (is (async/put! chan1 1))

    (is (= 1 (<!! chan2)))
    (is (= 1 (<!! chan3)))

    (is (nil? (async/close! chan1)))

    (is (not (async/put! chan1 2)))

    (is (nil? (<!! chan2)))
    (is (nil? (<!! chan3)))))

(deftest mult-chan-feedback-test
  ;; in ---- f --out-- (mult/tap) -- out
  ;;      ^                |
  ;;      |  pipe feedback |
  ;;      +----------------+
  (let [in (async/chan)
        out (async/chan)
        feedback (async/chan)

        m-ch (async/mult chan1)

        f (fn [in]
            (let [out (async/chan)]
              (go-loop []
                (let [v (<! in)]
                  (if (> v 10)
                    (async/close! out)
                    (do (>! out (inc v))
                        (recur)))))
              out))

        f-out (async/mult (f in))]

    (is (async/tap f-out feedback))
    (is (async/tap f-out out))

    (async/pipe feedback in)

    (is (async/put! in 0))

    (is (= (reduce + (range 12))
           (<!! (async/reduce + 0 out))))
    ))


;; https://adventofcode.com/2019/day/9

(defn exp-int
  [base pow]
  (cond (zero? pow)
        1

        (= 1 pow)
        base

        :else
        (recur (* base base) (dec pow))))

(defn digit
  [n index]
  (-> n
      (quot (exp-int 10 index))
      (mod 10)))

(deftest digit-number-test
  (let [index 1
        number 120]
    (is (= 2 (digit 120 1)))
    (is (= 0 (digit 2 1)))))

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

(defn mode
  ;; flag 0 based
  [parameters flag]
  (case (digit parameters flag)
    0 :positional
    1 :immediate
    2 :relative))

(defn parse-opcode
  [input]
  (let [opcode (mod input 100)
        parameters (quot input 100)]
    (case opcode
      1 [opcode
         (mode parameters 0)
         (mode parameters 1)
         (mode parameters 2)]
      2 [opcode
         (mode parameters 0)
         (mode parameters 1)
         (mode parameters 2)]
      3 [opcode
         (mode parameters 0)]
      4  [opcode
          (mode parameters 0)]
      5  [opcode
          (mode parameters 0)
          (mode parameters 1)]
      6  [opcode
          (mode parameters 0)
          (mode parameters 1)]
      7  [opcode
          (mode parameters 0)
          (mode parameters 1)
          (mode parameters 0)]
      8  [opcode
          (mode parameters 0)
          (mode parameters 1)
          (mode parameters 2)]
      99 [opcode])))

(deftest test-parse-opcode
  (is (= [1 :positional :positional :positional]
         (parse-opcode 1)))
  (is (= [1 :positional :immediate :positional]
         (parse-opcode 1001)))
  (is (= [1 :positional :immediate :relative]
         (parse-opcode 21001)))
  (is (= [3 :immediate]
         (parse-opcode 103)))
  (is (= [4 :positional]
         (parse-opcode 4)))
  (is (= [4 :immediate]
         (parse-opcode 104)))
  (is (= [99]
         (parse-opcode 99))))

(defn run
  ([input-chan program]
   (run input-chan program false))
  ([input-chan program debug?]
   (let [output-chan (async/chan)]
     (go-loop [registers {:pointer 0}
               memory program]
       (let [{:keys [pointer]} registers
             [opcode & modes] (parse-opcode (nth memory pointer))]
         (when debug?
           (println (take 20 memory) [opcode modes]))
         (case opcode
           1 (recur (update registers :pointer + 4)
                    (run-instruction + pointer memory modes))

           2 (recur (update registers :pointer + 4)
                    (run-instruction * pointer memory modes))

           3 (recur (update registers :pointer + 2)
                    (store-value memory
                                 (+ pointer 1)
                                 (<! input-chan)))

           4 (do (>! output-chan (read-value memory (+ pointer 1) (nth modes 0)))
                 (recur (update registers :pointer + 2)
                        memory))

           5 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
               (if (not (zero? test-value))
                 (recur (update registers :pointer (fn [pointer]
                                                     (read-value memory (+ pointer 2) (nth modes 1))))
                        memory)
                 (recur (update registers :pointer + 3)
                        memory)))

           6 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
               (if (zero? test-value)
                 (recur (update registers :pointer (fn [pointer]
                                                     (read-value memory (+ pointer 2) (nth modes 1))))
                        memory)
                 (recur (update registers :pointer + 3)
                        memory)))

           7 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                   test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
               (recur (update registers :pointer + 4)
                      (store-value memory
                                   (+ pointer 3)
                                   (if (< test-value1 test-value2) 1 0))))

           8 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                   test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
               (recur (update registers :pointer + 4)
                      (store-value memory
                                   (+ pointer 3)
                                   (if (= test-value1 test-value2) 1 0))))

           99 (async/close! output-chan))))
     output-chan)))

(defn read-code
  [code]
  (->> (clojure.string/split code #",")
       (mapv #(Integer/parseInt %))))

(defn read-program
  [program]
  (-> (slurp program)
      (clojure.string/trim)
      read-code))

(defn eval-intcode
  [in code]
  (run in (read-code code)))

(deftest run-channel-test
  (let [program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        run-example (fn [program & args]
                      (let [in (async/chan)
                            out (eval-intcode in program)]
                        (doseq [arg args]
                          (async/put! in arg))
                        (<!! out)))]

    (is (= 4 (run-example program 4 0)))
    (is (= 43 (run-example program 3 4)))
    (is (= 432 (run-example program 2 43)))
    (is (= 4321 (run-example program 1 432)))
    (is (= 43210 (run-example program 0 4321)))))

(defn init-amplifiers
  [code settings]
  (let [program (read-program code)
        in (async/chan)
        out (reduce
             (fn [signal setting]
               (async/put! signal setting)
               (run signal program))
             in
             settings)]
    {:in in
     :out out}))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn load-and-execute-1
  [code]
  (reduce max
          (for [settings (permutations (range 5))
                :let [{:keys [in out]} (init-amplifiers code settings)]]
            (do (async/put! in 0)
                (<!! out)))))

(deftest level-1
  (is (= 422858 (load-and-execute-1 (io/resource "advent-of-code-2019/day07.in")))))
