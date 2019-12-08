(ns advent-of-code-2019.day07
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async
             :refer [<! >! <!! >!! go go-loop thread chan close! to-chan
                     pipeline pipeline-blocking pipeline-async]]))

;; https://adventofcode.com/2019/day/7

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
  [inputs program]
  (loop [pointer 0
         memory program
         inputs inputs
         outputs []]
    (let [[opcode & modes] (parse-opcode (nth memory pointer))]
      #_(println (take 20 memory) [opcode modes])
      (case opcode
        1 (recur (+ pointer 4)
                 (run-instruction + pointer memory modes)
                 inputs
                 outputs)

        2 (recur (+ pointer 4)
                 (run-instruction * pointer memory modes)
                 inputs
                 outputs)

        3 (recur (+ pointer 2)
                 (store-value memory
                              (+ pointer 1)
                              (first inputs))
                 (rest inputs)
                 outputs)

        4 (recur (+ pointer 2)
                 memory
                 inputs
                 (conj outputs (read-value memory (+ pointer 1) (nth modes 0))))

        5 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
            (if (not (zero? test-value))
              (recur (read-value memory (+ pointer 2) (nth modes 1))
                     memory
                     inputs
                     outputs)
              (recur (+ pointer 3) memory inputs outputs)))

        6 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
            (if (zero? test-value)
              (recur (read-value memory (+ pointer 2) (nth modes 1))
                     memory
                     inputs
                     outputs)
              (recur (+ pointer 3) memory inputs outputs)))

        7 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
            (recur (+ pointer 4)
                   (store-value memory
                                (+ pointer 3)
                                (if (< test-value1 test-value2) 1 0))
                   inputs
                   outputs))

        8 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
            (recur (+ pointer 4)
                   (store-value memory
                                (+ pointer 3)
                                (if (= test-value1 test-value2) 1 0))
                   inputs
                   outputs))

        99 {:outputs outputs
            :memory (clojure.string/join "," memory)}))))

(defn read-code
  [code]
  (->> (clojure.string/split code #",")
       (mapv #(Integer/parseInt %))))

(defn read-program
  [program]
  (-> (slurp program)
      (clojure.string/trim)
      read-code))

(defn amplifiers->trusters
  [signal settings program]
  (reduce
   (fn [signal setting]
     (first (:outputs (run [setting signal] program))))
   signal
   settings))

(defn eval-intcode
  [inputs code]
  (run inputs (read-code code)))

(deftest level-1
  (let [program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"]
    (is (= [4]
           (:outputs (eval-intcode [4 0] program))))
    (is (= [43]
           (:outputs (eval-intcode [3 4] program))))
    (is (= [432]
           (:outputs (eval-intcode [2 43] program))))
    (is (= [4321]
           (:outputs (eval-intcode [1 432] program))))
    (is (= [43210]
           (:outputs (eval-intcode [0 4321] program)))))

  (deftest amplifiers-settings
    (is (= 54321
           (amplifiers->trusters 0 [0 1 2 3 4] (read-code "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))))
    (is (= 65210
           (amplifiers->trusters 0 [1 0 4 3 2]
                                 (read-code "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))))))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn optimize
  [initial-signal program]
  (reduce
   max
   (for [settings (permutations (range 5))]
     (amplifiers->trusters initial-signal settings program))))

(defn load-and-execute-1
  [initial-signal code]
  (let [program (read-program code)]
    (optimize initial-signal program)))

(comment
  (load-and-execute-1 0 (io/resource "advent-of-code-2019/day07.in"))
  ;; => 422858
  )

(defn run-feedback
  [input-chan program]
  (let [output-chan (async/chan)]
    (go-loop [pointer 0
              memory program]
      (let [[opcode & modes] (parse-opcode (nth memory pointer))]
        #_(println (take 20 memory) [opcode modes])
        (case opcode
          1 (recur (+ pointer 4)
                   (run-instruction + pointer memory modes))

          2 (recur (+ pointer 4)
                   (run-instruction * pointer memory modes))

          3 (recur (+ pointer 2)
                   (store-value memory
                                (+ pointer 1)
                                (<! input-chan)))

          4 (do (>! output-chan (read-value memory (+ pointer 1) (nth modes 0)))
                (recur (+ pointer 2)
                       memory))

          5 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
              (if (not (zero? test-value))
                (recur (read-value memory (+ pointer 2) (nth modes 1))
                       memory)
                (recur (+ pointer 3) memory)))

          6 (let [test-value (read-value memory (+ pointer 1) (nth modes 0))]
              (if (zero? test-value)
                (recur (read-value memory (+ pointer 2) (nth modes 1))
                       memory)
                (recur (+ pointer 3) memory)))

          7 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                  test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
              (recur (+ pointer 4)
                     (store-value memory
                                  (+ pointer 3)
                                  (if (< test-value1 test-value2) 1 0))))

          8 (let [test-value1 (read-value memory (+ pointer 1) (nth modes 0))
                  test-value2 (read-value memory (+ pointer 2) (nth modes 1))]
              (recur (+ pointer 4)
                     (store-value memory
                                  (+ pointer 3)
                                  (if (= test-value1 test-value2) 1 0))))

          99 (async/close! output-chan))))
    output-chan))

#_(amplifiers->trusters 0 [9 8 7 6 5] (read-code "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))

(def output (atom []))

(defn printer
  [in]
  (go (while true (swap! output conj (<! in)))))

(defn feedback-amplifiers->trusters
  [program initial-signal settings]
  (let [input-chan (async/chan)
        truster-chan (async/chan)]
    (let [signal-read (async/mult
                       (reduce
                        (fn [ch setting]
                          (let [output-ch (run-feedback ch program)]
                            (async/put! ch setting)
                            output-ch))
                        input-chan
                        settings))]

      (async/put! input-chan initial-signal)

      (async/tap signal-read input-chan)
      (async/tap signal-read truster-chan)

      (printer truster-chan)
      #_(async/<!! (async/reduce max 0 truster-chan)))
    ))

#_(feedback-amplifiers->trusters
   (read-code "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
   [9 8 7 6 5])

(defn optimize-feedback
  [initial-signal program]
  (for [settings (permutations (range 5 10))]
    (feedback-amplifiers->trusters program initial-signal settings)))

(defn load-and-execute-2
  [initial-signal code]
  (reset! output [])
  (let [program (read-program code)]
    (optimize-feedback initial-signal program)))

(comment
  (load-and-execute-2 0 (io/resource "advent-of-code-2019/day07.in"))

  (reduce max @output)
;; => 14897241
  )



#_(defn feedback-amplifiers->trusters
    [initial-signal settings program]
    (let [input-chan (async/chan)])
    (reduce
     (fn [{:keys [signal program]} setting]
       (let [{:keys [outputs memory]} (run [setting signal] program)]
         {:signal (last outputs)
          :program memory}))
     {:signal signal
      :program program}
     settings))
