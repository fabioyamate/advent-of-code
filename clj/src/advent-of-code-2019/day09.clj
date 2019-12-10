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

(def debug? (atom false))
(def events (atom []))

(defn exp-int
  [base pow]
  (loop [acc base
         pow pow]
    (cond (zero? pow)
          1

          (= 1 pow)
          acc

          :else
          (recur (*' acc base) (dec pow)))))

(deftest exp-int-test
  (is (= 4 (exp-int 2 2)))
  (is (= 100000 (exp-int 10 5))))

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

(def value nth)

(defn address
  [memory pointer]
  (get memory pointer))

(defn read-value
  [{memory :ram {:keys [pointer relative-base modes]} :registers :as context} offset]
  (case (modes (- offset 1))
    0 (value memory (value memory (+ pointer offset))) ;; positional
    1 (value memory (+ pointer offset)) ;; immediate
    2 (value memory (+ relative-base (+ pointer offset))))) ;; relative

(defn store-value
  [{{:keys [pointer]} :registers memory :ram :as context} offset value]
  (assoc-in context [:ram (address memory (+ pointer offset))] value))

(defn run-instruction
  [context f]
  (store-value context 3
               (f (read-value context 1)
                  (read-value context 2))))

(defn parse-opcode
  [input]
  (let [opcode (mod input 100)
        parameters (quot input 100)]
    [opcode
     [(digit parameters 0)
      (digit parameters 1)
      (digit parameters 2)]]))

(deftest test-parse-opcode
  (is (= [1 [0 0 0]]
         (parse-opcode 1)))
  (is (= [1 [0 1 0]]
         (parse-opcode 1001)))
  (is (= [1 [0 1 2]]
         (parse-opcode 21001)))
  (is (= [3 [1 0 0]]
         (parse-opcode 103)))
  (is (= [4 [0 0 0]]
         (parse-opcode 4)))
  (is (= [4 [1 0 0]]
         (parse-opcode 104)))
  (is (= [99 [0 0 0]]
         (parse-opcode 99))))

(defn instruction-counter
  [context offset]
  (update-in context [:registers :pointer] + offset))

(defn set-relative-base
  [context]
  (update-in context [:registers :relative-base] + (read-value context 1)))

(defn update-register
  [context register f & args]
  (apply update-in context [:registers register] f args))

(defn jump
  [context f]
  (assoc-in context [:registers :pointer] (f context)))

(defn set-instruction
  [{memory :ram {:keys [pointer]} :registers :as context}]
  (let [opcode (value memory pointer)
        [opcode modes] (parse-opcode opcode)
        out' (update context :registers assoc
                     :opcode opcode
                     :modes modes)]
    (swap! events conj out')
    out'))

(defn instruction
  [{memory :ram {:keys [pointer]} :registers}]
  (value memory pointer))

(defn opcode
  [context]
  (get-in context [:registers :opcode]))

(defn run
  [input-chan program]
  (let [output-chan (async/chan)]
    (go-loop [context {:registers {:pointer 0
                                   :relative-base 0
                                   :opcode 0
                                   :modes [0 0 0]}
                       :ram program}]
      (let [context (set-instruction context)]
        (case (opcode context)
          1 (recur (-> context
                       (run-instruction +')
                       (instruction-counter 4)))

          2 (recur (-> context
                       (run-instruction *')
                       (instruction-counter 4)))

          3 (recur (-> context
                       (store-value 1 (<! input-chan))
                       (instruction-counter 2)))

          4 (do (>! output-chan (read-value context 1))
                (instruction-counter context 2))

          5 (let [test-value (read-value context 1)]
              (if (not (zero? test-value))
                (recur (jump context #(read-value % 2)))
                (recur (instruction-counter context 3))))

          6 (let [test-value (read-value context 1)]
              (if (zero? test-value)
                (recur (jump context #(read-value % 2)))
                (recur (instruction-counter 3))))

          7 (let [test-value1 (read-value context 1)
                  test-value2 (read-value context 2)]
              (recur (-> context
                         (store-value 3 (if (< test-value1 test-value2) 1 0))
                         (instruction-counter 4))))

          8 (let [test-value1 (read-value context 1)
                  test-value2 (read-value context 2)]
              (recur (-> context
                         (store-value 3 (if (= test-value1 test-value2) 1 0))
                         (instruction-counter 4))))

          9 (recur (-> context
                       (update-register :relative-base + (read-value context 1))
                       (update-register :pointer + 2)))

          99 (async/close! output-chan))))
    output-chan))

(defn read-code
  [code]
  (->> (clojure.string/split code #",")
       (mapv #(Long/parseLong %))))

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

(deftest day-7-test-case
  (is (= 422858 (load-and-execute-1 (io/resource "advent-of-code-2019/day07.in")))))

(deftest level-1
  #_(let [in (async/chan)]
      (testing "takes no input and produces a copy of itself as output"
        (is (= 1001 (<!! (eval-intcode in "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")))))

      (testing "should output a 16-digit number"
        (is (<= (exp-int 10 15)  (<!! (eval-intcode in "1102,34915192,34915192,7,4,7,99,0")))))

      (testing "should output the large number in the middle"
        (is (= 1125899906842624 (<!! (eval-intcode in "104,1125899906842624,99")))))


      )

  (let [in (async/chan)]
    (async/put! in 1)
    (is (= 1 (<!! (run in (read-program (io/resource "advent-of-code-2019/day09.in"))))))))

(clojure.pprint/pprint @events)

(clojure.pprint/pprint (take 30 (read-program (io/resource "advent-of-code-2019/day09.in"))))
