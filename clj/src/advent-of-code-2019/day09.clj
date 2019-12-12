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

        m-ch (async/mult in)

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
  [{memory :ram {:keys [pointer relative-base modes]} :registers :as context} offset]
  (doto (case (modes (- offset 1))
          0 (value memory (+ pointer offset))
          1 (+ pointer offset)
          2 (+ relative-base (value memory (+ pointer offset))))
    (->> (println "address:" (modes (- offset 1)) ":"))))

(defn read-value
  [{memory :ram :as context} offset]
  (value memory (address context offset)))

(defn store-value
  [{{:keys [pointer modes]} :registers memory :ram :as context} offset value]
  (assoc-in context [:ram (address context offset)] value))

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

(defn set-relative-base
  [context]
  (update-in context [:registers :relative-base] + (read-value context 1)))

(defn update-register
  [context register f & args]
  (apply update-in context [:registers register] f args))

(defn jump
  [context f]
  (println "jumping to " (f context))
  (assoc-in context [:registers :pointer] (f context)))

(defn set-instruction
  [{memory :ram {:keys [pointer]} :registers :as context}]
  (let [opcode (value memory pointer)
        [opcode modes] (parse-opcode opcode)
        out (update context :registers assoc
                    :opcode opcode
                    :modes modes)]
    (println "registers:" (:registers out))
    (swap! events conj out)
    out))

(defn instruction
  [{memory :ram {:keys [pointer]} :registers}]
  (value memory pointer))

(defn opcode
  [context]
  (get-in context [:registers :opcode]))

(defn zero-filling
  [size mem]
  (let [fill (- size (count mem))]
    (into mem (repeat fill 0))))

(deftest zero-filling-test
  (is (= [1 0 0 0] (zero-filling 4 [1]))))

(defn run
  [input-chan program]
  (let [output-chan (async/chan)]
    (go-loop [context {:registers {:pointer 0
                                   :relative-base 0
                                   :opcode 0
                                   :modes [0 0 0]}
                       :ram (zero-filling (* 16 1024) program)}]
      (let [context (set-instruction context)]
        (case (doto (opcode context) (->> (println "opcode:")))
          1 (recur (-> context
                       (run-instruction +')
                       (update-register :pointer + 4)))

          2 (recur (-> context
                       (run-instruction *')
                       (update-register :pointer + 4)))

          3 (let [v (<! input-chan)]
              (println "input:" v)
              (recur (-> context
                         (store-value 1 v)
                         (update-register :pointer + 2))))

          4 (let [v (read-value context 1)]
              (println "output:" v)
              (>! output-chan v)
              (recur (update-register context :pointer + 2)))

          5 (let [test-value (read-value context 1)]
              (if (not (zero? test-value))
                (recur (jump context #(read-value % 2)))
                (recur (update-register context :pointer + 3))))

          6 (let [test-value (read-value context 1)]
              (if (zero? test-value)
                (recur (jump context #(read-value % 2)))
                (recur (update-register context :pointer + 3))))

          7 (let [test-value1 (read-value context 1)
                  test-value2 (read-value context 2)]
              (recur (-> context
                         (store-value 3 (if (< test-value1 test-value2) 1 0))
                         (update-register :pointer + 4))))

          8 (let [test-value1 (read-value context 1)
                  test-value2 (read-value context 2)]
              (recur (-> context
                         (store-value 3 (if (= test-value1 test-value2) 1 0))
                         (update-register :pointer + 4))))

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

(defn run-example
  [program & args]
  (let [in (async/chan)
        out (eval-intcode in program)]
    (doseq [arg args]
      (async/put! in arg))
    (<!! (async/reduce conj [] out))))

(deftest io-test
  ;; positional
  (is (= [9999] (run-example "3,0,4,0,99" 9999)))
  ;; immediate
  (is (= [9999] (run-example "103,3,4,1,99" 9999)))
  (is (= [9999] (run-example "3,3,104,0,99" 9999)))
  ;; relative
  (is (= [9999] (run-example "109,2,203,0,204,0,99" 9999)))
  )

(deftest intcode-opcode-test
  (testing "opcode 01: addition"
    (is (= [(+ 1 1)] (run-example "1,0,0,0,4,0,99")))
    (is (= [(+ 1 2)] (run-example "1101,1,2,0,4,0,99")))
    (is (= [(+ 2201 2201)] (run-example "2201,0,0,0,4,0,99"))))

  (testing "opcode 02: multiply"
    (is (= [(* 2 2)] (run-example "2,0,0,0,4,0,99")))
    (is (= [(* 1 2)] (run-example "1102,1,2,0,4,0,99")))
    (is (= [(* 2202 2202)] (run-example "2202,0,0,0,4,0,99"))))

  (io-test)

  (testing "opcode 05: jump if not zero"
    (is (= [5] (run-example "5,0,0,0,0,4,0,99")))
    (is (= [105] (run-example "105,1,3,4,4,0,99")))
    (is (= [1105] (run-example "1105,1,3,4,0,99")))
    (is (= [2105] (run-example "2105,1,3,4,4,0,99")))
    (is (= [205] (run-example "205,1,3,4,4,0,99")))
    (is (= [1205] (run-example "1205,1,3,4,0,99")))
    #_(is (= [2205] (run-example "2205,1,3,4,0,99")))
    )

  )

(deftest day-5-test
  (is (= [99] (run-example "1002,4,3,7,4,0,99,33")))

  (testing "is equal to 8"
    ;; positional
    (is (= [1] (run-example "3,9,8,9,10,9,4,9,99,-1,8" 8)))
    (is (= [0] (run-example "3,9,8,9,10,9,4,9,99,-1,8" 9)))
    ;; immediate
    (is (= [1] (run-example "3,3,1108,-1,8,3,4,3,99" 8)))
    (is (= [0] (run-example "3,3,1108,-1,8,3,4,3,99" 9)))
    )

  (testing "is less than 8"
    (is (= [1] (run-example "3,9,7,9,10,9,4,9,99,-1,8" 7)))
    (is (= [0] (run-example "3,9,7,9,10,9,4,9,99,-1,8" 8)))
    (is (= [1] (run-example "3,3,1107,-1,8,3,4,3,99" 7)))
    (is (= [0] (run-example "3,3,1107,-1,8,3,4,3,99" 9))))

  (testing "jump if zero"
    ;; position
    (is (= [0] (run-example "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 0)))
    (is (= [1] (run-example "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" 1)))
    ;; immediate
    (is (= [0] (run-example "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 0)))
    (is (= [1] (run-example "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" 1)))    )

  (testing "testing equality from 8 (999,1000,1001)"
    (let [code "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"]
      (is (= [999] (run-example code 7)))
      (is (= [1000] (run-example code 8)))
      (is (= [1001] (run-example code 9))))))

(deftest relative-test
  (is (is (= 678 (run-example "3,1985,109,2000,109,19,204,-34,99" 678)))))

(deftest run-channel-test
  (let [program "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"]

    (is (= [4] (run-example program 4 0)))
    (is (= [43] (run-example program 3 4)))
    (is (= [432] (run-example program 2 43)))
    (is (= [4321] (run-example program 1 432)))
    (is (= [43210] (run-example program 0 4321)))))

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
                (last (<!! (async/reduce conj [] out)))))))

(deftest day-7-test-case
  (is (= 422858 (load-and-execute-1 (io/resource "advent-of-code-2019/day07.in")))))

(deftest level-1
  (let [in (async/chan)]
    (testing "takes no input and produces a copy of itself as output"
      (let [code "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"]
        (is (= (read-code code)
               (<!! (async/reduce conj [] (eval-intcode in code)))))))

    (testing "should output a 16-digit number"
      (is (<= (exp-int 10 15)  (<!! (eval-intcode in "1102,34915192,34915192,7,4,7,99,0")))))

    (testing "should output the large number in the middle"
      (is (= 1125899906842624 (<!! (eval-intcode in "104,1125899906842624,99")))))))

(deftest foo-test
  (let [in (async/chan)]
    (async/put! in 1)
    (is (= [3241900951] (<!! (async/reduce conj [] (run in (read-program (io/resource "advent-of-code-2019/day09.in")))))))))

#_(deftest level-1
    (let [in (async/chan)]
      (testing "takes no input and produces a copy of itself as output"
        (let [code "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"]
          (is (= (read-code code)
                 (<!! (async/reduce conj [] (eval-intcode in code)))))))))
