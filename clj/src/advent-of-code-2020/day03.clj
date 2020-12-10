(ns advent-of-code-2020.day03
  (:use clojure.test)
  (:require [clojure.java.io :as io]))

(def open \.)
(def tree \#)

(def data
  {:forest []
   :pos-x 0
   :pos-y 0
   :rows 10
   :cols 10})

(defn move-right
  [{:keys [pos-x cols] :as state} step]
  (update state :pos-x #(mod (+ % step) cols)))

(defn move-down
  [{:keys [pos-x rows] :as state} step]
  (update state :pos-y + step))

(deftest move-right-test
  (is (= 0 (:pos-x (move-right data 0))))
  (is (= 0 (:pos-x (move-right data 10))))
  (is (= 3 (:pos-x (move-right data 3))))
  (is (= 3 (:pos-x (move-right data 13)))))

(deftest move-down-test
  (is (= 0 (:pos-y (move-down data 0))))
  (is (= 10 (:pos-y (move-down data 10))))
  (is (= 3 (:pos-y (move-down data 3))))
  (is (= 13 (:pos-y (move-down data 13)))))

(defn walk [right down]
  (fn [state]
    (-> state
        (move-right right)
        (move-down down))))

(defn read-forest
  [data]
  (->> data
       (clojure.string/split-lines)
       (mapv vec)))

(defn parse-input
  [data]
  (let [forest (read-forest data)]
    {:forest forest
     :pos-x 0
     :pos-y 0
     :rows (count (first forest))
     :cols (count forest)
     :trees-encountered 0}))

(defn found-tree?
  [{:keys [forest pos-x pos-y]}]
  (= tree (get-in forest [pos-y pos-x])))

(defn bottom?
  [{:keys [pos-y cols]}]
  (>= pos-y cols))

(defn solve-1
  [data step]
  (loop [state (parse-input data)]
    (let [state' (step state)
          state'' (if (found-tree? state')
                    (update state' :trees-encountered inc)
                    state')]
      (if (bottom? state'')
        state''
        (recur state'')))))

(def sample
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(deftest solve-1-test
  (is (= 7 (:trees-encountered (solve-1 sample (walk 3 1)))))
  (is (= 250 (:trees-encountered (solve-1 (slurp (io/resource "advent-of-code-2020/day03.in")) (walk 3 1))))))


;; the first solution is basically simulating the navigation
;; however we just want to find the fn that computes the line and move the position

(defn x [r cols step]
  (mod (* step r) cols))

(defn encountered-trees
  [founds]
  (count (filter #{tree} founds)))

(defn solve-1a
  [data {:keys [right down]}]
  (let [forest (read-forest data)
        n-rows (count forest)
        n-cols (count (first forest))]
    (encountered-trees
     (map-indexed (fn [i r]
                    (get-in forest [r (mod (* right i) n-cols)]))
                  (range 0 n-rows down)))))

(deftest solve-1a-test
  (is (= 7 (solve-1a sample {:right 3 :down 1})))
  (is (= 250 (solve-1a (slurp (io/resource "advent-of-code-2020/day03.in"))
                       {:right 3 :down 1}))))

(defn solve-2a
  [data slopes]
  (apply *
         (for [slope slopes]
           (solve-1a data slope))))

(deftest solve-2a-test
  (let [slopes [{:right 1 :down 1}
                {:right 3 :down 1}
                {:right 5 :down 1}
                {:right 7 :down 1}
                {:right 1 :down 2}]]
    (is (= [2 7 3 4 2] (map #(solve-1a sample %) slopes)))
    (is (= 336 (solve-2a sample slopes)))

    (is (= 1592662500 (solve-2a (slurp (io/resource "advent-of-code-2020/day03.in")) slopes)))))
