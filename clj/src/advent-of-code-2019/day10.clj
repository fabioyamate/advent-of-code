(ns advent-of-code-2019.day10
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn locate-asteroids
  [input]
  (set
   (for [[y row] (map-indexed vector input)
         [x col] (map-indexed vector row)
         :when (= \# col)]
     [x y])))

(deftest locate-asteroids-test
  (is (= [] (locate-asteroids [])))
  (is (= [] (locate-asteroids ["."])))
  (is (= [[0 0]] (locate-asteroids ["#"])))
  (is (= [[2 0]] (locate-asteroids ["..#"])))
  (is (= [[1 0] [1 1]] (locate-asteroids [".#" ".#"]))))

(defn vector-xy
  [[x1 y1] [x2 y2]]
  [(- x1 x2)
   (- y1 y2)])

(defn vector-direction
  [[x y]]
  (cond (zero? x)
        (if (neg? y) [0 -1] [0 1])

        (zero? y)
        (if (neg? x) [-1 0] [1 0])

        :else
        (if (neg? x)
          [1 (/ y x)]
          [-1 (/ y x)])))

(defn line-sight
  ;; this is a vector problem, if asteroids have the same vector
  [x ys]
  (reduce (fn [vectors y]
            (conj vectors (vector-direction (vector-xy x y))))
          #{}
          ys))

(defn compute-detections
  [space-map]
  (let [asteroids (locate-asteroids space-map)]
    (map (fn [x]
           [x (count (line-sight x (disj asteroids x)))])
         asteroids)))

(defn highest-detection
  [space-map]
  (->> (compute-detections space-map)
       (sort-by second)
       last
       last))

(defn best-location
  [space-map]
  (->> (compute-detections space-map)
       (sort-by second)
       last
       first))

(deftest best-location-test
  (is (= [3 4] (best-location [".#..#"
                               "....."
                               "#####"
                               "....#"
                               "...##"])))
  (is (= [5 8] (best-location (clojure.string/split-lines "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")))))

(deftest level-1
  (is (= 253 (-> (io/resource "advent-of-code-2019/day10.in")
                 slurp
                 clojure.string/split-lines
                 highest-detection))))

(defn level-2-whatever
  [space-map]
  (let [asteroids (locate-asteroids space-map)]
    (for [x asteroids
          y (disj asteroids x)
          :let [v (vector-xy x y)]]
      {:direction (vector-direction v)
       :vector v
       :coordinate x})))

(->> (level-2-whatever [".#..#"
                        "....."
                        "#####"
                        "....#"
                        "...##"])
     (group-by :direction)
     (map (fn [[x vs]]
            [x (sort-by :vector vs)]))
     (into {}))
