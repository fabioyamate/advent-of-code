(ns advent-of-code-2019.day03
  (:use clojure.test)
  (:require [clojure.java.io :as io]))

;; https://adventofcode.com/2019/day/3

(defn abs [x]
  (if (neg? x) (- x) x))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn points
  [[x y] [direction distance]]
  (for [p (range 1 (inc distance))]
    (case direction
      :up [x (+ y p)]
      :right [(+ x p) y]
      :down [x (- y p)]
      :left [(- x p) y])))

(defn wire
  [position path]
  (loop [position position
         path path
         wire-points []]
    (if (empty? path)
      wire-points
      (let [points (points position (first path))]
        (recur (last points)
               (rest path)
               (into wire-points points))))))

(defn parse-move
  [move-str]
  (let [[_ direction distance] (re-matches #"([LRUD])(\d+)" move-str)]
    (case direction
      "L" [:left (Integer/parseInt distance)]
      "R" [:right (Integer/parseInt distance)]
      "U" [:up (Integer/parseInt distance)]
      "D" [:down (Integer/parseInt distance)])))

(defn read-path
  [input]
  (-> input
      (clojure.string/trim)
      (clojure.string/split #",")
      (->> (map parse-move))))

(defn wires-intersection
  [wires]
  (->> (map set wires)
       (apply clojure.set/intersection)))

(defn min-distance-1
  [paths]
  (let [center-position [0 0]]
    (->> paths
         (map read-path)
         (map (partial wire center-position))
         wires-intersection
         (map (partial manhattan-distance center-position))
         (apply min))))

(deftest level-1
  (is (= 6 (min-distance-1 ["R8,U5,L5,D3"
                            "U7,R6,D4,L4"])))
  (is (= 159 (min-distance-1 ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                              "U62,R66,U55,R34,D71,R55,D58,R83"])))
  (is (= 135 (min-distance-1 ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
"
                              "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]))))

(defn solve-1
  []
  (with-open [rdr (io/reader (io/resource "advent-of-code-2019/day03.in"))]
    (min-distance-1 (line-seq rdr))))

(solve-1)
;; => 446

(defn wire-intersection->steps
  [from to path]
  (reduce
   (fn [distance point]
     (if (= point to)
       (reduced (inc distance))
       (inc distance)))
   0
   (wire from path)))

(defn fewest-combined-steps-to-intersection
  [paths-str]
  (let [paths (map read-path paths-str)]
    (apply min
           (for [point (->> (map (partial wire [0 0]) paths)
                            wires-intersection)]
             (->> (map (partial wire-intersection->steps [0 0] point) paths)
                  (apply +))))))

(defn solve-2
  []
  (with-open [rdr (io/reader (io/resource "advent-of-code-2019/day03.in"))]
    (fewest-combined-steps-to-intersection (line-seq rdr))))

(solve-2)
;; => 9006

(deftest level-2
  (is (= 20 (wire-intersection->steps [0 0] [3 3] (read-path "R8,U5,L5,D3"))))
  (is (= 15 (wire-intersection->steps [0 0] [6 5] (read-path "R8,U5,L5,D3"))))

  (is (= 30 (fewest-combined-steps-to-intersection ["R8,U5,L5,D3"
                                                    "U7,R6,D4,L4"])))
  (is (= 610 (fewest-combined-steps-to-intersection ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                                     "U62,R66,U55,R34,D71,R55,D58,R83"])))
  (is (= 410 (fewest-combined-steps-to-intersection ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
"
                                                     "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]))))
