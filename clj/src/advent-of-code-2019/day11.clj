(ns advent-of-code-2019.day11
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [advent-of-code-2019.day09 :as intcode]
            [clojure.core.async :as async :refer [<!!]]))

(def state
  {:vector [0 1]
   :position [0 0]
   :path []
   :grid {}})

(defn color-paint
  [color]
  (if (zero? color) "." "#"))

(defn rotate
  [[x y] direction]
  (if (zero? direction)
    [(- y) x]
    [y x]))

(defn walk
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn move
  [{:keys [vector position]} color-value direction]
  (let [color (color-paint color-value)
        new-vector (rotate vector direction)
        new-position (walk position new-vector)]
    (-> (assoc state
               :position new-position
               :vector new-vector)
        (update :grid assoc position color)
        (update :path conj {:color color
                            :position position}))))

(deftest move-test
  (is (= {:vector [-1 0]
          :position [-1 0]
          :path [{:color ".", :position [0 0]}]
          :grid {[0 0] "."}}
         (move state 0 0)))
  (is (= {:vector [1 0]
          :position [1 0]
          :path [{:color ".", :position [0 0]}]
          :grid {[0 0] "."}}
         (move state 0 1))))

(deftest level-1
  (is (= 1 (let [in (async/chan)
                 out (intcode/run in (intcode/read-program (io/resource "advent-of-code-2019/day09.in")))]
             (loop [state {:vector [0 1]
                           :position [0 0]
                           :path []
                           :grid {}}]
               (let [new-value (get (:grid state) (:position state) 0)]
                 (println new-value)
                 (async/put! in new-value)
                 (if-some [color (<!! out)]
                   (let [direction (<!! out)]
                     (println "move" [color direction])
                     (recur (move state color direction)))
                   state)))))))
