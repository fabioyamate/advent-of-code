(ns advent-of-code-2019.day06
  (:use clojure.test)
  (:require
   [clojure.java.io :as io]
   [clojure.zip :as zip]))

(defn prewalk
  [f loc]
  (loop [loc loc]
    (if (zip/end? loc)
      loc
      (recur (zip/next (f loc))))))

(defn build-tree
  ;; super cowboy code...
  [root edges]
  (let [node (:n root)
        child? (fn [[p c]]
                 (when (= node p)
                   {:n c
                    :c []}))
        children (keep child? edges)
        remainings (remove child? edges)]
    (if (empty? children)
      root
      (assoc root :c (map #(build-tree % remainings) children)))))

(defn tree->zipper
  [tree]
  (zip/zipper :n
              :c
              (fn [n c] (assoc n :c c))
              tree))

(defn node-height
  [loc]
  (cond
    (zip/end? loc) loc
    (zip/up loc) (zip/edit loc assoc :h (inc (:h (zip/node (zip/up loc)))))
    :else (zip/edit loc assoc :h 0)))

(defn compute-heights
  [root]
  (->> (tree->zipper root)
       (prewalk node-height)
       zip/root))

(defn parse-edge
  [edge]
  (clojure.string/split edge #"\)"))

(defn solve-1
  [edges]
  (->> edges
       (map parse-edge)
       (build-tree {:n "COM"})
       compute-heights
       (tree-seq :n :c)
       #_(map #(select-keys % [:n :h]))
       (filter :n)
       (map :h)
       (reduce +)))

(deftest level-1
  (is (= 42 (solve-1 ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"]))))

(with-open [rdr (io/reader (io/resource "advent-of-code-2019/day06.in"))]
  (solve-1 (line-seq rdr)))
;; => 110190
