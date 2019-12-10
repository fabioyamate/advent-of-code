(ns advent-of-code-2019.day06
  (:use clojure.test)
  (:require
   [clojure.java.io :as io]
   [clojure.zip :as zip]
   [clojure.set :as set]))

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
       (filter :n)
       (map :h)
       (reduce +)))

(deftest level-1
  (is (= 42 (solve-1 ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"]))))

(with-open [rdr (io/reader (io/resource "advent-of-code-2019/day06.in"))]
  (solve-1 (line-seq rdr)))
;; => 110190

(defn find-node
  [node loc]
  (loop [loc loc]
    (if (= node (:n (zip/node loc)))
      loc
      (recur (zip/next loc)))))

(defn abs [x] (if (neg? x) (- x) x))

(defn solve-2
  [edges node-1 node-2]
  (let [zp (->> edges
                (map parse-edge)
                (build-tree {:n "COM"})
                compute-heights
                tree->zipper)
        node-a (find-node node-1 zp)
        height-a (:h (zip/node node-a))
        node-a-path (path-nodes node-a)
        node-b (find-node node-2 zp)
        height-b (:h (zip/node node-b))
        node-b-path (path-nodes node-b)
        deepest-common-node-height (->> (set/intersection (set node-a-path)
                                                          (set node-b-path))
                                        (sort-by :h)
                                        last
                                        :h)]
    (- (+ (abs (- height-a deepest-common-node-height))
          (abs (- height-b deepest-common-node-height)))
       2))) ;; the nodes itself doesn't count, it wanna orbit

(defn path-nodes
  [loc]
  (->> (zip/path loc)
       (map #(select-keys % [:n :h]))))

(deftest level-2
  (let [edges ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"]]
    (is (= 0 (solve-2 edges "F" "J")))
    (is (= 2 (solve-2 edges "K" "I"))))

  (let [edges ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"]]
    (is (= 4 (solve-2 edges "YOU" "SAN")))))

(with-open [rdr (io/reader (io/resource "advent-of-code-2019/day06.in"))]
  (solve-2 (line-seq rdr) "YOU" "SAN"))
;; => 343
