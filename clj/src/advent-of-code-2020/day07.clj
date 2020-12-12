(ns advent-of-code-2020.day07
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.stuartsierra.dependency :as dep]))

;; this problem is basically building a dependency graph
;; and counting its ancestors. I'm lazy into building a graph
;; data structure, so I'll rely on the dependency library that
;; will do all the work for me.

(comment
  (def g1 (-> (dep/graph)
              (dep/depend :b :a)   ; "B depends on A"
              (dep/depend :c :b)   ; "C depends on B"
              (dep/depend :c :a)   ; "C depends on A"
              (dep/depend :d :c))) ; "D depends on C"

  (dep/immediate-dependents g1 :d) ; #{}
  (dep/immediate-dependents g1 :c) ; #{:d}
  (dep/immediate-dependencies g1 :d) ; #{:c}

  (= 3 (count (dep/transitive-dependencies g1 :d))))

;; so the trick here is the regex, because we want to parte
;; and extract specific data and structure them.

(def parent-re #"(.*?) bags contain (.*)")
(def child-re #"(\d+) (\w+ \w+)")

(def line-sample "light red bags contain 1 bright white bag, 2 muted yellow bags")

(def sample
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn build
  [graph line]
  (let [[_ parent children] (re-find parent-re line)]
    (reduce
     (fn [g child]
       (let [[_ c bag] (re-find child-re child)]
         (dep/depend g bag parent)))
     graph
     (str/split children #","))))

(defn build-graph
  [data]
  (reduce build (dep/graph) (str/split-lines data)))

(defn solve-1
  [data bag]
  (count
   (dep/transitive-dependencies
    (build-graph data)
    bag)))

(deftest solve-1-test
  (is (= 4 (solve-1 sample "shiny gold")))
  (is (= 151 (solve-1 (slurp (io/resource "advent-of-code-2020/day07.in")) "shiny gold"))))

;; i'm just brute forcing the solution to compute the result... not thinking too much.
;; the strategy here is different from the first part. Here we are going to start from a
;; given node and traverse its inner bags.

(defn parse-bag
  "return [bag-name {inner-bag-1 count inner-bag-2 count ...}]"
  [line]
  (let [[_ parent children] (re-find parent-re line)]
    (when parent
      [parent (into {}
                    (for [child (str/split children #",")
                          :let [[_ c bag] (re-find child-re child)]
                          :when bag]
                      [bag c]))])))

(defn build-table-2
  "return a table with each bag and its inner bags"
  [data]
  (into {} (map parse-bag (str/split-lines data))))

;; the simple computing here is a recursive solution where:
;; count(bag) = (count inner-bag) + sum(count inner-inner-bag))
(defn count-bags
  [table inner-bags]
  (apply +
         (for [[bag c] inner-bags
               :let [down-count (count-bags table (get table bag))]]
           (* (Integer/parseInt c) (+ 1 down-count)))))

(defn solve-2
  [data bag]
  (let [table (build-table-2 data)]
    (count-bags table (get table bag))))

(deftest solve-2-test
  (is (= 41559 (solve-2 (slurp (io/resource "advent-of-code-2020/day07.in")) "shiny gold")))
  (is (= 32 (solve-2 sample "shiny gold"))))
