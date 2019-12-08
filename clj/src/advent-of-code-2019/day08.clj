(ns advent-of-code-2019.day08
  (:use clojure.test)
  (:require
   [clojure.java.io :as io]))

(defn f [w h input]
  (->> (partition-all (* w h) input)
       (map #(map str %))
       (map (fn [pixels]
              (merge {"0" 0 "1" 0 "2" 0}
                     (select-keys (frequencies pixels)
                                  ["0" "1" "2"]))))))



(defn solve-1
  []
  (let [res (reduce (fn [x y]
                      (if (> (get y "0")
                             (get x "0"))
                        x
                        y))
                    {"0" Integer/MAX_VALUE "1" 0 "2" 0}
                    (filter #(pos? (get % "0")) (f 25 6 (slurp (io/resource "advent-of-code-2019/day08.in")))))]
    (println res)
    (* (get res "1")
       (get res "2"))))

(solve-1)
;; => 1463


(defn g [w h]
  (->> (partition-all (* w h) (clojure.string/trim (slurp (io/resource "advent-of-code-2019/day08.in"))))
       (apply map (fn [& pixels]
                    (str (or (first (drop-while #(= \2 %) pixels)) " "))))
       (partition-all w)))

(doseq [line (map (fn [row]
                    (apply str
                           (map (fn [p]
                                  (case p
                                    "0" " "
                                    "2" " "
                                    "1" "#"
                                    p)) row)))
                  (g 25 6))]
  (println line))
