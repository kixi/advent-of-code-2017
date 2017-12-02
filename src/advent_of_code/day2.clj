(ns advent-of-code.day2
  (:require [clojure.test :refer [is deftest]]
            [clojure.string :as str]))

;; input data
(defn read-input-file [filename]
  (->> 
   (slurp filename)
   str/split-lines
   (map #(str "[" % "]"))
   (map read-string)
   ))

(def grid (read-input-file "src/advent_of_code/input.txt"))

;; part 1
(defn diff [vec]
  (let [smallest (reduce (fn [a x] (if (< a x) a x)) vec)
        largest (reduce (fn [a x] (if (> a x) a x)) vec)
        ]
    (- largest smallest)))

;; solution:
(reduce + (map diff grid)) ;; => 39126

;; Part 2
(defn checkrow [xs]
  (->> 
   (for [x xs
         y xs
         :when (and (not= x y) (= (mod x y) 0))]
       (/ x y))
   (filter (comp not nil?))
   (first)))

;; solution
(reduce + (map checkrow grid)) ;; => 258

