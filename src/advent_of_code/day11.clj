(ns advent-of-code.day11
  (:require [clojure.string :as str]))



(def input (slurp "src/advent_of_code/input-day11.txt"))


(def directions {"n" [0 1]
                 "s" [0 -1]
                 "ne" [1 0]
                 "sw" [-1 0]
                 "nw" [-1 1]
                 "se" [1 -1]})


(defn parse-input [input]
  (->> (str/split input #",")
       (map str/trim)
       (map directions)))

(defn move [[xa ya] [x y]]
             [(+ xa x) (+ ya y)])

(defn move-many [input-directions]
  (reduce move
          [0 0]
          input-directions))

(defn distance-center [[x y]]
   (max x y))

(distance-center (move-many (parse-input input)))

;; part 2

(defn max-distance [input-directions]
  (->>
   (reductions move
               [0 0]
               input-directions)
   (map distance-center)
   (apply max)))


(max-distance (parse-input input))
