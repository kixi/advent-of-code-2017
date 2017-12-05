(ns advent-of-code.day5
  (:require [clojure.test :refer [is deftest]]
            [clojure.string :as str]))


(def maze [0 3 0 1 -3])

(defn jump [[maze pos]]
  (let [v (nth maze pos )]
    [(if (>= v 3)
       (update maze pos dec)
       (update maze pos inc))
     (+ v pos)]))

(defn in-boundaries [[maze pos]]
  (and (>= pos 0) (< pos (count maze))))

(defn find-out [maze]
  (loop [count 0
         maze-pos [maze 0]]
    (if (in-boundaries maze-pos)
      (recur (inc count) (jump maze-pos))
      count)
    ))


(find-out maze)
(def inp-maze (read-string (str "["
                              (slurp "src/advent_of_code/input-day5.txt")
                              "]")))

(find-out inp-maze)
