(ns advent-of-code.day22
  (:require [clojure.string :as str]))


(def input "..#
#..
...")

(def input (slurp "src/advent_of_code/input-day22.txt"))

(defn parse-input []
  (let [offset (quot (count (str/split-lines input)) 2)]
    (->>
     input
     str/split-lines
     (map-indexed (fn [row line]
                    (map-indexed (fn [col v]
                                   (if (= \# v)
                                     [[(- col offset) (- offset row)] true]))
                                 line)))
     (apply concat)
     (filter identity)
     (into {})
     )))

(defmulti turn (fn [turn-dir dir] turn-dir))
(defmethod turn :right [_ [x y]] [y (- 0 x)])
(defmethod turn :left  [_ [x y]] [(- 0 y) x])

(defn turn-and-move [carrier direction]
  (let [nd (turn direction (:dir carrier))]
    (->
     carrier
     (assoc :dir nd)
     (assoc :pos (mapv + (:pos carrier) nd))))
  )

(turn-and-move {:pos [0 0] :dir [0 1]} :right)
(defn step [[grid carrier infections]]
  (if (grid (:pos carrier))
    [(dissoc grid (:pos carrier)) (turn-and-move carrier :right) infections]
    [(assoc grid (:pos carrier) true) (turn-and-move carrier :left) (inc infections)]
    ))

(last (first (drop 10000 (iterate step [(parse-input)
                                        {:pos [0 0] :dir [0 1]}
                                        0]))))

