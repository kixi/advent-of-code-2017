(ns advent-of-code.day22b
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
                                     [[(- col offset) (- offset row)] :infected]))
                                 line)))
     (apply concat)
     (filter identity)
     (into {})
     )))

(defmulti turn (fn [turn-dir dir] turn-dir))
(defmethod turn :right [_ [x y]] [y (- 0 x)])
(defmethod turn :left  [_ [x y]] [(- 0 y) x])
(defmethod turn :reverse [_ [x y]] [(- 0 x) (- 0 y)])
(defmethod turn :continue [_ [x y]] [x y])

(defn turn-and-move [carrier direction]
  (let [nd (turn direction (:dir carrier))]
    (->
     carrier
     (assoc :dir nd)
     (assoc :pos (mapv + (:pos carrier) nd)))))

(def find-move
  {:clean :left
   :weakend :continue
   :infected :right
   :flagged :reverse})

(def next-state
  {:clean :weakend
   :weakend :infected
   :infected :flagged
   :flagged :clean})

(turn-and-move {:pos [0 0] :dir [0 1]} :right)
(defn step [[grid carrier infections]]
  (let [state (grid (:pos carrier) :clean)
        new-state (next-state state)]
    (if (= :clean new-state)
      [(dissoc grid (:pos carrier))
       (turn-and-move carrier (find-move state))
       infections]
      [(assoc grid (:pos carrier) new-state)
       (turn-and-move carrier (find-move state))
       (if (= :infected (next-state state))
         (inc infections)
         infections)]))
    )

(last (first (drop 10000000 (iterate step [(parse-input)
                                        {:pos [0 0] :dir [0 1]}
                                        0]))))


