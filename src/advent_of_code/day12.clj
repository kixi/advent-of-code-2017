(ns advent-of-code.day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

#_(def input
  "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(def input (slurp "src/advent_of_code/input-day12.txt"))
(str/split-lines input)

(def m
  (->>
   (str/split-lines input)
   (map #(str/split % #"\s+" ))
   (map #(map (partial read-string) %))
   (map (fn [l] {(first l) (drop 2 l)}))
   (apply merge)
   ))


(defn collect-group [m group-set id]
  (if-let [children (m id)]
    (letfn [(rec [c gs]
                (if-let [child (first c)]
                  (if (gs child)
                    (set/union gs (rec (rest c) gs))
                    (set/union gs (rec (rest c) (set/union gs (collect-group m (conj gs child) child)))))
                  gs))]
      (rec children group-set))))

;; part 1
(count (collect-group m #{} 0))

;;part 2
(def groups (->> (keys m)
                 (map #(collect-group m #{%} %))
                 distinct))

(count groups)
