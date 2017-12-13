(ns advent-of-code.day13
  (:require [clojure.string :as str]))
(def istr "0: 3
1: 2
4: 4
6: 4")
(def input (->> (slurp "src/advent_of_code/input-day13.txt")
               ;; istr
                (str/split-lines)
                (map #(str "[" (str/replace % #":" "") "]"))
                (map read-string)))

(defn position [pdepth t]
  (let [depth (dec pdepth)]
    (if (even? (quot t depth))
      (mod t depth)
      (- depth (mod t depth)))))

(defn severity [delay [layer depth]]
  (if (= (position depth (+ layer delay)) 0)
    (* layer depth)
    0))

(defn total-severity [delay input]
  (->> (map (partial severity delay) input)
       (apply +)))

;; part 1
(total-severity 0 input)

(defn caught-at [delay [layer depth]]
  (if (= (position depth (+ layer delay)) 0)
    layer))

(defn all-layers [delay input]
  (keep (partial caught-at delay) input))


(->> (iterate inc 0)
(map #(all-layers % input))
(take-while (comp not  empty?))
count)
