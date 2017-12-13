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

(defn position [range t]
  (let [r (dec range)]
    (if (even? (quot t r))
      (mod t r)
      (- r (mod t r)))))

(defn caught? [t range]
  (zero? (position range t)))

(defn severity [depth range]
  (* depth range))

(defn total-severity [layers]
  (->>
   layers
   (filter (partial apply caught?))
   (map (partial apply severity))
   (apply +)))

;; part 1
;;(total-severity input)

(defn caught-in-firewall? [delay input]
  (reduce (fn [_ [depth range]]
            (when-let [c (caught? (+ depth delay) range)]
              (reduced c)))
          nil
          input))

(defn part-2 []
  (->> (range)
       (take-while #(caught-in-firewall? % input))
       count))

