(ns advent-of-code.day20
  (:require [clojure.string :as str]))

(defn parse-line [str-line]
  (->>
   (str/split str-line #", ")
   (map (fn [s] {(keyword (str (first s)))
                 (->(str "[" (.substring s 3 (- (.length s) 1)) "]")
                    (read-string))
                 }))
   (apply merge)))

(def coords
  (->> (slurp "src/advent_of_code/input-day20.txt")
       (str/split-lines)
       (mapv parse-line)
       (map-indexed (fn [idx item]
                      (assoc item :idx idx)))))


(defn distance [v]
  (apply + (map #(if (< % 0) (- %) %) v)))

(defn addv [v0 v1]
  (mapv + v0 v1))

(defn next-iter-item [item]
  (let [v (update item :v addv (:a item))
        p (update v :p addv (:v v))]
    p))

(defn next-iter-coords [coords]
  (map next-iter-item coords))

(defn min-distance-particles [coords]
  (let [min-distance 
        (->>
         coords
         (map #(distance (:p %)))
         (apply min))]
    (filter #(= (distance (:p %)) min-distance) coords)
    )
  )

(defn part-1 []
  (loop [c coords i 0 p {:idx -1}]
    (let [particle (first (min-distance-particles c))]
      (when (not= (:idx p) (:idx particle))
        (println "Iteration" i "Particle:" particle))
      (if (< i 1000)
        (recur (next-iter-coords c) (inc i) particle)
        particle))))

(defn coll-positions [coords]
  (->> coords
       (map :p)
       (frequencies)
       (filter (fn [[k v]] (> v 1)))
       (map first)
       set))

(defn part-2 []
  (->
   (loop [c coords i 0]
     (let [cp (coll-positions c)
           nc (filter #(not (cp (:p %))) c)]
       (println (count nc))
       (if (< i 1000)
         (recur (next-iter-coords nc) (inc i))
         nc))
     )
   count)
  )
