(ns advent-of-code.day19
  (:require [clojure.string :as str]))


(def maze (->>
            (slurp "src/advent_of_code/input-day19.txt")
            (str/split-lines)
            (mapv seq)
            (mapv #(into [] %))
            ))

(def maxrow (count maze))
(def maxcol (count (first maze)))

(defn inside? [[row col]]
  (and
   (< -1 row maxrow)
   (< -1 col maxcol)))

(def d
  {:right [0 1]
   :left [0 -1]
   :down [1 0]
   :up [-1 0]})

(defn move [[x y] direction]
  (let [[dx dy] (direction d)]
    [(+ x dx) (+ y dy)]))

(defn sym [{:keys [pos]}]
  (get-in maze pos))

(defn find-start []
  (loop [p [0 0]]
    (if (= (get-in maze p) \|)
      p
      (if (inside? p)
        (recur (move p :right))))))

(defn forward [pos]
  (->
   (assoc pos :pos (move (:pos pos) (:dir pos)))
   (update :steps inc)))

(defn turn [{:keys [pos dir] :as position}]
  (update (condp contains? dir
            #{:up :down} (if (= (get-in maze (move pos :left)) \-)
                           (assoc position :dir :left :pos (move pos :left))
                           (assoc position :dir :right :pos (move pos :right))
                           )

            #{:left :right} (if (= (get-in maze (move pos :down)) \|)
                              (assoc position :dir :down :pos (move pos :down))
                              (assoc position :dir :up :pos (move pos :up))
                              )
            )
          :steps inc))

(defn run
  [start-pos]
  (loop [pos start-pos]
    (println pos (sym pos))
    (condp contains? (sym pos)
      #{\| \-} (recur (forward pos))
      #{\+} (recur (turn pos))
      #{\space} pos
      (recur (assoc (forward pos)
                    :letters (str (:letters pos) (sym pos)))))))

(run {:pos (find-start)
     :dir :down
      :letters ""
      :steps 0})
