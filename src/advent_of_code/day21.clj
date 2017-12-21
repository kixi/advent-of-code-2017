(ns advent-of-code.day21
  (:require [clojure.string :as str]))

(def input "../.# => ##./#../...
  .#./..#/### => #..#/..../..../#..#")
(def input (slurp "src/advent_of_code/input-day21.txt"))

(defn flip-v [p]
  (vec (reverse p)))

(defn flip-h [p]
  (mapv (comp (partial apply str) reverse) p))

(defn rotate [p]
  (apply mapv (comp (partial apply str) reverse list) p))

(defn pattern-variants [pattern]
  (let [rv (take 4 (iterate rotate pattern))
        fv (mapv flip-v rv)
        fh (mapv flip-h rv)]
    (into #{} (concat rv fv fh))))

(defn parse-rule [v]
  (if (= (count v) 5)
    {:in (take 2 v)
     :out (drop 2 v)
     :variants (pattern-variants (take 2 v))}
    {:in (take 3 v)
     :out (drop 3 v)
     :variants (pattern-variants (take 3 v))}))

(def rules
  (->>
   input
   str/split-lines
   (map #(re-seq #"[\.#]+" %))
   (map parse-rule)))

(def pattern [".#."
              "..#"
              "###"])


(defn match? [rule pattern]
  (contains? (:variants rule) pattern))

(defn trans-pattern [pattern rules]
  (let [r  (first (drop-while #(not (match? % pattern)) rules))]
    (:out r)))

(defn log [msg x]
  (println msg x)
  x)

(defn split-pattern [pattern]
  (if (= 0 (rem (count pattern) 2))
    (->>
     (partition 2 pattern)
     (map (fn [[p0 p1]]
            (map (fn [b0 b1]
                   [(apply str b0)
                    (apply str b1)])
                 (partition 2 p0) (partition 2 p1))))
     flatten
     (partition 2))
    (->>
     (partition 3 pattern)
     (map (fn [[p0 p1 p2]]
            (map (fn [b0 b1 b2]
                   [(apply str b0)
                    (apply str b1)
                    (apply str b2)])
                 (partition 3 p0)
                 (partition 3 p1)
                 (partition 3 p2))))
     flatten
     (partition 3))))

(defn join-pattern [patterns]
  (->> (partition (int (Math/sqrt (count patterns))) patterns)
       (map (fn [tuple]
              (apply map (comp (partial apply str) list) tuple)))
       flatten))

(defn transformation-step [pattern]
  (->>
   pattern
   split-pattern
   (map #(trans-pattern % rules))
   (join-pattern)))

(defn count-on [it]
  (count (filter #(= % \#) (apply str (flatten (first (drop it (iterate transformation-step pattern))))))))

(count-on 18)
