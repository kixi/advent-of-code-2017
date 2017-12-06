(ns advent-of-code.day6
  (:require [clojure.test :refer [is deftest]]
            [clojure.string :as str]))

(def b [0 2 7 0])
(defn most-blocks [bank]
  (reduce (fn [[aidx aval] [idx val]]
            (if (> val aval)
              [idx val]
              [aidx aval]))
          (map vector 
               (range 0 (count bank))
               bank))
  )

(most-blocks b)

(defn distribute [bank]
  (let [[idx val] (most-blocks bank)
        bank* (assoc bank idx 0)
        bank-count (count bank)]
    (loop [b bank*
           i (mod (inc idx) bank-count)
           v val]
      (if (> v 0)
        (recur
         (update b i inc)
         (mod (inc i) bank-count)
         (dec v))
        b)
      )
    ))

(defn distribute-all [bank]
  (loop [solutions #{}
         b bank
         c 1]
    (let [s (distribute b)]
      (if (solutions s)
        c
        (recur (conj solutions s)
               s
               (inc c)))
      )))

(distribute-all [5	1	10	0	1	7	13	14	3	12	8	10	7	12	0	6]) 

(defn distribute-all-2 [bank]
  (loop [solutions {}
         b bank
         c 1]
    (let [s (distribute b)]
      (if (solutions s)
        (- c (solutions s))
        (recur (assoc solutions s c)
               s
               (inc c)))
      )))

(distribute-all-2 [5	1	10	0	1	7	13	14	3	12	8	10	7	12	0	6]) 
