(ns advent-of-code.day6
  (:require [clojure.test :refer [is deftest]]
            [clojure.string :as str]))

(def b [0 2 7 0])
(defn highest-bank [banks]
  (reduce (fn [[idxa vala] [idx val]]
            (if (> val vala)
              [idx val]
              [idxa vala]))
          (map vector (range 0 (count banks)) banks )))

(highest-bank b)

(defn distribute [banks]
  (let [[idx val] (highest-bank banks)
        rem-banks (assoc banks idx 0)]

    rem-banks
    ))

(distribute b)
