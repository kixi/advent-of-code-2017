(ns advent-of-code.day9
  (:require [clojure.test :refer [is deftest]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/advent_of_code/input-day9.txt"))

(defn parse [input]
  (->> input
       (reduce (fn [{:keys [state stream count] :as st} c]
                 (case state
                   :comment (case c
                              \> (assoc st :state :groups )
                              \! (assoc st :state :comment-skip)
                              (assoc st :count (inc count)))
                   :comment-skip (assoc st :state :comment)
                   :groups (case c
                             \< (assoc st :state :comment)
                             (assoc st 
                                    :state :groups
                                    :stream (conj stream c)))))
               {:state :groups
                :stream []
                :count 0})))
;;part 1
(:count (parse input))

(defn parse-groups [stream]
  (reduce (fn [{:keys [depth sum]} c]
            (case c
              \{ {:depth (inc depth)
                  :sum (+ sum (inc depth))}
              \} {:depth (dec depth)
                  :sum sum}
              {:depth depth
               :sum sum}
              ))
          {:sum 0
           :depth 0}
          stream))

(parse-groups (:stream (parse input )))
