(ns advent-of-code.day4
  (:require [clojure.test :refer [is deftest]]
            [clojure.string :as str]))

(defn words [line]
  (str/split line #"\s"))

(defn valid-passphrase? [line]
  (let [ws (words line)]
    ;; discover duplicates (number of elements in list does not equal to number of elements in set)
    (= (count ws) (count (set ws)))))

(def input (slurp "src/advent_of_code/input-day4.txt"))

(->> 
 (str/split-lines input)
 (filter valid-passphrase?)
 count)

;; part 2
(defn valid-passphrase-anagram? [line]
  ;; sort each word -> anagrams are same sorted word
  (let [ws (map sort (words line))]
    (= (count ws) (count (set ws))))
  )

(->> 
 (str/split-lines input)
 (filter valid-passphrase-anagram?)
 count)
