(ns advent-of-code.day24
  (:require [clojure.string :as str]))

(def input
  "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10")

(def components
  (->>(slurp "src/advent_of_code/input-day24.txt") 
      (str/split-lines)
      (map #(str/split % #"/"))
      (mapv (partial mapv read-string))))

components

(defn dissoc-v [v pos]
  (vec (concat (subvec v 0 pos) (subvec v (inc pos)))))
(dissoc-v [[2 2] [2 3] [9 10]] 1)
(defn log [x]
  #_(println x)
  x)
(defn build-tree [start-component components strength p id]
  #_(println "build-tree" start-component components strength p id)
  (log 
   (->>(map-indexed (fn [idx c]
                      (do #_(println "ID" id "comp" c "idx" idx "components" components)
                          (cond
                            (= (second start-component) (first c))
                            (build-tree c
                                        (dissoc-v components idx)
                                        (+ strength (apply + c))
                                        (conj p c)
                                        (inc id))
                            (= (second start-component) (second c))
                            (build-tree [(second c) (first c)]
                                        (dissoc-v components idx)
                                        (+ strength (apply + c))
                                        (conj p c)
                                        (inc id))
                            :else
                            strength
                            ))) components)
       (apply (partial max 0))))
  )

(def t (build-tree [0 0] components 0 [] 0))
t
