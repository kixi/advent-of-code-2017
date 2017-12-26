(ns advent-of-code.day25
  (:require [clojure.string :as str]))

(def prog
  {:A {0 [1, 1, :B]
       1 [0, 1, :C]}
   :B {0 [0, -1, :A]
       1 [0, 1, :D]}
   :C {0 [1, 1, :D]
       1 [1, 1, :A]}
   :D {0 [1, -1 :E]
       1 [0, -1 :D]}
   :E {0 [1 1 :F]
       1 [1 -1 :B]}
   :F {0 [1 1 :A]
       1 [1 1 :E]}})

(defn step [[tape pos state]]
  (let [val (get tape pos 0)
        [w m s] (get-in prog [state val])]
    [(assoc tape pos w)
     (+ pos m)
     s]
    ))

(let [[tape pos state] (first (drop 12399302 (iterate step [{} 0 :A])))]
  (apply + (vals tape)))
