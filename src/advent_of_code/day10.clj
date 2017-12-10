(ns advent-of-code.day10
  (:require [clojure.spec.alpha :as s]))


(def list-of-number (range 0 5))

(defn hash-step [{:keys [hash pos] :as state} [len skip]]
  (println len skip)
  (let [[l1 l2] (split-at pos hash)
        tl (concat l2 l1)
        [tl1 tl2] (split-at len tl)
        stl (concat (reverse tl1) tl2)
        [stl1 stl2] (split-at (- (count hash) pos) stl)]
    {:hash (concat stl2 stl1)
     :pos (mod (+ pos skip len) (count hash))})
  )
(clojure.pprint/pprint (reductions
                        hash-step
                        {:hash list-of-number
                         :pos 0}
                        (map vector  [3 4 1 5] (range 0 5))))


(println (hash-step {:hash [4 3 0 1 2] :pos 1} [5 3]))

(def input [183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88])
(let [hash (reduce
            hash-step
            {:hash (range 0 256)
             :pos 0}
            (map vector input (range 0 (inc (count input)) )))]
  (* (first (:hash hash)) (second (:hash hash))))
