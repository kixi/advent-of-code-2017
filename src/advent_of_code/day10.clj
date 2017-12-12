(ns advent-of-code.day10
  (:require [clojure.spec.alpha :as s]))


(def list-of-number (range 0 5))

(defn hash-step [{:keys [hash pos] :as state} [len skip]]
  (let [[l1 l2] (split-at pos hash)
        tl (concat l2 l1)
        [tl1 tl2] (split-at len tl)
        stl (concat (reverse tl1) tl2)
        [stl1 stl2] (split-at (- (count hash) pos) stl)]
    {:hash (concat stl2 stl1)
     :pos (mod (+ pos skip len) (count hash))})
  )


(def input [183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88])

(defn round [input]
  (reduce
            hash-step
            {:hash (range 0 256)
             :pos 0}
            (map vector input (iterate inc 0))))


(defn part-1 []
  (let [hash (round input)]
    (* (first (:hash hash)) (second (:hash hash)))))

(part-1)

(def input-str "183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88")

(defn ascii [input]
  (concat (map int input) [17,31,73,47,23]))

(defn sparse-hash [input]
  (let [hash (reduce
              hash-step
              {:hash (range 0 256)
               :pos 0}
              (map vector (flatten (repeat 64 input)) (iterate inc 0)) 
              )]
    (:hash hash)
    ))

(defn dense-hash [sp-hash]
  (->> (partition 16 sp-hash)
       (map #(reduce bit-xor %))
       (map #(format "%02x" %))
       (apply str)))

(defn hash-f [input]
  (-> (ascii input)
      sparse-hash
      dense-hash
      ))

(hash-f input-str)
