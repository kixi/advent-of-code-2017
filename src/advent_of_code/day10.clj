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

(defn position)

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

(def input-str "183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88")
(defn ascii [input]
  (concat (map int input) [17,31,73,47,23]))

(def asc (ascii input-str))

(defn sparse-hash [asc]
  (let [hash (reduce
              hash-step
              {:hash (range 0 256)
               :pos 0}
              (->> (map vector asc (range 0 (inc (count asc)) ))
                   (mapcat #(repeat 64 %)))
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
      println
      sparse-hash
      println
      dense-hash
      println
      ))

(hash-f input-str)
(hash-f "")
(dense-hash sp-hash)
(format "%02x" 47)
