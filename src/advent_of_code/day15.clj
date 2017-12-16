(ns advent-of-code.day15)

(def gen-a-start 65)
(def gen-b-start 8921)

(def gen-a-start 289)
(def gen-b-start 629)

(defn gen-f [factor a]
  (rem (* factor a) 2147483647))



(def gen-a (partial gen-f 16807))
(def gen-b (partial gen-f 48271))

(def gen  (iterate (fn [[a b]]
                     [(gen-a a) (gen-b b)]) [65 8921]))

(def c 0x0000ffff)

(defn cp [a b]
  (= (bit-and a c) (bit-and b c)))

(take 5 gen)
#_(count (filter cp (take 40000001  gen)))

(defn part-1 []
  (loop [a gen-a-start b gen-b-start i 0 c 0]

    (if (< i 40000001)
      (recur (gen-a a) (gen-b b)
             (inc i)
             (if (cp a b) 
               (inc c) c))
      c)))

(part-1)

;; part-2
(defn gen-f2 [factor m a]
  (loop [a* (gen-f factor a)]
    (if (zero? (mod a* m))
      a*
      (recur (gen-f factor a*)))
    ))

(def gen-a-2 (partial gen-f2 16807 4))
(def gen-b-2 (partial gen-f2 48271 8))

(defn part-2 [count]
  (loop [a gen-a-start b gen-b-start i 0 c 0]

    (if (< i count)
      (recur (gen-a-2 a) (gen-b-2 b)
             (inc i)
             (if (cp a b) 
               (inc c) c))
      c)))

(part-2 5000001)
