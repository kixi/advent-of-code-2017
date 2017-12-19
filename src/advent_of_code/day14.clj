(ns advent-of-code.day14
  (:require [advent-of-code.day10 :as hash]))

(def ex-inp "flqrgnkx")
(def ex-inp "ljoxqyyw")

(defn hex-char-to-number [character]
  (Integer/parseInt (str character) 16))

(defn bit-count-nb [number]
  (loop [n number bitcount 0]
    (if (= n 0)
      bitcount
      (recur (quot n 2) (+ bitcount (rem n 2))))))

(defn bit-count-hex [hex-string]
  (->>
   (map hex-char-to-number hex-string )
   (map bit-count-nb)
   (apply +)))

(defn hash-array []
(->>
 (range 0 128)
 (map #(str ex-inp "-" %))
 (map hash/hash-f)))

(def hex-array (hash-array))

(defn fillup [bits]
  (loop [b bits]
    (if (< (count b) 4)
      (recur (conj  b 0))
      (reverse b))))

(defn number->bits [number]
  (loop [n number bits []]
    (if (= n 0)
      (fillup bits)
      (recur (quot n 2) (conj bits (rem n 2))))))

(defn hex-string->bits [hex-string]
  (->>
   (map hex-char-to-number hex-string)
   (map number->bits)
   (apply concat)
   vec)
  )

(defn bit-down [b0 b1]
                 (if (and (= b0 1) (zero? b1))
                   1
                   0)
                 ) 


(def matrix (->> (mapv hex-string->bits hex-array)))

(defn mutable-matrix [m]
  (mapv #(mapv atom %) m))

(def directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn add-directions [x y]
  (mapv + x y))

(defn valid-coord [[x y]]
  (and (>= x 0)
       (>= y 0)
       (< x 128)
       (< y 128)))

(defn nc [[x y]]
  (let [[x* y*] [(inc x) y]]
    (if (> x* 127)
      [0 (inc y)]
      [x* y*])
    ))

(defn mark-partitions [matrix]
  (let [mm (mutable-matrix matrix)
        mark-cluster (fn mark-cluster [[x y] cluster-id]
                       (let [coords (->> (map #(add-directions [x y] %) directions)
                                         (filter valid-coord)
                                         (filter #(= 1 @(get-in mm %)) ))]
                         (reset! (get-in mm [x y]) cluster-id)
                         (doseq [c coords]
                           (mark-cluster c cluster-id))))]
    (loop [[x y] [0 0] cid 2]
      (if (valid-coord [x y])
        (do
          (if (= 1 @(get-in mm [x y]))
            (mark-cluster [x y] cid))
          (recur (nc [x y]) (inc cid))))
      
      )
    (mapv #(mapv deref %) mm)))


(dec (count (distinct (flatten (mark-partitions matrix)))))




