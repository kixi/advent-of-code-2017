(ns advent-of-code.day3
  (:require [clojure.test :refer [is deftest]]))


(defn next [x y]
  (cond
    (= [x y] [0 0]) :right
    (and (> y 0) (<= x y) (> (+ x y) 0)) :left
    (and (< x 0) (<= y (- x)) (> y x)) :down
    (and (< y 0) (>= x y) (<= x (- y))) :right
    :default :up
    )
  )


(defn move [[x y] mv]
  (case mv
    :right [(inc x) y]
    :left [(dec x) y]
    :down [x (dec y)]
    :up [x (inc y)]))

(defn make-move [[x y]]
  (let [m (next x y)]
    (move [x y] m))
  )

(defn distance [[x y]]
  (+ (Math/abs x ) (Math/abs y)))


(defn steps [n]
  (loop [pos [0 0] cnt 1]
    (if (>= cnt n)
      pos 
      (recur (make-move pos) (inc cnt) ))))


(distance (steps 325489))

;; part two

(defn neighbors [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]
   [(dec x) (inc y)]
   [(dec x) (dec y)]
   [(inc x) (inc y)]
   [(inc x) (dec y)]

   ])


(defn sum [pos sums]
  (if (= pos [0 0])
    1
    (reduce + (vals (select-keys  sums (neighbors pos))))))

(defn part2 [n]
  (loop [pos [0 0] sums {}]
    (let [s (sum pos sums)]
      (if (> s n)
        s
        (recur (make-move pos) (assoc sums pos s))))))



(part2 325489)


(deftest test-next
  (is (= :right (next 0 0)))
  (is (= :up (next 1 0)))

  (is (= :left (next 2 2)))
  (is (= :left (next 1 2)))
  (is (= :left (next -1 2)))

  (is (= :down (next -2 2)))
  (is (= :down (next -2 1)))
  (is (= :down (next -2 -1)))

  (is (= :right (next -2 -2)))
  (is (= :right (next -1 -2)))
  (is (= :right (next 1 -2)))
  (is (= :right (next 2 -2)))

  (is (= :up (next 3 -2)))

  (is (= :up (next 3 0)))
  (is (= :up (next 3 2)))

  )
