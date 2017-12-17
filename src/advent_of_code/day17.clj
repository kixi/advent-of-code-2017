(ns advent-of-code.day17)

(def steps 345)

;; concat split-at results in stackoverflow
(defn insert-intov [v pos value]
  (-> (subvec v 0 pos)
      (into [value])
      (into (subvec v pos))))

(def resl
  (loop [state [0] c 1 p 0]
    (if (<= c 2017)
      (let [p* (inc  (rem (+ p steps) c))]
        (recur (insert-intov state p* c) 
               (inc c)
               p*))
      state)))

(second (take 2 (drop-while #(not= 2017 %) resl)))

;; 0 stays always in first position so we just have to keep track of the second position
(def res2
  (loop [c 1 p 0 pos1 nil]
    (if (<= c 50000000)
      (let [p* (inc  (rem (+ p steps) c))]
        (recur  
               (inc c)
               p*
               (if (= p* 1) c pos1)))
      pos1)))

res2
