(ns advent-of-code.day23
  (:require [clojure.string :as str]))

#_(def prog
  "set a 1
  add a 2
  mul a a
  mod a 5
  snd a
  set a 0
  rcv a
  jgz a -1
  set a 1
  jgz a -2")

(def prog (slurp "src/advent_of_code/input-day23.txt"))
(def parsed-prog
  (->> prog
                     str/split-lines
                     (map #(str "[" % "]"))
                     (mapv read-string)))
(-> parsed-prog)
(defn getval [env param]
  (if (or (symbol? param) (keyword? param))
    (env param 0)
    param))

(defmulti instruction (fn [instr] (first instr)))

(defmethod instruction 'set [[_ param1 param2]]
  (fn [env]
    (assoc env param1 (getval env param2))))

(defmethod instruction 'sub [[_ param1 param2]]
  (fn [env]
     (update env param1 (fnil - 0) (getval env param2))))

(defmethod instruction 'mul [[_ param1 param2]]
  (fn [env]
    (-> 
     (update env param1 (fnil * 0) (getval env param2))
     (update-in [:debug :mul] (fnil inc 0)))))

(defmethod instruction 'jnz [[_ param1 param2]]
  (fn [env]
    (if (not= (getval env param1) 0)
      (assoc env :ip (dec (+ (getval env :ip) (getval env param2))))
      env)))

(defn exec-prog [prog]
  (loop [env {:ip 0} cnt 0]
    (if (and (< (:ip env) (count prog)) (not (:rcv env)))
          (recur ((instruction (get prog (:ip env))) (update env :ip inc)) (inc cnt))
      (assoc env :count cnt))))

(exec-prog parsed-prog)


(defn prime? [x]
  (not (some #(= (mod x %) 0) (range 2 (dec x)))))

(defn part-2 []
  (let [b (+ (* 84 100) 100000)
        c (+ 17000 b)]
    (->
     (filter (comp not prime?) (range b (inc c) 17))
     count)))

(part-2)
