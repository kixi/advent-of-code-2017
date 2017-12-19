(ns advent-of-code.day18
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

(def prog (slurp "src/advent_of_code/input-day18.txt"))
(def parsed-prog
  (->> prog
                     str/split-lines
                     (map #(str "[" % "]"))
                     (mapv read-string)))

(defn getval [env param]
  (if (or (symbol? param) (keyword? param))
    (env param 0)
    param))

(defmulti instruction (fn [instr] (first instr)))

(defmethod instruction 'set [[_ param1 param2]]
  (fn [env]
    (assoc env param1 (getval env param2))))

(defmethod instruction 'add [[_ param1 param2]]
  (fn [env]
    (update env param1 (fnil + 0) (getval env param2))))

(defmethod instruction 'mul [[_ param1 param2]]
  (fn [env]
    (update env param1 (fnil * 0) (getval env param2))))

(defmethod instruction 'mod [[_ param1 param2]]
  (fn [env]
    (update env param1 (fnil rem 0) (getval env param2))))

(defmethod instruction 'snd [[_ param1]]
  (fn [env]
    (assoc env :snd (getval env param1))))

(defmethod instruction 'rcv [[_ param1]]
  (fn [env]
    (if (zero? (getval env param1))
      env
      (assoc env :rcv (getval env :snd)))))

(defmethod instruction 'jgz [[_ param1 param2]]
  (fn [env]
    (if (> (getval env param1) 0)
      (assoc env :ip (dec (+ (getval env :ip) (getval env param2))))
      env)))

(defn exec-prog [prog]
  (loop [env {:ip 0}]
    (if (and (< (:ip env) (count prog)) (not (:rcv env)))
      (recur ((instruction (get prog (:ip env))) (update env :ip inc)))
      env)))


(exec-prog parsed-prog)
