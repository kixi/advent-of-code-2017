(ns advent-of-code.day18-2
  (:require [clojure.string :as str]
            [clojure.core.async :as async]))

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
    #_(println (:prog-id env) ": Send message " (getval env param1))
    (async/>!! (:out env) (getval env param1))
    (update env :send-msgs (fnil inc 0) )))

(defmethod instruction 'rcv [[_ param1]]
  (fn [env]
    (let [[v ch](async/alts!! [(:in env)
                         (async/timeout 100)])]
      (if v
        (do
          #_(println (:prog-id env) ": Message received " v)
          (assoc env param1 v))
        (assoc env :timeout true)))))

(defmethod instruction 'jgz [[_ param1 param2]]
  (fn [env]
    (if (> (getval env param1) 0)
      (assoc env :ip (dec (+ (getval env :ip) (getval env param2))))
      env)))

(defn exec-prog [prog prog-id in-queue out-queue]
  (loop [env {:ip 0
              'p prog-id
              :in in-queue
              :out out-queue
              :prog-id prog-id}]
    #_(println "Prog" prog-id " *** " env)
    (if (and (< (:ip env) (count prog)) (not (:timeout env)))
      (recur ((instruction (get prog (:ip env))) (update env :ip inc)))
      (do (println env)
        env))))



(defn run []
  (let [queue1 (async/chan 10000)
        queue2 (async/chan 10000)]
    [(async/thread (exec-prog parsed-prog 0 queue1 queue2))
     (async/thread (exec-prog parsed-prog 1 queue2 queue1))]))


(println (run))


