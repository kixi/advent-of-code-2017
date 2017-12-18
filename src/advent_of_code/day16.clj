(ns advent-of-code.day16
  (:require [clojure.string :as str]))

(def input-str "s1,x3/4,pe/b")

(defmulti ->cmd (fn [cmd]
                  (first cmd)))
(defmethod ->cmd :s [[c [size]]]
  (fn [progs]
    (let [[p0 p1] (split-at (- (count progs) size) progs)]
      (vec (concat p1 p0)))))

(defmethod ->cmd :x [[c [pos0 pos1]]]
  (fn [progs]
    (->
     progs
     (assoc pos0 (nth progs pos1))
     (assoc pos1 (nth progs pos0)))))

(defmethod ->cmd :p [[c [p0 p1]]]
  (fn [progs]
    (mapv (fn [p]
            (condp = p
              p0 p1
              p1 p0
              p))
          progs)))

(defn parse-cmd [str-cmd]
  [(keyword (str (first str-cmd)))
   (read-string (str "[" (.replace (.substring str-cmd 1) "/" " ") "]"))])

(def progs ['a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p])
#_((->cmd (parse-cmd "pe/b")) ['a 'b 'c 'd 'e])
(defn dance-step [progs commands]
  (reduce (fn [res cmd]
            (cmd res)) progs commands))

(defn commands []
  (->>
   (->
    (slurp "src/advent_of_code/input-day16.txt")
    (str/split #","))
   (map #(->cmd (parse-cmd %)))))

(defn find-reps []
  (let [cmds (commands)]

    (loop [cnt 0 p (dance-step progs cmds)]
      (if (not= p progs)
        (recur (inc cnt) (dance-step p cmds))
        (inc cnt)))))

(defn part2 []
  (let [cmds (commands)
        reps (rem 1000000000 (find-reps))]
    (apply str
           (loop [cnt 0 p progs]
             (if (< cnt reps)
               (recur (inc cnt) (dance-step p cmds))
               p)))))

