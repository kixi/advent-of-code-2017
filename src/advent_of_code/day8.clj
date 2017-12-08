(ns advent-of-code.day8
(:require [clojure.test :refer [is deftest]]
          [clojure.string :as str]
          [clojure.set :as set]))


#_(def prog-str "b inc 5 if a > 1
  a inc 1 if b < 5
  c dec -10 if a >= 1
  c inc -20 if c == 10")

(def prog-str (slurp "src/advent_of_code/input-day8.txt"))
(def prog (->> prog-str
               str/split-lines
               (map #(read-string (str "[" % "]")))))


(def command-map {'== '=
         '!= 'not=
         'inc '+
         'dec '-})

(defn command [c]
  (or (get command-map c) c))

(defn my-eval [[var1 cmd val1 i var2 cmp val2] env]
  (let [val-var2 (or (get env var2) 0)
        form1 (list (command cmp) val-var2 val2)]
    (if (eval form1)
      (let [form2 (list (command cmd)  (or (get env var1) 0) val1)]
        (assoc env var1 (eval form2)))
      env)))

(defn eval-prog [prog]
  (reduce (fn [env prog-line]
            (let [e (my-eval prog-line env)]
              e))
          {}
          prog))

(def env (eval-prog prog))

(defn max-env [env]
  (if (empty? env)
    Integer/MIN_VALUE
    (apply max (vals env))))

(max-env env)

;; part-2
(defn eval-prog-max [prog]
  (reduce (fn [[max env] prog-line]
            (let [e (my-eval prog-line env)
                  maxe (max-env e)]
              [(if (> maxe max) maxe max) e]))
          [Integer/MIN_VALUE {}]
          prog))

(first (eval-prog-max prog))
