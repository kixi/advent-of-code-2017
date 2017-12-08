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
  (get command-map c c))

(defn my-eval [[var1 cmd val1 i var2 cmp val2] env]
  (let [val-var2 (get env var2 0)
        form1 (list (command cmp) val-var2 val2)]
    (if (eval form1)
      (let [form2 (list (command cmd)  (get env var1 0) val1)]
        (assoc env var1 (eval form2)))
      env)))

(defn eval-prog [prog]
  (->>
   (reduce (fn [env prog-line]
             (my-eval prog-line env))
           {}
           prog)
   vals
   (apply max)))

;; part 1
(eval-prog prog)


(defn eval-prog2 [prog]
  (->>
   (reductions (fn [env prog-line]
                 (my-eval prog-line env))
               {}
               prog)
   (map vals)
   (keep identity)
   (map (partial apply max))
   (apply max)
   ))

(eval-prog2 prog)

