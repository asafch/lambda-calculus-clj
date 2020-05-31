(ns lambda-calculus-clj.core
  (:require [clojure.math.numeric-tower :as math]))

; -------------- Basic Church numerals

(defn c0 [s] (fn [z] z))

(defn c1 [s]
  (fn [z] (s z)))

(defn c2 [s]
  (fn [z] (s (s z))))

(defn c3 [s]
  (fn [z] (s (s (s z)))))

; -------------- Transformations

(defn s->c
  "Read the string form of the nth Church numeral and construct a Clojure function that represents it"
  [s]
  (-> s
      read-string
      eval))

(defn c->s
  "Build the string form of a Clojure function that represents the nth Church numeral"
  [c_n]
  (let [s (fn [acc]
            (let [prefix (subs acc 0 16)
                  suffix (subs acc 16)]
              (str prefix "(s " suffix ")")))
        z "(fn [s] (fn [z] z))"]
    ((c_n s) z)))

(defmacro n->c
  "Take an integer n and build a Clojure function that is the nth Church numeral"
  [n]
  (let [make-fn-body (fn [n]
                       (loop [i n
                              acc 'z]
                         (if (zero? i)
                           acc
                           (recur (dec i) (list 's acc)))))
        body (make-fn-body n)]
    (list 'fn '[s] (list 'fn '[z] body))))

(defn c->n
  "Take a Clojure function that is the nth Church numeral and return the integer that it represents"
  [c_n]
  ((c_n inc) 0))

; -------------- Some sand for the playground

(def c_0 (n->c 0))

(def c_1 (n->c 1))

(def c_2 (n->c 2))

(def c_3 (n->c 3))

(def c_4 (n->c 4))

(def c_5 (n->c 5))

(c->n (n->c 50))

(c->s (n->c 2))















; -------------- Successor function


(defn successor [c_n]
  (fn [s]
    (fn [z] (s ((c_n s) z)))))

(c->n (successor (n->c 50)))

(c->s (successor (n->c 2)))

(map #(println (c->s (successor %)))
     [c_0 c_1 c_2 c_3])

















; -------------- Addition

(defn plus
  "Add two Church numerals"
  [a]
  (fn [b] ((b successor) a)))

(defn check-plus []
  (let [c_6 (n->c 6)]
    (println (str "In string form: " (c->s ((plus c_5) c_6))))
    (println (str "Actual number: " (c->n ((plus c_5) c_6))))))

















; -------------- Multiplication

(defn mult
  "Multiply two Church numerals"
  [a]
  (fn [b] (b (plus a))))

;(c->n ((mult c_2) c_3))
;(c->s ((mult c_2) c_3))

(defn mult
  "Multiply two Church numerals"
  [a]
  (fn ([b] ((b (plus a)) c_0))))

(defn check-mult []
  (do
    (println (str "In string form: " (c->s ((mult c_2) c_3))))
    (println (str "Actual number: " (c->n ((mult c_2) c_3))))))

















; -------------- Exponentiation

(defn exp
  "Raise the Church numeral 'a' to the b-th power"
  [a]
  (fn [b] ((b (mult a)) c_1)))

(defn check-exp []
  (do
    (println (str "In string form: " (c->s ((exp c_2) c_3))))
    (println (str "Actual number: " (c->n ((exp c_2) c_3))))
    (println (str "In string form: " (c->s ((exp c_4) c_0))))
    (println (str "Actual number: " (c->n ((exp c_4) c_0))))))

















; -------------- Super-exponentiation

(defn super-exp
  "Raise the Church numeral 'a' to the b-th power-tower"
  [a]
  (fn [b] ((b (exp a)) c_1)))

(defn t2 [a] (fn [b] ((b (super-exp a)) c_1)))
(defn t3 [a] (fn [b] ((b (t2 a)) c_1)))
(defn t4 [a] (fn [b] ((b (t3 a)) c_1)))
(defn t5 [a] (fn [b] ((b (t4 a)) c_1)))

(c->s ((super-exp c_2) c_3))

(defn surprise
  "What should happen here?"
  []
  (do
    (println (str "In string form: " (c->s ((super-exp c_2) c_3))))
    (println (str "Actual number: " (c->n ((super-exp c_2) c_3))))
    (println (str "In string form: " (c->s ((super-exp c_2) c_4))))
    (println (str "Actual number: " (c->n ((super-exp c_2) c_4))))))

(defn count-digits
  "Count the number of digits of an integer"
  ([n]
   (count-digits n 0))
  ([n acc]
   (if (zero? n)
     acc
     (recur (quot n 10) (inc acc)))))

(math/expt 2 2)                                             ; 2^2
(math/expt 2 (math/expt 2 2))                               ; 2^(2^2)
(math/expt 2 (math/expt 2 (math/expt 2 2)))                 ; 2^(2^(2^2)))
(math/expt 2 (math/expt 2 (math/expt 2 (math/expt 2 2))))   ; 2^(2^(2^(2^2))))
(count-digits (math/expt 2 (math/expt 2 (math/expt 2 (math/expt 2 2)))))

(defn omg []
  "For a JVM BigIneteger the maximum number of digits it can have is Integer.MAX_VALUE, because
  the BigInteger has an int that counts the length of the underlying array.
  Therefore the number 2^(2^(2^(2^(2^2))))) that we're trying to calculate in the function
  cannot be calculated using the JVM."
  (future
    (time (count-digits (math/expt 2 (math/expt 2 (math/expt 2 (math/expt 2 (math/expt 2 2)))))))))

















; -------------- Data structures

(defn pair [a]
  (fn [b]
    (fn [s] ((s a) b))))

((pair 5) 10)

(defn my-first [a]
  (fn [b] a))

(defn my-second [a]
  (fn [b] b))

(((pair 5) 10) my-first)
(((pair 5) 10) my-second)

















; -------------- Control flow

(defn True [x]
  (fn [y] x))

(defn False [x]
  (fn [y] y))

(defn Not [a]
  ((a False) True))

(defn And [a]
  (fn [b] ((a b) False)))

;             X
;          a=t  a=f
;         X       False
;      b=t  b=f
;   True     False

(defn Or [a]
  (fn [b] ((a True) b)))

















; -------------- Basic predicates

(defn Zero?
  "The basic mathematical predicate for Church numerals"
  [c_n]
  ((c_n (fn [x] False)) True))

(defn If
  "Lambda calculus branching logic"
  [test]
  (fn [do-if-true]
    (fn [do-if-false] ((test do-if-true) do-if-false))))

(((If (Zero? c_0))
  true)
 false)

(((If (Zero? c_1))
  true)
 false)

















; -------------- Math

(defn predecessor
  "Use an ordered pair to hold a Church numeral (c_n) and its successor (c_n+1), in fact making the
  first item in the pair the predecessor of the second one."
  [c_n]
  (let [next-elem (fn [p]
                    (let [p_2 (p my-second)]
                      ((pair p_2) (successor p_2))))]
    (((c_n next-elem) ((pair c_0) c_0)) my-first)))

(c->n (predecessor c_1))
(c->n (predecessor (n->c 100)))



(defn Monus
  "
               { a-b,   a >= b
  a monus b =  {
               { 0,     otherwise
  "
  [a]
  (fn [b] ((b predecessor) a)))

(c->n ((Monus c_5) c_2))
(c->n ((Monus c_2) c_5))

(defn Greater-or-equal
  ">="
  [a]
  (fn [b] (Zero? ((Monus b) a))))

(((If ((Greater-or-equal c_3) c_1))
  true)
 false)

(((If ((Greater-or-equal c_3) c_4))
  true)
 false)

(defn Lesser-or-equal
  "<="
  [a]
  (fn [b] (Zero? ((Monus a) b))))

(((If ((Lesser-or-equal c_3) c_4))
  true)
 false)

(((If ((Lesser-or-equal c_3) c_1))
  true)
 false)

(defn Greater
  ">"
  [a]
  (fn [b] (Not ((Lesser-or-equal a) b))))

(((If ((Greater c_3) c_1))
  true)
 false)

(((If ((Greater c_3) c_4))
  true)
 false)

(defn Lesser
  "<"
  [a]
  (fn [b] (Not ((Greater-or-equal a) b))))

(((If ((Lesser c_3) c_4))
  true)
 false)

(((If ((Lesser c_3) c_1))
  true)
 false)

(defn Equal
  "="
  [a]
  (fn [b] ((And ((Greater-or-equal a) b))
           ((Lesser-or-equal a) b))))

(((If ((Equal c_3) c_3))
  true)
 false)

(((If ((Equal c_3) c_2))
  true)
 false)

(defn Not-equal
  "!="
  [a]
  (fn [b] (Not ((Equal a) b))))

(((If ((Not-equal c_3) c_2))
  true)
 false)

(((If ((Not-equal c_3) c_3))
  true)
 false)

















; -------------- Advanced examples for ordered pairs

(defn fact
  "Calculates n! in Church numerals"
  [c_n]
  (let [next-elem (fn [p]
                    (let [p_1 (p my-first)
                          p_2 (p my-second)]
                      ((pair (successor p_1)) ((mult p_1) p_2))))]
    (((c_n next-elem) ((pair c_1) c_1)) my-second)))

(c->n (fact (n->c 0)))
(c->n (fact (n->c 1)))
(c->n (fact (n->c 2)))
(c->n (fact (n->c 3)))
(c->n (fact (n->c 4)))
(c->n (fact (n->c 5)))
(c->n (fact (n->c 6)))

(defn fib
  "Calculates the nth Fibonacci number, in Church numerals, using the iterative approach"
  [c_n]
  (let [next-elem (fn [p]
                    (let [p_1 (p my-first)
                          p_2 (p my-second)]
                      ((pair p_2) ((plus p_1) p_2))))]
    (((c_n next-elem) ((pair c_0) c_1)) my-second)))

(c->n (fib (n->c 0)))
(c->n (fib (n->c 1)))
(c->n (fib (n->c 2)))
(c->n (fib (n->c 3)))
(c->n (fib (n->c 4)))
(c->n (fib (n->c 5)))
(c->n (fib (n->c 6)))
