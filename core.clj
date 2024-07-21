(ns fourclojure-solutions.core
  (:require [clojure.string :as str]))

(comment
  ;; Problem 1, Nothing but the Truth
  (empty? [])

  ;; Problem 2, Simple Math
  4

  ;; Problem 3, Strings
  "HELLO WORLD"

  ;; Problem 4, Lists
  :a :b :c

  ;; Problem 5, conj on lists
  `(1 2 3 4)

  ;; Problem 6, Vectors
  :a :b :c

  ;; Problem 7, conj on vectors
  `(1 2 3 4)

  ;; Problem 8, Sets
  #{:a :b :c :d}

  ;; Problem 9, conj on sets
  2

  ;; Problem 10, Maps
  20

  ;; Problem 11, conj on maps
  [:b 2]

  ;; Problem 12, Sequences
  3

  ;; Problem 13, rest
  (conj `(40) 30 20)

  ;; Problem 14, Functions
  8

  ;; Problem 15, Double Down
  (fn [num] (* 2 num))

  ;; Problem 16, Hello World
  (fn [name] (str "Hello, " name "!"))

  ;; Problem 17, map
  [6 7 8]

  ;; Problem 18, filter
  [6 7]

  ;; Problem 19, Last Element
  (fn [s]
    (cond
      (empty? (rest s)) (first s)
      :else (recur (rest s))))

  ;; Problem 20, Penultimate Element
  (fn [[x & xs]]
    (cond
      (= (count xs) 1) x
      :else (recur xs)))

  ;; Problem 21, Nth Element
  (fn [[x & xs] n]
    (cond
      (zero? n) x
      :else (recur xs (- n 1))))

  ;; Problem 22, Count a Sequence
  (fn [sqn]
    (reduce (fn [n _] (inc n)) 0 sqn))

  ;; Problem 23, Reverse a Sequence
  (fn [sqn] (reduce conj `() sqn))

  ;; Problem 24, Sum It All Up
  (fn [sqn] (reduce + sqn))

  ;; Problem 25, Find the odd numbers
  (fn [sqn] (filter odd? sqn))

  ;; Problem 26, Fibonacci Sequence
  (fn [n]
    (cond
      (zero? n) `()
      (= 1 n) `(1)
      (= 2 n) `(1 1)
      :else (let [fibber (fn [x seq]
                           (if (zero? x)
                             seq
                             (recur
                              (- x 1)
                              (conj seq (+ (first seq) (second seq))))))]
              (reverse (fibber (- n 2) `(1 1))))))

  ;; Problem 27, Palindrome Detector
  (fn [sqn] (= (reverse sqn) (seq sqn)))

  ;; Problem 28, Flatten a Sequence
  (fn [x]
    (filter (complement sequential?)
            (rest (tree-seq sequential? seq x))))

  ;; Problem 29, Get the Caps
  (fn [s]
    (let [caps (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]
      (str/join (filter #(contains? caps %) s))))

  ;; Problem 30, Compress a Sequence
  (fn [s]
    (let [deduImpl (fn [ns [x & xs]]
                    (cond (empty? xs) (conj ns x)
                          (= x (first xs)) (recur ns xs)
                          :else (recur (conj ns x) xs)))]
         (deduImpl [] s)))

  ;; Problem 31, Pack a Sequence
  #(partition-by identity %)

  ;; Problem 32, Duplicate a Sequence
  (fn [s] (reduce #(conj %1 %2 %2) [] s))

  ;; Problem 33, Replicate a Sequence
  (fn [s n] (reduce #(apply conj %1 (repeat n %2)) [] s))

  ;; Problem 34, Implement range 
  (fn [lb ub]
     (loop [s [lb]]
       (if (= (last s) (- ub 1))
         s 
         (recur (conj s (inc (last s)))))))

 )
