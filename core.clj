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
  ;; Problem 35, Local bindings
  7

  ;; Problem 36, Let it Be
  ; [x 7 y 3 z 1]

  ;; Problem 37, Regular Expressions
  "ABC"

  ;; Problem 38, Maximum value
  (fn [& x]
    (loop [max (first x) rx (rest x)]
      (if (empty? rx)
        max
        (recur (if (< max (first rx)) (first rx) max) (rest rx)))))

  ;; Problem 39, Interleave Two Seqs
  (fn [sqa sqb]
    (loop [sqab [] sqa sqa sqb sqb]
      (if (or (empty? sqa) (empty? sqb))
        sqab
        (recur (conj sqab (first sqa) (first sqb)) (rest sqa) (rest sqb)))))

  ;; Problem 40, Interpose a Seq
  (fn [in sq]
    (reduce #(conj %1 in %2) [(first sq)] (rest sq)))

  ;; Problem 41, Drop Every Nth Item
  ((fn [sq n]
     (loop [i 1 sq sq nsq []]
       (cond (empty? sq) nsq
             (= i n) (recur 1 (rest sq) nsq)
             :else (recur (inc i) (rest sq) (conj nsq (first sq)))))) [1 2 3 4 5 6 7 8] 3)

  ;; Problem 42, Factorial Fun
  (#(reduce * (range 1 (inc %))) 5)

  ;; Problem 43, Reverse Interleave
  (fn [sq x]
    (->> sq
         (partition x)
         (apply map vector)))

  ;; Problem 44, Rotate Sequence
  (fn [n sqn]
    (let [steps (mod n (count sqn))
          n-el (take steps sqn)
          rest-sqn (drop steps sqn)]
      (concat rest-sqn n-el)))

  ;; Problem 45, Intro to Iterate
  `(1 4 7 10 13)

  ;; Problem 46, Flipping out
  ((fn [fun]
     (fn [x y] (fun y x))) quot)

  ;; Problem 47, Contain Yourself
  4

  ;; Problem 48, Intro to some
  6

  ;; Problem 49, Split a sequence
  (fn [split-at-i sqn]
    [(take split-at-i sqn) (drop split-at-i sqn)])

  ;; Problem 50, Split by Type
  #(vals (group-by type %))

  ;; Problem 51, Advanced Destructuring
  [1 2 3 4 5]

  ;; Problem 52, Intro to Destructuring
  [c e]

  ;; Problem 53, Longest Increasing Sub-Seq
  (fn [s]
    (loop [remaining s longest-seq [] current-seq []]
      (if (empty? remaining)
        longest-seq
        (let [current-num (first remaining)
              rest-nums (rest remaining)
              should-extend? (or (empty? current-seq) (> current-num (last current-seq)))
              new-current-seq (if should-extend? (conj current-seq current-num) [current-num])
              count-new-current-seq (count new-current-seq)
              is-new-longest? (and (> count-new-current-seq 1) (> count-new-current-seq (count longest-seq)))
              new-longest-seq (if is-new-longest? new-current-seq longest-seq)]
          (recur rest-nums new-longest-seq new-current-seq)))))

  ;; Problem 54, Partition a Sequence
  (fn [x s]
    (loop [remaining s res []]
      (if (empty? remaining)
        res
        (let [current-sub-seq (take x remaining)
              new-remaining (drop x remaining)
              complete-sub-s? (= (count current-sub-seq) x)
              new-res (if complete-sub-s? (conj res current-sub-seq) res)]
          (recur new-remaining new-res)))))

  ;; Problem 55, Count Occurences
  (fn [s] (reduce #(update %1 %2 (fnil inc 0)) {} s))

  ;; Problem 56, Find Distinct Items
  (fn [s] (reduce (fn [acc cur]
                          (if (some #{cur} acc)
                            acc
                            (conj acc cur))) [] s))
  )
