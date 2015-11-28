(ns iter.test.core-test
  "@ctdean"
  (:require [clojure.test :refer :all])
  (:require [clojure.pprint :refer [cl-format]])
  (:require [iter.core :refer [iter iter* define-iter-op]]))

(deftest simple-test
  (is (= [1 4 9]
         (iter (foreach x [1 2 3])
               (collect (* x x)))))

  (is (nil? (iter "foo")))

  (is (= [[1 0] [1 2] [1 7] [2 0] [2 2] [2 7] [3 0] [3 2] [3 7]]
         (iter (foreach x [1 2 3])
               (foreach y [0 2 7])
               (collect [x y]))))

  (is (= [0 4 21]
         (iter (foreach [x y] (map vector [1 2 3] [0 2 7]))
               (collect (* x y)))))

  (is (= [0 4 21]
         (iter (foreach [x y] [1 2 3] [0 2 7])
               (collect (* x y)))))

  (is (= '(0 1 1 2 4 3 9 4 16 5)
         (iter (foreach x (range 5))
               (collect (* x x))
               (collect (inc x)))))

  (is (= '(0 1 2 3 6 5 12 7 20 9)
         (iter (foreach [x y] (range 5) (range 1 10))
               (collect (* x y))
               (collect (+ x y)))))

  (is (= '(0 -1 2 -3 4)
         (iter (foreach x (range 5))
               (collect (if (even? x)
                            x
                            (- x))))))
  )

(deftest iter*-test
  (is (= (range 10)
         (let [state (atom [])]
           (iter* (foreach x (range 10)) (swap! state conj x) (collect x))
           @state)))
  (is (>= 10
         (let [state (atom [])]
           (iter (foreach x (range 10)) (swap! state conj x) (collect x))
           (count @state))))
  (is (= (range 10)
         (let [state (atom [])]
           (iter* (foreach x (range 10)) (swap! state conj x))
           @state)))
  (is (= (range 10)
         (iter* (foreach x (range 10)) (collect x)))))

(deftest if-test
  (is (= [0 2 4 4 16 6 36 8 64 10]
         (iter (foreach x (range 10))
               (if (even? x)
                   (collect (* x x))
                   (collect (inc x))))))

  (is (= [0 4 16 36 64]
         (iter (foreach x (range 10))
               (if (even? x)
                   (collect (* x x))))))

  (is (= [[:z 0] [:z 1] [:z 2] [:z 3] [:z 4]
          [:m 5] [:z 5] [:m 6] [:z 6] [:m 7] [:z 7] [:m 8] [:z 8] [:m 9] [:z 9]]
         (iter (foreach x (range 10))
               (if (> x 4)
                   (collect [:m x]))
               (collect [:z x]))))

  (is (= [[:a 0] [:a 1] [:a 2] [:a 3] [:a 4]
          [:a 5] [:m 5] [:a 6] [:m 6] [:a 7] [:m 7] [:a 8] [:m 8] [:a 9] [:m 9]]
         (iter (foreach x (range 10))
               (collect [:a x])
               (if (> x 4)
                   (collect [:m x])))))

  (is (= '(0 0 1 2 4 3 4 16 5 6 36 7 8 64 9)
         (iter (foreach x (range 10))
               (collect x)
               (if (even? x)
                   (collect (* x x))))))

  (is (= [[:a 0]        [:m 0] [:e 0] [:z 0]
          [:a 1]        [:m 1] [:o 1] [:z 1]
          [:a 2]        [:m 2] [:e 2] [:z 2]
          [:a 3]        [:m 3] [:o 3] [:z 3]
          [:a 4]        [:m 4] [:e 4] [:z 4]
          [:a 5] [:g 5] [:m 5] [:o 5] [:z 5]
          [:a 6] [:g 6] [:m 6] [:e 6] [:z 6]
          [:a 7] [:g 7] [:m 7] [:o 7] [:z 7]
          [:a 8] [:g 8] [:m 8] [:e 8] [:z 8]
          [:a 9] [:g 9] [:m 9] [:o 9] [:z 9]]
         (iter (foreach x (range 10))
               (collect [:a x])
               (if (> x 4)
                   (collect [:g x]))
               (collect [:m x])
               (if (even? x)
                   (collect [:e x])
                   (collect [:o x]))
               (collect [:z x])))))

(deftest expr-test
  (let [state (atom [])]
    (is (= [0 1 4 9 16 25 36 49 64 81]
           (iter (foreach x (range 10))
                 (swap! state conj (* 2 x))
                 (collect (* x x)))))
    (is (= [0 2 4 6 8 10 12 14 16 18]
           @state)))

  (let [state (atom [])]
    (is (= [0 4 16 36 64]
           (iter (foreach x (range 10))
                 (if (odd? x)
                     (swap! state conj (* 2 x)))
                 (if (even? x)
                     (collect (* x x))))))
    (is (= [2 6 10 14 18]
           @state))))

(deftest prev-test
  (is (= [[0 nil] [1 0] [2 1] [3 2] [4 3] [5 4] [6 5] [7 6] [8 7] [9 8]]
         (iter (foreach x (range 10))
               (with-prev y x)
               (collect [x y]))))
  (is (= [[0 8] [1 0] [2 1] [3 2] [4 3] [5 4] [6 5] [7 6] [8 7] [9 8]]
         (iter (foreach x (range 10))
               (with-prev y x 8)
               (collect [x y]))))
  (is (= [[0 8 77] [1 0 8] [2 1 0] [3 2 1] [4 3 2]
          [5 4 3] [6 5 4] [7 6 5] [8 7 6] [9 8 7]]
         (iter (foreach x (range 10))
               (with-prev y x 8)
               (with-prev z y 77)
               (collect [x y z])))))

(deftest foreach-first-test
  (is (= [100 1 2 3 4 5 6 7 8 9]
         (iter (foreach x (range 10))
               (with-prev first? false true)
               (if first?
                   (collect 100)
                   (collect x)))))
  (is (= [:a 0 1 2 3 4]
         (iter (foreach x (range 5))
               (with-prev first-time? false true)
               (if first-time?
                   (collect :a))
               (collect x))))
  (is (= [:a 0 1 2 3 4]
         (iter (foreach x (range 5))
               (with-first first?)
               (if first?
                   (collect :a))
               (collect x)))))

(deftest set-form-test
  (is (= [[0 0] [1 1] [2 4] [3 9] [4 16]]
         (iter (foreach x (range 5))
               (with y (* x x))
               (collect [x y])))))

(deftest let-test
  (is (= [[0 0] [1 1] [2 4] [3 9] [4 16]]
         (iter (foreach x (range 5))
               (let [y (* x x)]
                 (collect [x y])))))
    (is (= [[0 0 1] [1 1 2] [2 4 5] [3 9 10] [4 16 17]]
           (iter (foreach x (range 5))
                 (let [y (* x x)
                       z (inc y)]
                   (collect [x y z])))))
    (is (= [0 1 2 3 4]
           (iter (foreach x (range 5))
                 (let []
                   (collect x))))))

(deftest stop-test
  (is (= [0]
         (iter (foreach x (range 10))
               (if (even? x)
                   (collect (* x x))
                   (stop)))))

  (is (= [0]
         (iter (foreach x (range 10))
               (collect (* x x))
               (stop))))

  (is (= [[:z 0] [:z 1] [:z 2] [:z 3] [:z 4]]
         (iter (foreach x (range 10))
               (if (> x 4)
                   (stop))
               (collect [:z x]))))
  (is (= [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1]]
         (iter (foreach x (range 10))
               (foreach y (range 2))
               (if (> x 2)
                   (stop))
               (collect [x y]))))
  (is (= [[0 0] [0 1] [1 0] [1 1] [2 0] [2 1]]
         (iter (foreach x (range 10))
               (if (> x 2)
                   (stop))
               (foreach y (range 2))
               (collect [x y]))))
  (is (= [[:a 0] [:a 1] [:a 2]]
         (iter (foreach x (range 10))
               (if (> x 2)
                   (stop))
               (collect [:a x])
               (foreach y (range 2))))))

(deftest break-test
  (is (= [0 1 2]
         (iter (foreach x (range 10))
               (if (> x 2)
                   (break))
               (collect x))))
  (is (= [0 1 2 3]
         (iter (foreach x (range 10))
               (collect x)
               (if (> x 2)
                   (break)))))
  (is (= [0 1 2 3 :done]
         (iter (foreach x (range 10))
               (collect x)
               (if (> x 2)
                   (break))
               (end (collect :done)))))
  (is (= [0 [0 0] [0 1] [0 2]
          1 [1 0] [1 1] [1 2]
          2 [2 0] [2 1] [2 2]
          3 [3 0] [3 1] [3 2]
          :done]
         (iter (foreach x (range 4))
               (collect x)
               (foreach y (range 5))
               (if (> y 2)
                   (break))
               (collect [x y])
               (end (collect :done)))))
  (is (= [0 [0 0] [0 1] [0 2] :done]
         (iter (foreach x (range 4))
               (collect x)
               (foreach y (range 5))
               (if (> y 2)
                   (break 2))
               (collect [x y])
               (end (collect :done)))))
  (is (= [[0 0 0] [0 0 1] [0 1 0] [0 1 1] [0 2 0] [0 2 1]
          [1 0 0] [1 0 1] [1 1 0] [1 1 1] [1 2 0] [1 2 1]
          [2 0 0] [2 0 1] [2 1 0] [2 1 1] [2 2 0] [2 2 1]
          :done]
         (iter (foreach x (range 3))
               (foreach y (range 3))
               (foreach z (range 3))
               (if (> z 1)
                   (break))
               (collect [x y z])
               (end (collect :done)))))
  (is (= [[0 0 0] [0 0 1]
          [1 0 0] [1 0 1]
          [2 0 0] [2 0 1]
          :done]
         (iter (foreach x (range 3))
               (foreach y (range 3))
               (foreach z (range 3))
               (if (> z 1)
                   (break 2))
               (collect [x y z])
               (end (collect :done)))))
  (is (= [[0 0 0] [0 0 1] :done]
         (iter (foreach x (range 3))
               (foreach y (range 3))
               (foreach z (range 3))
               (if (> z 1)
                   (break 3))
               (collect [x y z])
               (end (collect :done))))))

(deftest continue-test
  (is (= [0 2 4 6 8]
         (iter (foreach x (range 10))
               (if (odd? x)
                   (continue))
               (collect x))))
  (is (= [0 :a 1 2 :a 3 4 :a 5 6 :a 7 8 :a 9]
         (iter (foreach x (range 10))
               (collect x)
               (if (odd? x)
                   (continue))
               (collect :a))))
  (is (= [0 2 4 6 8 :done]
         (iter (foreach x (range 10))
               (if (odd? x)
                   (continue))
               (collect x)
               (end (collect :done)))))
  (is (= [0
          [0 0] [0 1] [0 2] [0 3] [0 4]
          1 2
          [2 0] [2 1] [2 2] [2 3] [2 4]
          3 :done]
         (iter (foreach x (range 4))
               (collect x)
               (foreach y (range 5))
               (if (odd? x)
                   (continue))
               (collect [x y])
               (end (collect :done)))))
  (is (= [0 [0 0] [0 1] [0 2] :done]
         (iter (foreach x (range 4))
               (collect x)
               (foreach y (range 5))
               (if (> y 2)
                   (break 2))
               (collect [x y])
               (end (collect :done)))))
  (is (= [[0 0 1] [0 1 1] [0 2 1]
          [1 0 1] [1 1 1] [1 2 1]
          [2 0 1] [2 1 1] [2 2 1]
          :done]
         (iter (foreach x (range 3))
               (foreach y (range 3))
               (foreach z (range 3))
               (if (even? z)
                   (continue))
               (collect [x y z])
               (end (collect :done)))))
  (is (= [[0 0 0] [0 1 0] [0 2 0]
          [1 0 0] [1 1 0] [1 2 0]
          [2 0 0] [2 1 0] [2 2 0]
          :done]
         (iter (foreach x (range 3))
               (foreach y (range 3))
               (foreach z (range 3))
               (if (odd? z)
                   (continue 2))
               (collect [x y z])
               (end (collect :done)))))
  (is (= [[0 0 0] [0 0 1]
          [1 0 0] [1 0 1]
          [2 0 0] [2 0 1]
          :done]
         (iter (foreach x (range 3))
               (foreach y (range 3))
               (foreach z (range 3))
               (if (> z 1)
                   (continue 3))
               (collect [x y z])
               (end (collect :done))))))

(deftest begin-test
  (let [state (atom [])]
    (is (= [0 1 2 3 4]
           (iter (foreach x (range 10))
                 (begin (swap! state conj :a))
                 (if (> x 4)
                     (do
                       (begin (swap! state conj :b))
                       (stop)))
                 (collect x))))
    (is (= [:a :b] @state))))

(deftest end-test
  (is (= [0 1 2 3 4 :done]
         (iter (foreach x (range 10))
               (end (collect :done))
               (if (> x 4)
                   (stop))
               (collect x))))
  (let [state (atom [])]
    (is (= [0 1 2 3 4 :done]
           (iter (foreach x (range 10))
                 (end (collect :done))
                 (end (swap! state conj :a))
                 (if (> x 4)
                     (do
                       (end (swap! state conj :b))
                       (stop)))
                 (collect x))))
    (is (= [:a :b] @state)))
  (let [state (atom [])]
    (is (= [0 1 2 3 4 :done]
           (iter (foreach x (range 10))
                 (end (collect :done))
                 (end (swap! state conj :a))
                 (if (> x 4)
                     (do
                       (end (swap! state conj :a))
                       (stop)))
                 (collect x))))
    (is (= [:a :a] @state))))

(defn- find-next
  ([key coll]
   (find-next key coll nil))
  ([key coll not-found]
   (iter (fornext xs (rest xs) coll)
         (if (empty? xs)
             (return not-found))
         (if (and (= (first xs) key) (seq (rest xs)))
             (return (second xs))))))

(deftest return-test
  (is (= 64
         (iter (foreach x (range 10))
               (if (and (zero? (mod x 4)) (> x 5))
                   (return (* x x))))))
  (is (= 25
         (iter (foreach x (range 10))
               (end 'no-op)
               (if (> x 4)
                   (return (* x x))))))
  (is (= 88
         (iter (foreach x (range 10))
               (end (return 88))
               (if (> x 4)
                   (return (* x x))))))
  (is (= 2
         (find-next :b [:a 1 :b 2 :c 3] )))
  (is (= [6 4]
         (iter (foreach x (range 10))
               (foreach y (range 10))
               (if (and (zero? (mod y 4)) (> y 2) (> x 5))
                   (return [x y]))))))

(deftest finding-test
  (is (= 64
         (iter (foreach x (range 10))
               (finding (* x x) (and (zero? (mod x 4)) (> x 5))))))

  (is (= 125
         (iter (foreach x (range 10))
               (with y (if (> x 4)
                           (* x x x)))
               (finding y)))))

(deftest reducing-test
  (is (= 45
         (iter (foreach x (range 10))
               (reducing x :by +))))
  (is (= 20
         (iter (foreach x (range 10))
               (if (even? x)
                   (reducing x :by +)))))
  (is (= 120
         (iter (foreach x (range 10))
               (if (even? x)
                   (reducing x :by + :init 100)))))
  (is (= 120
         (iter (foreach x (range 10))
               (if (even? x)
                   (reducing x :init 100 :by +)))))
  (is (= 100
         (iter (foreach x [])
               (if (even? x)
                   (reducing x :init 100 :by +)))))
  (is (= 101
         (iter (foreach x [])
               (if (even? x)
                   (reducing x :init 100 :by + :finally-by inc))))))

(deftest finally-by-test
  (is (= 45
         (iter (foreach x (range 10))
               (collect x)
               (finally-by (fn [xs] (reduce + xs))))))
  (is (= 20
         (iter (foreach x (range 10))
               (if (even? x)
                   (collect x))
               (finally-by (fn [xs] (reduce + xs)))))))

(deftest do-form-test
  (let [state (atom [])]
    (is (= '(0 0 1 8 2 3 64 4 5 216 6 7 512 8 9)
           (iter (foreach x (range 10))
                 (if (even? x)
                     (do
                       (swap! state conj x)
                       (collect (* x x x))
                       (swap! state conj (* x x))))
                 (collect x))))
    (is (= [0 0 2 4 4 16 6 36 8 64]
           @state)))
  (let [state (atom [])]
    (is (empty?
         (iter (foreach x (range 10))
               (if (even? x)
                   (do
                     (swap! state conj x))))))
    (is (= [0 2 4 6 8]
           @state))))

(deftest with-var-test
  (is (= '(0 1 4 9 16 25 36 49 64 81)
         (iter (foreach x (range 10))
               (with y (* x x))
               (collect y))))
  (is (= '(110 111 112 113 114 115 116 117 118 119)
         (iter (foreach x (range 10))
               (with y (+ 10 x))
               (with z (+ 100 y))
               (collect z)))))

(deftest accum-var-test
  (is (= '(0 1 3 6 10 15 21 28 36 45)
         (iter (foreach x (range 10))
               (accum sum (+ sum x) 0)
               (collect sum))))
  (is (= '(3 1 4 5 9 2 6 8 7)
         (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (if (not (seen? x))
                   (collect x))
               (accum seen? (conj seen? x) #{}))))
  (is (= '(3 1 4 5 9 2 6 8 7)
         (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (if (not (seen? x))
                   (do
                     (collect x)
                     (accum seen? (conj seen? x) #{})))))))

(define-iter-op my-maximizing [x]
  `(iter.macros/reducing ~x :by max))

(deftest define-iter-test
  (is (= 9
         (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (my-maximizing x)))))

(deftest collect-map-test
  (is (= {"zero" 0, "one" 1, "two" 4, "three" 9, "four" 16}
         (iter (foreach x (range 5))
               (collect-map (cl-format nil "~r" x)
                            (* x x))))))

(deftest iter-when-test
  (is (= ["one" 1 "three" 3 "five" 5 "seven" 7 "nine" 9]
         (iter (foreach x (range 10))
               (when (odd? x)
                 (collect (cl-format nil "~r" x))
                 (collect x)))))
  (is (= 165
         (iter (foreach x (range 10))
               (when (odd? x)
                 (reducing (* x x) :by +))))))

(deftest agg-test
  (is (= 9
         (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (maximizing x))))
  (is (= 1
         (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (minimizing x))))
  (is (= "programmable"
         (iter (foreach s (clojure.string/split
                           "Lisp is a programmable programming language" #" "))
               (maximizing s :using (count s)))))
  (is (= "a"
         (iter (foreach s (clojure.string/split
                           "Lisp is a programmable programming language" #" "))
               (minimizing s :using (count s)))))
  (is (= 45
         (iter (foreach x (range 10))
               (summing x))))
  (is (= 25
         (iter (foreach x (range 10))
               (when (odd? x)
                 (summing x)))))
  (is (= 190
         (iter (foreach x (range 10))
               (when (odd? x)
                 (summing (* x x))
                 (summing x)))))
  (is (= 15
         (iter (foreach x (range 6))
               (when (odd? x)
                 (multiplying x)))))
  (is (= 5
         (iter (foreach x (range 10))
               (when (odd? x)
                 (counting x)))))
  (is (= 2
         (iter (foreach x [1 nil 2])
               (counting x))))
  (is (= 9
         (iter (foreach x (range 10))
               (when (and (> x 0) (even? x))
                 (counting (* x x)))
               (when (odd? x)
                 (counting x))))))

(deftest collect-cat-test
  (is (= [1 4 8 16 2 3 16 64 256 4 5]
         (iter (foreach x [1 2 3 4 5])
               (when (even? x)
                 (collect-cat [(* x x) (* x x x) (* x x x x)]))
               (collect x))))
  (is (= [nil 1 4 8 16 nil 2 nil 3 16 64 256 nil 4 nil 5]
         (iter (foreach x [1 2 3 4 5])
               (when (even? x)
                 (collect-cat [(* x x) (* x x x) (* x x x x)]))
               (collect nil)
               (collect x)))))

(deftest nested-test
  (is (= [[0 0] [1 0] [1 1] [2 0] [2 1] [2 2]]
         (iter (foreach x (range 3))
               (foreach y (range (inc x)))
               (collect [x y]))))
  (is (= [[:a 88] [:a 99] [:b 88] [:b 99] [:c 88] [:c 99]]
         (iter (foreach x [:a :b :c])
               (foreach y [88 99])
               (collect [x y]))))
  (is (= [:a 88 99 :b 88 99 :c 88 99]
         (iter (foreach x [:a :b :c])
               (collect x)
               (foreach y [88 99])
               (collect y))))
  (is (= [[:row 1] [1 "a"] [1 "b"] [:row 2] [2 "a"] [2 "b"] [:row 3] [3 "a"] [3 "b"]]
         (iter (foreach row [1 2 3])
               (collect [:row row])
               (foreach col ["a" "b"])
               (collect [row col]))))
  (is (= ["comedy" "romance" "comedy" "romance" "action" "comedy"]
         (iter (foreach t ["comedy" "Romance" "Comedy Romance" "action comedy"])
               (foreach s (clojure.string/split t #" "))
               (collect (clojure.string/lower-case s)))))
  (is (= [:a :b :c]
         (iter (foreach x [:a :b :c])
               (collect x)
               (foreach y [])
               (collect y)))))

(deftest times-test
  (is (= [:a :a :a :a :a :a :a :a]
         (iter (times 8)
               (collect :a))))
  (is (= [:a]
         (iter (times 1)
               (collect :a))))
  (is (nil?
       (iter (times 0)
             (collect :a))))
  (is (nil?
       (iter (times -1)
             (collect :a))))
  (is (= 123456
         (count (take 123456
                      (iter (times)
                            (collect :a)))))))

(deftest forlist-test
  (is (= [[:a :b :c :d] [:b :c :d] [:c :d] [:d]]
         (iter (forlist x [:a :b :c :d])
               (collect x))))

  (is (= '([2 3] [1 3] [8 1] [2 3] [8 1])
         (iter (forlist nums [2 7 1 8 2 8 1 8 2])
               (with head (first nums))
               (with tail (rest nums))
               (with dist (.indexOf tail head))
               (when (> dist 0)
                 (collect [head dist]))))))

(deftest forever-test
  (is (= 123456
         (count (take 123456
                      (iter (forever)
                            (collect :a)))))))
(deftest while-test
  (is (= [0 1 2 3 :done]
         (iter (foreach x (range 10))
               (collect x)
               (while (< x 3))
               (end (collect :done))))))

(deftest collect-uniq-test
  (is (= '(3 1 4 5 9 2 6 8)
         (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8])
               (collect-uniq x)))))

(deftest collect-freq-test
  (is (= {"two" 2, "eight" 3}
         (iter (foreach x [2 7 1 8 2 8 1 8])
               (when (even? x)
                 (collect-freq (cl-format nil "~r" x)))))))
