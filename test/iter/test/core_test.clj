(ns iter.test.core-test
  "@ctdean"
  (:require [clojure.test :refer :all])
  (:require [clojure.pprint :refer [cl-format]])
  (:require [iter.core :refer [iter define-iter-clause]]))

(deftest raw-test
  (is (= [1 4 9]
         (iter (for-each x [1 2 3])
               (collect (* x x)))))

  (is (= [0 4 21]
         (iter (for-each x [1 2 3])
               (for-each y [0 2 7])
               (collect (* x y)))))

  (is (= '(0 1 1 2 4 3 9 4 16 5)
         (iter (for-each x (range 5))
               (collect (* x x))
               (collect (inc x)))))

  (is (= '(0 1 2 3 6 5 12 7 20 9)
         (iter (for-each x (range 5))
               (for-each y (range 1 10))
               (collect (* x y))
               (collect (+ x y)))))

  (is (= '(0 -1 2 -3 4)
         (iter (for-each x (range 5))
               (collect (if (even? x) x (- x))))))
  )

(deftest if-test
  (is (= [0 2 4 4 16 6 36 8 64 10]
         (iter (for-each x (range 10))
               (if (even? x)
                   (collect (* x x))
                   (collect (inc x))))))

  (is (= [0 4 16 36 64]
         (iter (for-each x (range 10))
               (if (even? x)
                   (collect (* x x))))))

  (is (= [[:z 0] [:z 1] [:z 2] [:z 3] [:z 4]
          [:m 5] [:z 5] [:m 6] [:z 6] [:m 7] [:z 7] [:m 8] [:z 8] [:m 9] [:z 9]]
         (iter (for-each x (range 10))
               (if (> x 4)
                   (collect [:m x]))
               (collect [:z x]))))

  (is (= [[:a 0] [:a 1] [:a 2] [:a 3] [:a 4]
          [:a 5] [:m 5] [:a 6] [:m 6] [:a 7] [:m 7] [:a 8] [:m 8] [:a 9] [:m 9]]
         (iter (for-each x (range 10))
               (collect [:a x])
               (if (> x 4)
                   (collect [:m x])))))

  (is (= '(0 0 1 2 4 3 4 16 5 6 36 7 8 64 9)
         (iter (for-each x (range 10))
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
         (iter (for-each x (range 10))
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
           (iter (for-each x (range 10))
                 (swap! state conj (* 2 x))
                 (collect (* x x)))))
    (is (= [0 2 4 6 8 10 12 14 16 18]
           @state)))

  (let [state (atom [])]
    (is (= [0 4 16 36 64]
           (iter (for-each x (range 10))
                 (if (odd? x)
                     (swap! state conj (* 2 x)))
                 (if (even? x)
                     (collect (* x x))))))
    (is (= [2 6 10 14 18]
           @state))))

(deftest prev-test
  (is (= [[0 nil] [1 0] [2 1] [3 2] [4 3] [5 4] [6 5] [7 6] [8 7] [9 8]]
         (iter (for-each x (range 10))
               (for-prev y x)
               (collect [x y]))))
  (is (= [[0 8] [1 0] [2 1] [3 2] [4 3] [5 4] [6 5] [7 6] [8 7] [9 8]]
         (iter (for-each x (range 10))
               (for-prev y x 8)
               (collect [x y]))))
  (is (= [[0 8 77] [1 0 8] [2 1 0] [3 2 1] [4 3 2]
          [5 4 3] [6 5 4] [7 6 5] [8 7 6] [9 8 7]]
         (iter (for-each x (range 10))
               (for-prev y x 8)
               (for-prev z y 77)
               (collect [x y z])))))

(deftest for-each-first-test
  (is (= [100 1 2 3 4 5 6 7 8 9]
         (iter (for-each x (range 10))
               (for-next first? false true)
               (if first?
                   (collect 100)
                   (collect x)))))
  (is (= [:a 0 1 2 3 4]
         (iter (for-each x (range 5))
               (for-next first-time? false true)
               (if first-time?
                   (collect :a))
               (collect x)))))

(deftest set-form-test
  (is (= [[0 0] [1 1] [2 4] [3 9] [4 16]]
         (iter (for-each x (range 5))
               (with y (* x x))
               (collect [x y])))))

(deftest stop-test
  (is (= [0]
         (iter (for-each x (range 10))
               (if (even? x)
                   (collect (* x x))
                   (stop)))))

  (is (= [0]
         (iter (for-each x (range 10))
               (collect (* x x))
               (stop))))

  (is (= [[:z 0] [:z 1] [:z 2] [:z 3] [:z 4]]
         (iter (for-each x (range 10))
               (if (> x 4)
                   (stop))
               (collect [:z x])))))

(deftest begin-test
  (let [state (atom [])]
    (is (= [0 1 2 3 4]
           (iter (for-each x (range 10))
                 (begin (swap! state conj :a))
                 (if (> x 4)
                     (do
                       (begin (swap! state conj :b))
                       (stop)))
                 (collect x))))
    (is (= [:a :b] @state))))

(deftest end-test
  (is (= [[:z 0] [:z 1] [:z 2] [:z 3] [:z 4] :done]
         (iter (for-each x (range 10))
               (end (collect :done))
               (if (> x 4)
                   (stop))
               (collect [:z x]))))
  (let [state (atom [])]
    (is (= [0 1 2 3 4 :done]
           (iter (for-each x (range 10))
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
           (iter (for-each x (range 10))
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
   (iter (for-next xs (rest xs) coll)
         (:if (empty? xs)
              (stop not-found))
         (:if (and (= (first xs) key) (seq (rest xs)))
              (return (second xs))))))

(deftest return-test
  (is (= 64
         (iter (for-each x (range 10))
               (if (and (zero? (mod x 4)) (> x 5))
                   (return (* x x))))))
  (is (= 2
         (find-next :b [:a 1 :b 2 :c 3] ))))

(deftest finding-test
  (is (= 64
         (iter (for-each x (range 10))
               (finding (* x x) (and (zero? (mod x 4)) (> x 5))))))

  (is (= 125
         (iter (for-each x (range 10))
               (with y (if (> x 4)
                           (* x x x)))
               (finding y)))))

(deftest reducing-test
  (is (= 45
         (iter (for-each x (range 10))
               (reducing x :by +))))
  (is (= 20
         (iter (for-each x (range 10))
               (if (even? x)
                   (reducing x :by +)))))
  (is (= 120
         (iter (for-each x (range 10))
               (if (even? x)
                   (reducing x :by + :init 100)))))
  (is (= 120
         (iter (for-each x (range 10))
               (if (even? x)
                   (reducing x :init 100 :by +)))))
  (is (= 100
         (iter (for-each x [])
               (if (even? x)
                   (reducing x :init 100 :by +))))))

(deftest finally-by-test
  (is (= 45
         (iter (for-each x (range 10))
               (collect x)
               (finally-by (fn [xs] (reduce + xs))))))
  (is (= 20
         (iter (for-each x (range 10))
               (if (even? x)
                   (collect x))
               (finally-by (fn [xs] (reduce + xs)))))))

(deftest do-form-test
  (let [state (atom [])]
    (is (= '(0 0 1 8 2 3 64 4 5 216 6 7 512 8 9)
           (iter (for-each x (range 10))
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
         (iter (for-each x (range 10))
               (if (even? x)
                   (do
                     (swap! state conj x))))))
    (is (= [0 2 4 6 8]
           @state))))

(deftest with-var-test
  (is (= '(0 1 4 9 16 25 36 49 64 81)
         (iter (for-each x (range 10))
               (with y (* x x))
               (collect y))))
    (is (= '(110 111 112 113 114 115 116 117 118 119)
         (iter (for-each x (range 10))
               (with y (+ 10 x))
               (with z (+ 100 y))
               (collect z)))))

(deftest accum-var-test
  (is (= '(0 1 3 6 10 15 21 28 36 45)
         (iter (for-each x (range 10))
               (accum sum (+ sum x) 0)
               (collect sum))))
  (is (= '(3 1 4 5 9 2 6 8 7)
         (iter (for-each x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (if (not (seen? x))
                   (collect x))
               (accum seen? (conj seen? x) #{}))))
  (is (= '(3 1 4 5 9 2 6 8 7)
         (iter (for-each x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (if (not (seen? x))
                   (do
                     (collect x)
                     (accum seen? (conj seen? x) #{})))))))

(define-iter-clause my-maximizing [x]
  `(iter.core/reducing ~x :by max))

(deftest define-iter-test
  (is (= 9
         (iter (for-each x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (my-maximizing x)))))

(deftest collect-map-test
  (is (= {"zero" 0, "one" 1, "two" 4, "three" 9, "four" 16}
         (iter (for-each x (range 5))
               (collect-map (cl-format nil "~r" x)
                            (* x x))))))

(deftest iter-when-test
  (is (= ["one" 1 "three" 3 "five" 5 "seven" 7 "nine" 9]
         (iter (for-each x (range 10))
               (when (odd? x)
                 (collect (cl-format nil "~r" x))
                 (collect x)))))
  (is (= 165
         (iter (for-each x (range 10))
               (when (odd? x)
                 (reducing (* x x) :by +))))))

(deftest agg-test
  (is (= 9
         (iter (for-each x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (maximize x))))
  (is (= 1
         (iter (for-each x [3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3])
               (minimize x))))
  (is (= 45
         (iter (for-each x (range 10))
               (sum x))))
  (is (= 25
         (iter (for-each x (range 10))
               (when (odd? x)
                 (sum x)))))
  (is (= 190
         (iter (for-each x (range 10))
               (when (odd? x)
                 (sum (* x x))
                 (sum x)))))
  (is (= 15
         (iter (for-each x (range 6))
               (when (odd? x)
                 (multiply x)))))
  (is (= 5
         (iter (for-each x (range 10))
               (when (odd? x)
                 (counting x)))))
  (is (= 9
         (iter (for-each x (range 10))
               (when (and (> x 0) (even? x))
                 (counting (* x x)))
               (when (odd? x)
                 (counting x))))))

(deftest collect-cat-test
  (is (= [1 4 8 16 2 3 16 64 256 4 5]
         (iter (for-each x [1 2 3 4 5])
               (when (even? x)
                 (collect-cat [(* x x) (* x x x) (* x x x x)]))
               (collect x))))
    (is (= [nil 1 4 8 16 nil 2 nil 3 16 64 256 nil 4 nil 5]
         (iter (for-each x [1 2 3 4 5])
               (when (even? x)
                 (collect-cat [(* x x) (* x x x) (* x x x x)]))
               (collect nil)
               (collect x)))))

(deftest nested-test
  (is (= ["comedy" "romance" "comedy" "romance" "action" "comedy"]
         (iter (for-each t ["comedy" "romance" "comedy romance" "action comedy"])
               (iter (for-each s (clojure.string/split t #" "))
                     (collect (clojure.string/lower-case s)))))))
