(ns each.test.core-test
  "@ctdean"
  (:require [clojure.test :refer :all])
  (:require [each.core :refer [each]]))

(deftest raw-test
  (is (= [1 4 9]
         (each (:for x [1 2 3])
               (:collect (* x x)))))

  (is (= [0 4 21]
         (each (:for x [1 2 3])
               (:for y [0 2 7])
               (:collect (* x y)))))

  (is (= '(0 1 1 2 4 3 9 4 16 5)
         (each (:for x (range 5))
               (:collect (* x x))
               (:collect (inc x)))))

  (is (= '(0 1 2 3 6 5 12 7 20 9)
         (each (:for x (range 5))
               (:for y (range 1 10))
               (:collect (* x y))
               (:collect (+ x y)))))
  )

(deftest if-test
  (is (= [0 2 4 4 16 6 36 8 64 10]
         (each (:for x (range 10))
               (:if (even? x)
                    (:collect (* x x))
                    (:collect (inc x))))))

  (is (= [0 4 16 36 64]
         (each (:for x (range 10))
               (:if (even? x)
                    (:collect (* x x))))))

  (is (= [[:z 0] [:z 1] [:z 2] [:z 3] [:z 4]
          [:m 5] [:z 5] [:m 6] [:z 6] [:m 7] [:z 7] [:m 8] [:z 8] [:m 9] [:z 9]]
         (each (:for x (range 10))
               (:if (> x 4)
                    (:collect [:m x]))
               (:collect [:z x]))))

  (is (= [[:a 0] [:a 1] [:a 2] [:a 3] [:a 4]
          [:a 5] [:m 5] [:a 6] [:m 6] [:a 7] [:m 7] [:a 8] [:m 8] [:a 9] [:m 9]]
         (each (:for x (range 10))
               (:collect [:a x])
               (:if (> x 4)
                    (:collect [:m x])))))

  (is (= '(0 0 1 2 4 3 4 16 5 6 36 7 8 64 9)
         (each (:for x (range 10))
               (:collect x)
               (:if (even? x)
                    (:collect (* x x))))))

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
         (each (:for x (range 10))
               (:collect [:a x])
               (:if (> x 4)
                    (:collect [:g x]))
               (:collect [:m x])
               (:if (even? x)
                    (:collect [:e x])
                    (:collect [:o x]))
               (:collect [:z x])))))

(deftest expr-test
  (let [state (atom [])]
    (is (= [0 1 4 9 16 25 36 49 64 81]
           (each (:for x (range 10))
                 (swap! state conj (* 2 x))
                 (:collect (* x x)))))
    (is (= [0 2 4 6 8 10 12 14 16 18]
           @state)))

  (let [state (atom [])]
    (is (= [0 4 16 36 64]
           (each (:for x (range 10))
                 (:if (odd? x)
                      (swap! state conj (* 2 x)))
                 (:if (even? x)
                      (:collect (* x x))))))
    (is (= [2 6 10 14 18]
           @state))))

(deftest prev-test
  (is (= [[0 nil] [1 0] [2 1] [3 2] [4 3] [5 4] [6 5] [7 6] [8 7] [9 8]]
         (each (:for x (range 10))
               (:for y :previous x)
               (:collect [x y]))))
  (is (= [[0 8] [1 0] [2 1] [3 2] [4 3] [5 4] [6 5] [7 6] [8 7] [9 8]]
         (each (:for x (range 10))
               (:for y :previous x :initially 8)
               (:collect [x y])))))

(deftest if-first-time-test
  (is (= [100 1 2 3 4 5 6 7 8 9]
         (each (:for x (range 10))
               (:if-first?
                (:collect 100)
                (:collect x)))))
  (is (= [:a 0 1 2 3 4]
         (each (:for x (range 5))
               (:if-first?
                (:collect :a))
               (:collect x)))))

(deftest let-test
  (is (= [[0 0] [1 1] [2 4] [3 9] [4 16]]
         (each (:for x (range 5))
               (:let y (* x x))
               (:collect [x y])))))

(deftest stop-test
  (is (= [0]
         (each (:for x (range 10))
               (:if (even? x)
                    (:collect (* x x))
                    (:stop)))))

  (is (= [0]
         (each (:for x (range 10))
               (:collect (* x x))
               (:stop))))

  (is (= [[:z 0] [:z 1] [:z 2] [:z 3] [:z 4]]
         (each (:for x (range 10))
               (:if (> x 4)
                    (:stop))
               (:collect [:z x])))))

(deftest return-test
  (is (= 64
         (each (:for x (range 10))
               (:if (and (zero? (mod x 4)) (> x 5))
                    (:return (* x x)))))))

(deftest finding-test
  (is (= 64
         (each (:for x (range 10))
               (:finding (* x x) :where (and (zero? (mod x 4)) (> x 5))))))

  (is (= 125
         (each (:for x (range 10))
               (:let y (if (> x 4)
                           (* x x x)))
               (:finding y)))))

(deftest reducing-test
  (is (= 45
         (each (:for x (range 10))
               (:reducing x :by +))))
  (is (= 20
         (each (:for x (range 10))
               (:if (even? x)
                    (:reducing x :by +))))))
