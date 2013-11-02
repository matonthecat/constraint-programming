(ns constraintsatisfaction.ac3-test
  (:use clojure.test
        constraintsatisfaction.ac3))

; domains:            {x [1 2 3], y [3 4 5]}
; unary-constraints:  [[x (partial > 1)] ...] aka. {x [(partial < 1)]}
; binary-constraints: [[[x y] >]]             aka. {#{x y} [> <=]}

(deftest test-check-unary-constraints 
  "tests fn check-unary-constraints"
  (let [domains {'x [1 2 3]}
        unary-constraints {'x [(partial < 1)]}]
    (is (= (check-unary-constraints domains unary-constraints)
           {'x [2 3]}))))

(deftest test-check-binary-constraints 
  "tests fn check-binary-constraints"
  (let [domains {'x [1 2 3 4] 'y [3 4 5]}
        binary-constraints {'(x y) [>]}
        result (check-binary-constraints domains binary-constraints)]
    (is (not (nil? result)))
    (if result (is (= (result 'y) [3])))
    (if result (is (= (result 'x) [4])))
    ))

(deftest test-to-arcs 
  (is (= (to-arcs {'(x y) [>]}) '((x y) (y x)))))

(deftest test-to-arcs-noarcs 
    (is (empty? (to-arcs {}))))

(deftest test-arc-reduce
  (is (= (arc-reduce-x < [1 2 3 6] [3 4 5]) [1 2 3]))
  (is (= (arc-reduce-y >= [1 2 3 5] [3 4 6]) [3 4])))

(deftest test-ac3-simple
  (let [domains {'x [1 2 3]}
        unary-constraints {'x [(partial < 1)]}]
    (is (= (ac3 domains unary-constraints {})
           {'x [2 3]}))))

(deftest test-ac3-simple+
  (let [domains {'x [1 2 3]}
        unary-constraints {'x [(partial < 1)]}]
    (is (= (check-unary-constraints domains unary-constraints)
           {'x [2 3]}))))
