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

(deftest test-to-arcs 
  (let [result (to-arcs {'(x y) [>]})]
    (is (contains? result #{'x 'y}))
    (is (= (result #{'x 'y}) [['(x y) [>]]]))))

(deftest test-arc-reduce
  (is (= (arc-reduce < [1 2 3] [3 4 5]) [1 2 3])))