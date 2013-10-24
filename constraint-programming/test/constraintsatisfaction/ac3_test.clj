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
  (let [domains {'x [1 2 3] 'y [3 4 5]}
        binary-constraints {'(x y) [<]}
        result (check-binary-constraints domains binary-constraints)]
    (is (= (result 'x) [1 2]))))

(deftest test-to-arcs 
  (let [result (to-arcs {'(x y) [>]})]
    (is (contains? result #{'x 'y}))
    (is (= (result #{'x 'y}) [['(x y) [>]]]))))

(deftest test-to-arcs-noarcs 
    (is (= (to-arcs {}) {})))

(deftest test-arc-reduce
  (is (= (arc-reduce < [1 2 3] [3 4 5]) [1 2 3]))
  (is (= (arc-reduce >= [1 2 3] [3 4 5]) [3])))

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
