(ns constraintsatisfaction.ac3)

(defprotocol Constraint
  (args [this]))

(defrecord BinaryConstraint [pred x y]
  Constraint
  (args [this] [x y]))

(defrecord UnaryConstraint [pred x]
  Constraint
  (args [this] [x]))

; domains:            {x [1 2 3], y [3 4 5]}
; unary-constraints:  [[x (partial > 1)] ...] aka. {x [(partial < 1)]}
; binary-constraints: [[[x y] >]]             aka. {#{x y} [> <=]}

(defn check-unary-constraints [domains unary-constraints]
  (reduce 
    (fn [domains' [x domx]] 
      (assoc domains' x (filter (every-pred (unary-constraints x)) domx)))
    {}
    domains))

; TODO: probably this is a part of the above algorithm
(defn arc-reduce [pred domx domy]
  (filter (fn [x] (some (partial pred x) domy)) domx))

(defn to-arcs [binary-constraints]
  (group-by 
    #(set (first %)) 
    binary-constraints))

(defn check-binary-constraints 
  ([domains binary-constraints]
    (check-binary-constraints 
      domains 
      binary-constraints 
      (to-arcs binary-constraints)))
  ([domains binc agenda]
    (let [[[x y] preds & rest] agenda
          domx' (reduce #(arc-reduce % (domains x) (domains y)) preds)]
      (if (= (domains x) domx')
        (check-binary-constraints domains binc rest)
        (if (seq domx)
          (check-binary-constraints (assoc domains x domx') binc 
                                    (concat agenda (find-arcs-except x y)))
          nil)))))
        
(defn ac3 [domains unary-constraints binary-constraints]
  (let [domains (check-unary-constraints domains unary-constraints)]
    (check-binary-constraints domains binary-constraints)))
