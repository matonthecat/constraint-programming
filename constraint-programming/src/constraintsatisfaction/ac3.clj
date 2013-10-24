(ns constraintsatisfaction.ac3)

; domains:            {x [1 2 3], y [3 4 5]}
; unary-constraints:  [[x [(partial > 1)]] ...] aka. {x [(partial < 1)] ...}
; binary-constraints: [[[x y] [> <=]] ...]      aka. {#{x y} [> <=]}

(defn check-unary-constraints [domains unary-constraints]
  (reduce 
    (fn [domains' [x domx]] 
      (assoc domains' x (filter (apply every-pred (unary-constraints x)) domx)))
    {}
    domains))

; TODO: probably this is a part of the above algorithm
(defn arc-reduce [pred domx domy]
  (filter (fn [x] (some (partial pred x) domy)) domx))

; approach
;# agenda stores (undirected) edges to be processed
;# binary-constraints stores (directed) edges and preds to be checked
;# domains stores variables and their domains
;
;# simplified:
;# while(not empty agenda) arc-reduce
;# arc-reduce: 
;## filter domains of <x,y> by applying preds on <x,y>
;## when domain(x) changes agenda is updated (FIFO,LIFO...) with all pairs {<x,z> | z != y}

(defn to-arcs [binary-constraints]
  ; TODO: maybe it'd be better to store mapping directed edge -> binary-constraint key
  ;# this removes complexity off agenda construction and handling
;  (let [keys (keys binary-constraints)]
;    (zipmap (map set keys) (map binary-constraints keys))))
  (group-by 
    #(set (first %)) 
    binary-constraints))

(def ^:dynamic *agenda*)

(defn check-binary-constraints 
  "checks the given domains for all binary constraints"
  ([domains binary-constraints]
        (do-check-bin-constraints 
          domains 
          binary-constraints
          (to-arcs binary-constraints)
          ))
  
  ([domains binc agenda]
    (if (empty? agenda)
      domains
      (let [[[x y] preds & rest] agenda
            domx' (reduce #(arc-reduce % (domains x) (domains y)) preds)]
        (if (= (domains x) domx')
          (check-binary-constraints domains binc rest)
          (if (seq domains)
            (check-binary-constraints (assoc domains x domx') binc 
                                      (concat agenda ;(find-outgoing-arcs-except x y))
                                              nil))
            nil))))))


(defn ac3 [domains unary-constraints binary-constraints]
  (-> domains 
    (check-unary-constraints  unary-constraints)
    (check-binary-constraints binary-constraints)))
