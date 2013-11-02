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
  (distinct
         (concat (keys binary-constraints)
                 (map reverse (keys binary-constraints)))))

(def ^:dynamic *agenda*)

(defn find-out-edges [edges x & {y :except}]
  (filter (fn [[v w]] 
            (or (and (= v x)
                     (or (not    y)
                         (not= w y)))
                (and (= w x)
                     (or (not    y)
                         (not= v y)))))
          edges))

; TODO: probably this is a part of the above algorithm
(defn arc-reduce-x [pred domx domy]
  (filter (fn [x] (some #(pred x %) domy)) domx))
(defn arc-reduce-y [pred domx domy]
  (filter (fn [y] (some #(pred % y) domx)) domy))

(defn check-binary-constraints 
  "checks the given domains for all binary constraints"
  ([domains binary-constraints]
    (check-binary-constraints 
      domains 
      binary-constraints
      (to-arcs binary-constraints)))
  
  ([domains binc agenda]
    (if (empty? agenda)
      domains
      (let [;[[x y] & agenda] agenda
            x (-> agenda ffirst)
            y (-> agenda first second)
            
            agenda (rest agenda)
            
            domx' (if (binc [x y])
                    (reduce (fn [domx & [constraint]] (if constraint (arc-reduce-x constraint domx (domains y)) domx)) (domains x) (binc [x y]))
                    (domains x))
            domx' (if (binc [y x])
                    (reduce (fn [domx & [constraint]] (println constraint domx (domains y)) (if constraint (arc-reduce-y constraint (domains y) domx) domx)) domx'       (binc [y x]))
                    domx')]
        
        ;  (println  domx' rest)
        (if (seq domx')
          
          (if (not= (domains x) domx')
            (let [domains (assoc domains x domx')
                  agenda  (reduce conj agenda (find-out-edges (keys binc) x :except y))]
              
              (check-binary-constraints domains binc agenda))
            (check-binary-constraints domains binc agenda)))))))


(defn ac3 [domains unary-constraints binary-constraints]
  (-> domains 
    (check-unary-constraints  unary-constraints)
    (check-binary-constraints binary-constraints)))
