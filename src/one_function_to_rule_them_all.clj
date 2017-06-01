(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq)
)

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str-1 str-2] (str str-1 " " str-2)) a-seq)
  )
)

(defn my-interpose [x a-seq]
  (cond
    (empty? a-seq) '()
    (== (count a-seq) 1) (seq a-seq)
    :else (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))
  )
)

(defn my-count [a-seq]
  (let [_count (fn [count elem] (inc count))
       ]
    (reduce _count 0 a-seq)
  )
)

(defn my-reverse [a-seq]
  (let [_rev (fn [x y] (cons y x))
       ]
    (if (empty? a-seq)
      '()
      (reduce _rev [] a-seq)
    )
  )
)

(defn min-max-element [a-seq]
  (let [_minmax (fn [v elem] (vector (min (get v 0) elem) (max (get v 1) elem)))
       ]
    (cond
      (== (count a-seq) 0) []
      (== (count a-seq) 1) [(first a-seq) (first a-seq)]
      :else (reduce _minmax [(first a-seq)(first (rest a-seq))] a-seq)
    )
  )
)

(defn insert [sorted-seq n]
  (concat
    (take-while (fn [x] (< x n)) sorted-seq)
    [n]
    (drop-while (fn [x] (< x n)) sorted-seq)
  )
)

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq)
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn parity [a-seq]
  (reduce
    (fn [a-seq elem] (toggle a-seq elem))
    #{}
    a-seq
  )
)

(defn minus
  ([x] (- x))
  ([x y] (- x y))
)

(defn count-params [& more]
  (count more)
)

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (* x y (reduce * 1 more)))
)

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] 
    (fn [x]
      (and 
        (p1 x) 
        (p2 x) 
        (== (count (filter (fn [y] (y x)) more)) (count more))
      )
    )
  )
)

(defn my-map [f a-seq]
  [:-])
