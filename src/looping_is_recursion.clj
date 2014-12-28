(ns looping-is-recursion)

(defn power [base exp]
  (let [pow' (fn [acc n]
               (if (zero? n)
                 acc
                 (recur (* acc base) (dec n))))]
    (pow' 1 exp)))

(defn last-element [a-seq]
  (let [last' (fn [lst a-seq]
                (if (empty? a-seq)
                  lst
                  (recur (first a-seq) (rest a-seq))))]
    (last' nil a-seq)))

(defn seq= [seq1 seq2]
  (let [s-eq (fn [s1 s2]
               (cond
                 (and (empty? s1) (empty? s2)) true
                 (not= (first s1) (first s2)) false
                 (or (empty? s1) (empty? s2)) false ; to handle a case when one is empty another one has nil
                 :else (recur (rest s1) (rest s2))))]
    (s-eq seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0 ; initial values, new ones are assigned by recur call
         sq a-seq]
    (cond
      (empty? sq) nil
      (pred (first sq)) idx
      :else (recur (inc idx) (rest sq))))) ; recur ~ @tailrec in scala

(defn avg [a-seq]
  (loop [amount 0
         a-sum 0
         sq a-seq]
    (if (empty? sq)
      (/ a-sum amount)
      (recur (inc amount) (+ a-sum (first sq)) (rest sq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [accum #{}
           sq a-seq]
      (if (empty? sq)
        accum
        (recur (toggle accum (first sq)) (rest sq))))))

(defn fast-fibo [n]
  (loop [current 1
         f-n-1 0
         f-n 1]
    (cond
      (zero? n) 0
      (= current n) f-n
      :else (recur (inc current) f-n (+ f-n-1 f-n)))))

(defn cut-at-repetition [a-seq]
  (loop [already-met #{}
         accum []
         sq a-seq]
    (if (or (empty? sq) (contains? already-met (first sq)))
      accum
      (recur
        (conj already-met (first sq))
        (conj accum (first sq))
        (rest sq)))))

