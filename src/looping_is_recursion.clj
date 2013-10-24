(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [base exp acc]
                       (if (zero? exp)
                         acc
                         (recur base (dec exp) (* base acc))))]
    (power-helper base exp 1)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (loop [seq1 seq1
         seq2 seq2]
    (cond
     (every? empty? [seq1 seq2])
       true
     (or (apply distinct? (map first [seq1 seq2]))
         (some empty? [seq1 seq2]))
       false
     :else
       (recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         a-seq a-seq
         acc 0]
    (cond
     (empty? a-seq)       nil
     (pred (first a-seq)) acc
     :else                (recur pred (rest a-seq) (inc acc)))))

(defn avg [a-seq]
  (loop [a-seq a-seq
         acc 0
         n   0]
    (if (empty? a-seq)
      (/ acc n)
      (recur (rest a-seq)
             (+ (first a-seq) acc)
             (inc n)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [a-seq a-seq
           a-set #{}]
      (if (empty? a-seq)
        a-set
        (recur (rest a-seq)
               (toggle a-set (first a-seq)))))))

(defn fast-fibo [n]
  (if (< n 2)
    n
    (loop [n-2 0
           n-1 1
           n   n]
      (if (= n 2)
        (+ n-2 n-1)
        (recur n-1
               (+ n-2 n-1)
               (dec n))))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         b-seq []
         a-set #{}]
    (if (or (contains? a-set (first a-seq))
            (empty? a-seq))
      b-seq
      (recur (rest a-seq) (conj b-seq (first a-seq)) (conj a-set (first a-seq))))))


