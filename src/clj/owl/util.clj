(ns owl.util)

(defn dechunk
  [coll]
  (lazy-seq
   (when-let [[x] (seq coll)]
     (cons x
           (dechunk (rest coll))))))
