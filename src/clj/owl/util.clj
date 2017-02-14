(ns owl.util)

(defn dechunk
  [coll]
  (lazy-seq
   (when-let [[x] (seq coll)]
     (cons x
           (dechunk (rest coll))))))

(defn qartial
  "(techno?) remix of partial:
  > (defn f [a b c] {:a a :b b :c c})  ;;=> #'f
  > (def p (partial f 1 2))            ;;=> #'p
  > (def q (qartial f 2 3))            ;;=> #'q
  > (p 3)                              ;;=> {:a 1 :b 2 :c 3}  
  > (q 1)                              ;;=> {:a 1 :b 2 :c 3}"
  [f & outer]
  (fn
    [& inner]
    (apply f (concat inner outer))))

(defn mapify
  "f needs to return a key value pair"
  [f c]
  (into {} (map f c)))

(defn map-map
  [f m]
  (mapify
   (fn [[k v]]
     (f k v))
   m))

(defn map-keys
  [f m]
  (map-map
   (fn [k v]
     [(f k) v])
   m))

(defn map-vals
  [f m]
  (map-map
   (fn [k v]
     [k (f v)])
   m))

(defn parallelize
  [f]
  (comp
   (partial map deref)
   (partial mapv #(future (f %)))))

(defn sort-by-map
  [m]
  (fn [k]
    (sort-by
     (fn [e]
       (get e m 0))
     > k)))
