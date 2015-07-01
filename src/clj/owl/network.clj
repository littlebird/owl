(ns owl.network
  (:require
   [clojure.set :as set]))

(defn map-map
  [f m]
  (into
   {}
   (map
    (fn [[k v]]
      (f k v))
    m)))

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

(defn total-connections
  [network]
  (reduce + 0 (map (comp count last) network)))

(defn remove-isolates
  [network]
  (into
   {}
   (remove
    (fn [[id node]]
      (zero?
       (+ (-> node :in count)
          (-> node :out count))))
    network)))

(defn pare-network
  [network]
  (let [within (set (keys network))]
    (map-map
     (fn [target ids]
       [target (vec (set/intersection within (set ids)))])
     network)))

(defn reflect-network
  [network]
  (reduce
   (fn [all [id out]]
     (reduce
      (fn [all in]
        (update-in all [in :in] (fn [o] (conj (or o (set nil)) id))))
      (assoc-in all [id :out] (set out)) out))
   {} network))

(defn weighted-network
  [network]
  (let [total (total-connections network)
        weight (/ 1.0 total)]
    (reduce
     (fn [all [id out]]
       (let [weights (reduce (fn [weights o] (assoc weights o weight)) {} out)
             all (assoc-in all [id :out] weights)]
         (reduce
          (fn [all in]
            (assoc-in all [in :in id] weight))
          all out)))
     {} network)))

(defn total-weights
  [network]
  (reduce + 0 (mapcat (comp vals :in) (vals network))))

(defn remove-node-weights
  [node]
  (-> node
      (update-in [:in] keys)
      (update-in [:out] keys)
      (dissoc :total-weights)))

(defn remove-network-weights
  [network]
  (map-vals remove-node-weights network))

(defn jaccard-similarity
  [a b]
  (let [a (set a)
        b (set b)
        union (set/union a b)
        intersection (set/intersection a b)]
    (float (/ (count intersection) (count union)))))
[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
