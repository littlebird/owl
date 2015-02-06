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

(defn map-vals
  [f m]
  (map-map
   (fn [k v]
     [k (f v)])
   m))

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
