(ns owl.triad
  (:require
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]))

(def network {1 #{2 3} 2 #{1 3} 3 #{1 2}})

(def empty-node {[0 0 0] 0 [0 0 1] 0 [0 1 0] 0 [0 1 1] 0 [1 0 0] 0 [1 0 1] 0 [1 1 1] 0})

(defn empty-profile
  [network]
  (reduce
   (fn [profile node]
     (assoc profile node empty-node)) {} (keys network)))

(defn -all-potential-alters
  [network node]
  (remove (partial = node) (keys network)))

(defn tie-value
  [network [node alter]]
  (if (contains? (get network alter) node) 1 0))

(defn triad-type
  [network node [first-alter second-alter]]
  (map (partial tie-value network) [[node first-alter] [first-alter second-alter] [node second-alter]]))

(defn alter-pairs
  [network node all-potential-alters]
  (filter
   (fn [[first-alter second-alter]]
     (not= first-alter second-alter))
   (combo/selections (all-potential-alters network node) 2)))

(defn node-triad
  [network profile node]
  (let [alter-combinations (alter-pairs network node -all-potential-alters)]
    (reduce
     (fn [profile alter-combination]
       (update-in profile [node (triad-type network node alter-combination)] inc))
     profile
     alter-combinations)))

(defn triad-profile
  [network]
  (reduce
   (partial node-triad network)
   (empty-profile network)
   (keys network)))
