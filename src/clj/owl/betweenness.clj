(ns owl.betweenness
  (:import (owl.algorithm Betweenness)))

(defn empty-queue
  []
  (clojure.lang.PersistentQueue/EMPTY))

(defn undiscovered?
  [neighbor dependence]
  (let [entry (get dependence neighbor)]
    (or (nil? entry) (< entry 0))))

(defn visit-neighbor
  [state found neighbor]
  (if (undiscovered? neighbor (:dependence state))
    (let [preexisting (get-in state [:dependence found])]
      (-> state
          (assoc-in [:dependence neighbor] (inc preexisting))
          (update-in [:queue] #(conj % neighbor))))
    state))

(defn shortest-path?
  [found neighbor dependence]
  (= (get dependence neighbor)
     (inc (get dependence found))))

(defn find-shortest
  [state found neighbor]
  (if (shortest-path? found neighbor (:dependence state))
    (let [found-path (get-in state [:paths found])]
      (-> state
          (update-in [:paths neighbor] #(+ (or % 0) found-path))
          (update-in [:visited neighbor] #(conj % found))))
    state))

(defn dependence-state
  [node]
  {:stack (list)
   :dependence {node 0}
   :queue (conj (empty-queue) node)
   :paths {node 1}
   :visited {}})

(defn process-neighbor
  [found state neighbor]
  (-> state
      (visit-neighbor found neighbor)
      (find-shortest found neighbor)))

(defn node-dependence
  [node network]
  (loop [state (dependence-state node)]
    (if-let [found (-> state :queue first)]
      (let [state (-> state
                      (update-in [:queue] pop)
                      (update-in [:stack] #(conj % found)))
            neighbors (get network found)
            state (reduce (partial process-neighbor found) state neighbors)]
        (recur state))
      state)))

(defn distance-paths
  [distance paths distant visit]
  (let [ratio (double (/ (get paths visit 0) (get paths distant 0)))
        scale (* ratio (inc (get distance distant 0)))]
    (+ scale (get distance visit 0))))

(defn node-distance
  [distance paths visited distant]
  (reduce
   (fn [distance visit]
     (assoc-in
      distance [visit]
      (distance-paths distance paths distant visit)))
   distance (get visited distant)))

(defn node-betweenness
  [network betweenness node]
  (let [{:keys [stack paths visited]} (node-dependence node network)
        [betweenness distance]
        (reduce
         (fn [[betweenness distance] distant]
           (let [distance (node-distance distance paths visited distant)
                 betweenness
                 (if (= node distant)
                   betweenness
                   (update-in
                    betweenness [distant]
                    #(+ (or % 0) (get distance distant 0))))]
             [betweenness distance]))
         [betweenness {}] stack)]
    betweenness))

(defn pure-network-betweenness
  [network]
  (reduce
   (partial node-betweenness network)
   {} (keys network)))

(defn network-betweenness
  [network]
  (let [between (Betweenness. network)]
    (into {} (.calculate between))))
