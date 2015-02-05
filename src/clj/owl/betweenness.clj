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
  [found neighbor dependence queue]
  (if (undiscovered? neighbor dependence)
    (let [preexisting (get dependence found)
          dependence (assoc dependence neighbor (inc preexisting))
          queue (conj queue neighbor)]
      [dependence queue])
    [dependence queue]))

(defn shortest-path?
  [found neighbor dependence]
  (= (get dependence neighbor) (inc (get dependence found))))

(defn find-shortest
  [found neighbor dependence paths visited]
  (if (shortest-path? found neighbor dependence)
    (let [found-path (get paths found)
          paths (update-in paths [neighbor] #(+ (or % 0) found-path))
          visited (update-in visited [neighbor] #(conj % found))]
      [paths visited])
    [paths visited]))

(defn node-dependence
  [node network]
  (loop [stack (list)
         dependence {node 0}
         queue (conj (empty-queue) node)
         paths {node 1}
         visited {}]
    (if-let [found (first queue)]
      (let [queue (pop queue)
            stack (conj stack found)
            neighbors (get network found)
            [dependence queue paths visited]
            (reduce
             (fn [[dependence queue paths visited] neighbor]
               (let [[dependence queue] (visit-neighbor found neighbor dependence queue)
                     [paths visited] (find-shortest
                                      found neighbor dependence paths visited)]
                 [dependence queue paths visited]))
             [dependence queue paths visited]
             neighbors)]
        (recur stack dependence queue paths visited))
      [stack dependence queue paths visited])))

(defn node-betweenness
  [node network betweenness]
  (let [[stack dependence queue paths visited] (node-dependence node network)
        [betweenness distance]
        (reduce
         (fn [[betweenness distance] distant]
           (let [distance
                 (reduce
                  (fn [distance visit]
                    (update-in
                     distance [visit]
                     (fn [previous]
                       (let [distant-distance (get distance distant 0)
                             distant-path (get paths distant 0)
                             visit-distance (get distance visit 0)
                             visit-path (get paths visit 0)
                             ratio (double (/ visit-path distant-path))
                             scale (* ratio (inc distant-distance))]
                         (+ scale visit-distance)))))
                  distance (get visited distant))
                 
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
   (fn [betweenness node]
     (node-betweenness node network betweenness))
   {} (keys network)))

(defn network-betweenness
  [network]
  (let [between (Betweenness. network)]
    (.calculate between)))
