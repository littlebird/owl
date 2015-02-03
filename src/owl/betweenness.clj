(ns owl.betweenness
  (:require
   [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn empty-queue
  []
  (clojure.lang.PersistentQueue/EMPTY))

(def undiscovered? (comp not contains?))

(defn shortest-path?
  [found neighbor dependence]
  (= (get dependence neighbor) (inc (get dependence found))))

(defn neighbor-dependence
  [dependence queue paths visited found neighbors]
  (loop [dependence dependence
         queue queue
         paths paths
         visited visited
         neighbors neighbors]
    (if-let [neighbor (first neighbors)]
      (let [new? (undiscovered? dependence neighbor)
            dependence (if new?
                         (assoc dependence neighbor (inc (get dependence found)))
                         dependence)
            queue (if new?
                    (conj queue neighbor)
                    queue)

            shortest? (shortest-path? found neighbor dependence)
            paths (if shortest?
                    (assoc! paths neighbor (+ (get paths neighbor 0) (get paths found)))
                    paths)
            visited (if shortest?
                      (assoc! 
                       visited neighbor
                       (conj! (get visited neighbor (transient [])) found))
                      visited)]
        (recur dependence queue paths visited (rest neighbors)))
      [dependence queue paths visited])))

(defn node-dependence
  [node network]
  (loop [stack (list)
         dependence {node 0}
         queue (conj (empty-queue) node)
         paths (transient {node 1})
         visited (transient {})]
    (if-let [found (first queue)]
      (let [queue (pop queue)
            stack (conj stack found)
            neighbors (get network found)

            [dependence queue paths visited]
            (p :dependence-reduction
               (neighbor-dependence dependence queue paths visited found neighbors))]
        (recur stack dependence queue paths visited))
      [stack (persistent! paths) (persistent! visited)])))

(defn node-distance
  [distance paths distant visit]
  (p :distance
     (let [distant-distance (get distance distant 0)
           distant-path (get paths distant 0)
           visit-distance (get distance visit 0)
           visit-path (get paths visit 0)
           ratio (double (/ visit-path distant-path))
           scale (* ratio (inc distant-distance))]
       (+ scale visit-distance))))

(defn node-betweenness
  [node network betweenness]
  (let [[stack paths visited]
        (p :dependence (node-dependence node network))

        [betweenness distance]
        (reduce
         (fn [[betweenness distance] distant]
           (let [visiting (get visited distant)
                 distance
                 (p :full-distance
                    (reduce
                     (fn [distance visit]
                       (assoc! distance visit (node-distance distance paths distant visit)))
                     distance (if visiting (persistent! visiting))))
                 
                 betweenness
                 (if (= node distant)
                   betweenness
                   (p :betweenness
                      (update-in
                       betweenness [distant]
                       #(+ (or % 0) (get distance distant 0)))))]
             [betweenness distance]))
         [betweenness (transient {})] stack)]
    betweenness))

(defn network-betweenness
  [network]
  (reduce
   (fn [betweenness node]
     (node-betweenness node network betweenness))
   {} (keys network)))
