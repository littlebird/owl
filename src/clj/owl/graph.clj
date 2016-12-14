(ns owl.graph
  (:require
   [clojure.set :as set]))

(defn traverse
  ([structure f initial graph]
   (traverse structure f initial graph (rand-nth (keys graph))))
  ([structure f initial graph start]
   (loop [structure (conj structure start)
          visited #{start}
          result initial]
     (if-let [node (first structure)]
       (let [shrink (pop structure)
             neighbors (get graph node)
             unvisited (remove visited neighbors)
             expanded (reduce
                       (fn [structure neighbor]
                         (conj structure neighbor))
                       shrink unvisited)
             visited (set/union visited (set neighbors))
             iteration (f result node)]
         (recur expanded visited iteration))
       result))))

(def breadth-first (partial traverse (clojure.lang.PersistentQueue/EMPTY)))
(def depth-first (partial traverse nil))
