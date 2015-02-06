(ns owl.modularity
  (:require
   [clojure.set :as set]))

(defn total-connections
  [network]
  (reduce + 0 (map (comp count last) network)))

(defn prepare-network
  [network]
  (let [modules
        (into
         {}
         (map
          (fn [[id out]]
            [id {:nodes (set [id])
                 :in (set [])
                 :out (set out)
                 :weights (into {} (map (fn [o] [o 1]) out))}])
          network))
        total (total-connections network)]
    {:total (/ 1.0 (* 2 total)) :modules modules}))

(defn modularity-difference
  [network total modules community id]
  (let [module (get modules id)
        in-community (count (:in community))
        out-community (count (:out community))
        out-module (count (:out module))
        intersect (count (set/intersection (:out module) (:nodes community)))

        a (* (+ in-community intersect) total)
        b (* (+ out-community out-module) total)
        c (* in-community total)
        d (* out-community total)
        e (* out-module total)]
    (- (- a (* b b)) (- c (* d d) (* e e)))))
