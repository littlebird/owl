(ns owl.core
  (:require
   [owl.network :as network]
   [owl.betweenness :as betweenness]
   [owl.modularity :as modularity]))

(defn analyze-network
  [network]
  (let [betweenness (betweenness/network-betweenness network)
        weighted (network/weighted-network network)
        modularity (modularity/seek-unity weighted)]
    {:betweenness betweenness
     :modularity modularity}))

(defn output-network
  [{:keys [betweenness modularity]}]
  (let [network (:original modularity)
        unweighted (network/remove-network-weights network)]
    (map
     (fn [[id node]]
       (assoc node
         :identity id
         :betweenness (get betweenness id)))
     unweighted)))

