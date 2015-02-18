(ns owl.modularity
  (:require
   [clojure.set :as set]
   [taoensso.timbre :as timbre]
   [owl.network :as network]))

(timbre/refer-timbre)

(defn node-impact
  [node]
  (reduce
   + 0
   (concat
    (vals (get node :in))
    (vals (get node :out)))))

(defn edge-set
  [node direction]
  (set (keys (get node direction))))

(defn sum-weights
  ([weights]
     (reduce + 0 (map last weights)))
  ([weights ignoring?]
     (reduce
      (fn [sum [id weight]]
        (if (ignoring? id)
          (+ sum 0)
          (+ sum weight)))
      0 weights)))

(defn initial-communities
  [network]
  (into
   {}
   (map
    (fn [id]
      [id (set [id])])
    (keys network))))

(defn prepare-network
  [network]
  (let [pared (into {} (remove #(zero? (+ (-> % last :in count) (-> % last :out count))) network))
        commune (reduce
                 (fn [network id]
                   (-> network
                       (assoc-in [id :community] id)
                       (assoc-in [id :total-weights]
                                 (+ (sum-weights (get-in network [id :in]))
                                    (sum-weights (get-in network [id :out]))))))
                 pared (keys pared))
        total (network/total-weights commune)]
    {:network commune
     :total total
     :ratio (/ 1.0 total)
     :communities (initial-communities commune)
     :impact (network/map-vals node-impact commune)}))

(defn weights-within
  [network community]
  (reduce
   (fn [sum id]
     (let [weights (get-in network [id :out])
           in (-> weights (select-keys community) vals)]
       (+ sum (reduce + 0 in))))
   0 community))

(defn weights-without
  [network community]
  (reduce
   (fn [sum id]
     (let [node (get network id)
           in (sum-weights (get node :in) community)
           out (sum-weights (get node :out) community)]
       (+ sum in out)))
   0 community))

(defn node-relation
  [network community to]
  (reduce
   (fn [sum id]
     (let [node (get network id)
           in (get-in node [:in to] 0.0)
           out (get-in node [:out to] 0.0)]
       (+ sum in out)))
   0 community))

(defn community-weight
  [network community]
  (reduce + 0 (map (fn [id] (get-in network [id :total-weights])) community)))

(defn modularity-difference
  [{:keys [network ratio communities impact]} id community]
  (p :modularity
     (let [in-community (p :within (* 2.0 (weights-within network community)))
           total-community (p :total (community-weight network community))
           out-community (- total-community in-community)
           relation (p :relation (node-relation network community id))
           node-impact (get impact id)
           ratio (* 0.5 ratio)

           a (* (+ in-community relation) ratio)
           b (* (+ out-community node-impact) ratio)
           c (* in-community ratio)
           d (* out-community ratio)
           e (* node-impact ratio)]
       (- (- a (* b b)) (- c (* d d) (* e e))))))

(defn node-connections
  [node]
  (p :connections
     (set/union
      (-> node :out keys set)
      (-> node :in keys set))))

(defn communities-for
  [network connections]
  (set (map :community (vals (select-keys network connections)))))

(defn find-community
  [{:keys [network communities] :as graph} id]
  (p :find-community
     (let [node (get network id)
           connections (node-connections node)
           potential (communities-for network connections)]
       (if (empty? connections)
         id
         (first
          (sort-by
           (fn [community]
             (let [members (get communities community)]
               (modularity-difference graph id members)))
           > potential))))))

(defn community-for?
  [{:keys [network communities] :as graph} id]
  (let [node (get network id)
        connection (-> node :out keys first)
        community-id (get-in network [connection :community])
        community (get communities community-id)
        modularity (modularity-difference graph id community)]
    (if (> modularity 0)
      community-id)))

(defn community-for
  [{:keys [network communities] :as graph} id]
  (let [node (get network id)
        current-id (:community node)
        connections (shuffle (node-connections node))]
    (if (= 1 (count (get communities current-id)))
      (if-let [connection 
               (first
                (drop-while
                 (fn [connection]
                   (let [community-id (get-in network [connection :community])]
                     (if (not= community-id current-id)
                       (let [community (get communities community-id)
                             modularity (modularity-difference graph id community)]
                         (< modularity 0)))))
                 connections))]
        (get-in network [connection :community])))))

(defn join-community
  [graph id community]
  (let [current-id (get-in graph [:network id :community])
        current-community (get-in graph [:communities current-id])
        new-community (get-in graph [:communities community])]
    (if (= current-id community)
      graph
      (-> (reduce
           (fn [graph current]
             (assoc-in graph [:network current :community] community))
           graph current-community)
          (update-in [:communities] #(dissoc % current-id))
          (update-in [:communities community] #(set/union % current-community))))))

(defn merge-communities
  [graph]
  (reduce
   (fn [graph id]
     (if-let [community (community-for graph id)]
       (join-community graph id community)
       graph))
   graph (-> graph :network keys)))

(defn flow-upward
  [network to community]
  (reduce
   (fn [node connections]
     (reduce
      (fn [node [connection weight]]
        (if (community connection)
          node
          (let [upward (get-in network [connection :community])]
            (update-in node [upward] #(+ (or % 0) weight)))))
      node connections))
   {} to))

(defn merge-connections
  [network community direction]
  (let [connections
        (map
         (fn [id]
           (get-in network [id direction]))
         community)
        merged (flow-upward network connections community)]
    merged))

(defn merge-nodes
  [network id community]
  (let [in (merge-connections network community :in)
        out (merge-connections network community :out)]
    {:in in
     :out out
     :community id
     :total-weights (+ (sum-weights in) (sum-weights out))}))

(defn ascend-level
  [{:keys [network total ratio communities] :as graph}]
  (let [commune
        (reduce
         (fn [above [id community]]
           (let [aggregate (merge-nodes network id community)]
             (assoc above id aggregate)))
         {} communities)
        total (network/total-weights commune)]
    {:network commune
     :total total
     :ratio (/ 1.0 total)
     :communities (initial-communities commune)
     :impact (network/map-vals node-impact commune)
     :sublevel graph}))

(defn agglomerate
  [graph]
  (iterate
   (comp merge-communities ascend-level)
   (merge-communities graph)))

(defn unified?
  [graph]
  (or
   (= 1 (-> graph :communities count))
   (every?
    (partial = 1)
    (map count (-> graph :communities vals)))))

(defn seek-unity
  [network]
  (let [graph (prepare-network network)
        glom (agglomerate graph)
        quest (drop-while (comp not unified?) glom)]
    (first quest)))
