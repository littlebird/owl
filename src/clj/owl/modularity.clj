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
  ([weights] (sum-weights weights #{}))
  ([weights ignoring?]
     (reduce
      (fn [sum [id weight]]
        (if (ignoring? id)
          (+ sum 0)
          (+ sum weight)))
      0 weights)))

(defn prepare-network
  [network]
  (let [total (network/total-connections network)
        commune (reduce
                 (fn [network id]
                   (-> network
                       (assoc-in [id :community] id)
                       (assoc-in [id :total-weights]
                                 (+ (sum-weights (get-in network [id :in]))
                                    (sum-weights (get-in network [id :out]))))))
                 network (keys network))]
    {:network commune
     :total total
     :ratio (/ 1.0 total)
     :communities (into {} (map (fn [id] [id (set [id])]) (keys network)))
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
           ;; ratio (* 0.5 ratio)
           ratio 0.5

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
        connections (shuffle (node-connections node))]
    (if-let [connection 
             (first
              (drop-while
               (fn [connection]
                 (let [community-id (get-in network [connection :community])
                       community (get communities community-id)
                       modularity (modularity-difference graph id community)]
                   (< modularity 0)))
               connections))]
      (get-in network [connection :community]))))

(defn remove-from-community
  [communities id current]
  (dissoc communities current))

(defn join-community
  [graph id community]
  (p :join-community
     (let [current-id (get-in graph [:network id :community])
           current-community (get-in graph [:communities current-id])]
       (if (not= current-id community)
         (-> graph
             (assoc-in [:network id :community] community)
             (update-in [:communities] #(remove-from-community % id current-id))
             (update-in [:communities community] #(set/union % current-community)))
         graph))))

(defn merge-communities
  [graph]
  (reduce
   (fn [graph id]
     (let [community (find-community graph id)]
       (join-community graph id community)))
   graph (-> graph :network keys)))

(defn merge-communities
  [graph]
  (reduce
   (fn [graph id]
     (if-let [community (community-for graph id)]
       (join-community graph id community)
       graph))
   graph (-> graph :network keys)))
