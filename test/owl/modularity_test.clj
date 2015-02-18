(ns owl.modularity-test
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [owl.network :as network]
            [owl.modularity :as owl]))

(def pre-network
  {1 [2 3 4] 2 [1 3 4] 3 [1 2 4] 4 [1 2 3 5]
   5 [6 7 8] 6 [5 7 8] 7 [5 6 8] 8 [6 7 8]})

(def network
  (network/weighted-network pre-network))

(def graph
  (owl/prepare-network network))

(deftest impact-test
  (testing "node-impact"
    (let [node (get-in graph [:network 1])
          impact (owl/node-impact node)]
      (is (> impact 0.239)))))

(deftest merge-communities-test
  (testing "merge-communities"
    (let [merged (owl/merge-communities graph)]
      (is (>= (-> merged :communities count) 2))
      (is (< (-> merged :communities count) (count pre-network))))))

(deftest flow-test
  (testing "flow-upwards"
    (let [merged (owl/merge-communities graph)
          network (:network merged)
          community-id (-> merged :communities keys first)
          community (-> merged :communities (get community-id))
          connections (owl/merge-connections network community :out)]
      (is
       (empty?
        (set/difference
         (-> connections keys set)
         (-> merged :communities keys set)))))))

(deftest merge-nodes-test
  (testing "merge-nodes"
    (let [merged (owl/merge-communities graph)
          network (:network merged)
          community-id (-> merged :communities keys first)
          community (-> merged :communities (get community-id))
          node (owl/merge-nodes network community-id community)]
      (is
       (empty?
        (set/difference
         (-> node :out keys set)
         (-> merged :communities keys set))))
      (println (str (:network merged)))
      (println (str (:communities merged)))
      (println (str community-id) (str community))
      (println (str node)))))


