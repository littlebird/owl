(ns owl.algorithm.Between)

(defn get-val
  [map key default]
  (if (contains? map key) (key map) default))

(defn calculate
  [graph-map])
