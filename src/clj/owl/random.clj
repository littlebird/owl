(ns owl.random)

(defn make-density
  [total branch event tree]
  {:total total
   :branch branch
   :event event
   :tree tree})

(def empty-density (make-density 0.0 0.0 nil nil))

(defn add-event
  [{:keys [total branch event tree] :as density} novel probability]
  (if event
    (make-density
     (+ total probability) branch nil
     [density (add-event empty-density novel probability)])
    (if tree
      (let [[left right] tree]
        (if (> (- total branch) branch)
          (make-density (+ total probability) (+ branch probability) nil
           [(add-event left novel probability) right])
          (make-density (+ total probability) branch nil
           [left (add-event right novel probability)])))
      (make-density (+ total probability) probability novel nil))))

(defn sample
  ([density] (sample density (* (rand) (:total density))))
  ([{:keys [total branch event tree] :as density} point]
     (println point)
     (if density
       (if event
         event
         (let [[left right] tree]
           (if (< point branch)
             (recur left point)
             (recur right (- point branch))))))))
