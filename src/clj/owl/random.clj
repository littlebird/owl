(ns owl.random)

(defn make-mass
  [total branch event tree]
  {:total total
   :branch branch
   :event event
   :tree tree})

(def empty-mass (make-mass 0.0 0.0 nil nil))

(defn add-event
  [{:keys [total branch event tree] :as mass} novel probability]
  (if event
    (make-mass
     (+ total probability) branch nil
     [mass (add-event empty-mass novel probability)])
    (if tree
      (let [[left right] tree]
        (if (> (- total branch) branch)
          (make-mass (+ total probability) (+ branch probability) nil
           [(add-event left novel probability) right])
          (make-mass (+ total probability) branch nil
           [left (add-event right novel probability)])))
      (make-mass (+ total probability) probability novel nil))))

(defn sample
  ([mass] (sample mass (* (rand) (:total mass))))
  ([{:keys [total branch event tree] :as mass} point]
     (if mass
       (if event
         event
         (let [[left right] tree]
           (if (< point branch)
             (recur left point)
             (recur right (- point branch))))))))

