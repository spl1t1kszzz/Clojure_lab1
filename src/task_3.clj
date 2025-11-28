(ns task-3)

(defn parallel-filter-optimal
  [pred coll block-size]
  (let [v (vec coll)
        n (count v)
        starts (range 0 n block-size)
        futures (map (fn [start]
                       (future
                         (let [end (min n (+ start block-size))]
                           (loop [i start result []]
                             (if (< i end)
                               (let [item (v i)]
                                 (recur (inc i)
                                        (if (pred item)
                                          (conj result item)
                                          result)))
                               result)))))
                     starts)
        results (map deref futures)]
    (apply concat results)))


(defn prime? [n]
  (if (< n 2)
    false
    (let [_ (Thread/sleep 1)
          is-prime (cond
                     (< n 2) false
                     (= n 2) true
                     (even? n) false
                     :else
                     (let [limit (inc (int (Math/sqrt n)))]
                       (loop [i 3]
                         (cond
                           (> i limit) true
                           (zero? (mod n i)) false
                           :else (recur (inc i))))))]
      is-prime)))

(defn test-performance [block-size]
  (let [data (range 1 10001) pred prime?]
    (time (doall (filter pred data)))
    (time (doall (parallel-filter-optimal pred data block-size)))))


(defn -main []
  (test-performance 2000)
)