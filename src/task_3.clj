(ns task-3)

(defn parallel_filter [pred coll block_size]
  (loop [blocks coll futures []]
    (if (empty? blocks)
      (apply concat (map deref futures))
      (let [block (vec (take block_size blocks)) fut (future (doall (filter pred block)))]
        (recur (drop block_size blocks)
               (conj futures fut))))))

(defn lazy-parallel-filter [pred coll block-size]
  (letfn [(spawn_futures [s]
            (if-let [blocks (seq s)]
              (cons (future (doall (filter pred (take block-size blocks)))) (spawn_futures (drop block-size blocks)))
              ()))

          (collect_results [fs]
            (lazy-seq
              (if-let [f (seq fs)]
                (concat (deref (first f)) (collect_results (rest f)))
                ())))]

    (collect_results (spawn_futures coll))))


(defn is_prime [n]
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

(defn run_test [block-size]
  (let [data (range 1 2001)
        pred is_prime]

    (println "Обычный filter")
    (let [res (time (doall (filter pred data)))]
      (println (take 20 res)))

    (println "Параллельный filter")
    (let [res (time (parallel_filter pred data block-size))]
      (println (take 20 res)))

    (println "Ленивый параллельный filter")
    (let [res (time (doall (lazy-parallel-filter pred data block-size)))]
      (println (take 20 res)))))


(defn -main []
  (run_test 400)
)