(ns task-1)

; 1.1
(defn correct_strings [alphabet n]
  (cond
    (= n 0) '()
    (= n 1) (map str alphabet)
    :else   (for [p (correct_strings alphabet (dec n))
                  c alphabet
                  :when (not= (last p) c)]
              (str p c))))


; 1.2
(defn correct_strings_tail [alphabet n]
  (if (= n 0)
    '()
    (loop [acc (map str alphabet) k 1]
      (if (= k n)
        acc
        (recur (for [p acc c alphabet
                     :when (not= (last p) c)]
                 (str p c))
               (inc k))))))


; 1.3
(defn my_map [f coll]
  (reduce (fn [acc elem] (conj acc (f elem))) [] coll))

(defn my_filter [pred coll]
  (reduce (fn [acc elem] (if (pred elem) (conj acc elem) acc)) [] coll))

; 1.4
(defn correct_strings_2 [alphabet n]
  (cond
    (= n 0) '()
    (= n 1) (map str alphabet)
    :else
    (loop [acc (map str alphabet) k 1]
      (if (= k n)
        acc
        (recur (reduce (fn [new-acc p] (reduce (fn [new-acc2 c] (if (not= (last p) c) (conj new-acc2 (str p c)) new-acc2)) new-acc alphabet)) '() acc)
          (inc k))))))




(defn -main []
  (println "1.1:" (correct_strings '(\a \b \c) 3))
  (println "1.2:" (correct_strings_tail '(\a \b \c) 3))
  (println "1.3 map:" (my_map (fn [elem] (* 2 elem)) [1 2 3 4 5 6 7 8 9 10]))
  (println "1.3 filter:" (my_filter (fn [elem] (= 0 (mod elem 2))) [1 2 3 4 5 6 7 8 9 10]))
  (println "1.4:" (correct_strings_2 '(\a \b \c) 3 ))
  )
