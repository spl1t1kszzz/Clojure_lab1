(ns task-2)

(defn square [x] (* x x))

; left = (f(0) + f(x)) / 2
; right = sum (f(x_i))
(defn integrate [f h]
  (fn [x] (let [left (/ (+ (f 0) (f x)) 2) right (reduce (fn [acc t] (+ acc (f t))) 0 (range 0 (+ x h) h))]
            (* h (+ left right)))))
; 2.1
(defn m_integrate [f h]
  (memoize (integrate f h)))

; 2.2
(defn integrate-seq [f h]
  (map second (iterate (fn [[x acc]] [(+ x h) (+ acc (* h 0.5 (+ (f x) (f (+ x h)))))]) [0 0])))

(defn make-integral [f h]
  (let [seqF (integrate-seq f h)]
    (fn [x] (nth seqF (int (/ x h))))))


(defn -main []
  (let [h 0.00001 F (m_integrate square h) F1 (make-integral square h)]
    (time (println (F 10)))
    (time (println (F 10)))
    (time (println "F(10):" (F1 10)))
    (time (println "F(100):" (F1 100)))
    (time (println "F(90):" (F1 90)))
    (time (println "F(200):" (F1 200)))
    (time (println "F(150):" (F1 150)))
    (time (println "F(199):" (F1 199)))
    (time (println "F(200):" (F1 200)))
    ))
