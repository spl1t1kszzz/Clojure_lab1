(ns task-2)

(defn square [x] (* x x))

; left = (f(0) + f(x)) / 2
; right = sum (f(x_i))
(defn integrate [f x h]
  (let [left (/ (+ (f 0) (f x)) 2) right (reduce (fn [acc x] (+ acc (f x))) 0 (range 0 (+ x h) h))]
    (* h (+ left right)))
)

(def m_integrate (memoize (fn [f x h] (integrate f x h))))


(defn -main []
  (let [h 0.00000001]
    (println (time (integrate square 3 h)))
    (println (time (m_integrate square 3 h)))
    (println (time (m_integrate square 3 h)))
    ))