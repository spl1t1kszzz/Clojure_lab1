(ns task-4-test
  (:require [clojure.test :refer :all]
            [task-4 :refer :all]))

(deftest test-constructors
  ;(is (= 'p (var 'p)))


  (is (= [:and 'p 'q] (and* 'p 'q)))
  (is (= [:or 'p 'q 'r] (or* 'p 'q 'r)))
  (is (= [:not 'p] (not* 'p)))
  (is (= [:impl 'p 'q] (impl* 'p 'q))))

(deftest test-substitute
  (is (= true (substitute 'p 'p true)))
  (is (= 'q (substitute 'q 'p true)))
  (is (= [:and true 'q]
         (substitute (and* 'p 'q) 'p true))))

(deftest test-to-dnf-simple
  ;; p → q  эквивалентно  ¬p ∨ q   - уже ДНФ
  (is (= (or* (not* 'p) 'q)
         (to-dnf (impl* 'p 'q)))))

(deftest test-to-dnf-with-and-or
  ;; (p → q) ∧ r  => (¬p ∨ q) ∧ r => (¬p ∧ r) ∨ (q ∧ r)
  (is (= (or* (and* (not* 'p) 'r)
              (and* 'q 'r))
         (to-dnf (and* (impl* 'p 'q) 'r)))))

(deftest test-substitute-and-dnf
  ;; (p → q) ∧ r, подставим p = true:
  ;; (true → q) ∧ r ≡ (¬true ∨ q) ∧ r ≡ (false ∨ q) ∧ r ≡ q ∧ r
  (is (= (and* 'q 'r)
         (substitute-and-dnf (and* (impl* 'p 'q) 'r)
                             'p
                             true))))


