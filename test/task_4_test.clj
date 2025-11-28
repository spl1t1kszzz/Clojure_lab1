(ns task-4-test
  (:require [clojure.test :refer :all] [task-4 :refer :all]))

(deftest test-constructors
  (is (= (symbol "a") (make-var "a")))
  (is (= [:and 'a 'b] (and_ 'a 'b)))
  (is (= [:or  'a 'b 'c] (or_ 'a 'b 'c)))
  (is (= [:not 'a] (not_ 'a)))
  (is (= [:impl 'a 'b] (impl_ 'a 'b))))


(deftest test-substitute
  (is (= true (substitute 'a 'a true)))
  (is (= 'b (substitute 'b 'a true)))
  (is (= [:and true 'b] (substitute (and_ 'a 'b) 'a true))))

(deftest test-morgan_rules
  ; ¬(a ∧ b) → (¬a ∨ ¬b)
  (is (= (or_ (not_ 'a) (not_ 'b)) (replace_not (not_ (and_ 'a 'b)))))

  (is (= (and_ (not_ 'a) (not_ 'b)) (replace_not (not_ (or_ 'a 'b)))))

  (is (= 'a (replace_not (not_ (not_ 'a)))))

  (is (= false (replace_not (not_ true))))

  (is (= true (replace_not (not_ false)))))

(deftest test-distribute
  ; (p ∨ q) ∧ r → (p ∧ r) ∨ (q ∧ r)
  (is (= (or_ (and_ 'a 'c) (and_ 'b 'c))
         (distribute (or_ 'a 'b) 'c)))

  ; p ∧ (q ∨ r) → (p ∧ q) ∨ (p ∧ r)
  (is (= (or_ (and_ 'a 'b) (and_ 'a 'c))
         (distribute 'a (or_ 'b 'c))))

  ; (p ∨ q) ∧ (r ∨ s)
  (is (= (or_ (and_ 'a 'c) (and_ 'a 'd) (and_ 'b 'c) (and_ 'b 'd))
         (distribute (or_ 'a 'b) (or_ 'c 'd)))))

(deftest test-to-dnf-simple
  ; a → b ≡ ¬a ∨ b
  (is (= (or_ (not_ 'a) 'b) (to-dnf (impl_ 'a 'b)))))

(deftest test-to-dnf-with-and-or
  (is (= (or_ (and_ (not_ 'a) 'c) (and_ 'b 'c)) (to-dnf (and_ (impl_ 'a 'b) 'c)))))

(deftest test-to-dnf-deep
  ; F = ¬((X → Y) ∨ ¬(Y → Z))
  ; F = (X ∧ ¬Y) ∨ (X ∧ ¬Y ∧ Z)
  (is (= (or_ (and_ 'x (not_ 'y)) (and_ 'x (not_ 'y) 'z))
         (to-dnf (not_ (or_ (impl_ 'x 'y) (not_ (impl_ 'y 'z))))))))

(deftest test-substitute-and-dnf-basic
  ; (a → b) ∧ c, a = true
  ; (true → b) ∧ c
  ; (¬true ∨ b) ∧ c
  ; (false ∨ b) ∧ c
  ; b ∧ c
  (is (= (and_ 'b 'c) (substitute-and-dnf (and_ (impl_ 'a 'b) 'c) 'a true))))
