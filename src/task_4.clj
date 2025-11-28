(ns task-4)

;; === Базовые конструкторы ========================================

(defn var
  "Создаёт переменную-символ по строке."
  [name]
  (symbol name))

(defn and*
  "Конъюнкция. Принимает произвольное число аргументов.
   (and* a b c) => [:and a b c]"
  [& args]
  (into [:and] args))

(defn or*
  "Дизъюнкция. Принимает произвольное число аргументов.
   (or* a b c) => [:or a b c]"
  [& args]
  (into [:or] args))

(defn not*
  "Отрицание."
  [e]
  [:not e])

(defn impl*
  "Импликация a → b."
  [a b]
  [:impl a b])

;; === Вспомогательные функции ======================================

(defn literal?
  "Истинно, если выражение - булева константа или переменная."
  [e]
  (or (symbol? e)
      (true? e)
      (false? e)))

(defn node?
  "Истинно, если выражение - вектор вида [op & args]."
  [e]
  (and (vector? e)
       (keyword? (first e))))

(defn op
  "Возвращает операцию-ключевое слово или nil."
  [e]
  (when (node? e) (first e)))

(defn and-node? [e] (= (op e) :and))
(defn or-node?  [e] (= (op e) :or))
(defn not-node? [e] (= (op e) :not))
(defn impl-node? [e] (= (op e) :impl))


(defn substitute
  "Подставляет значение value вместо переменной v в выражение expr.
   value может быть либо булевым значением, либо другим выражением."
  [expr v value]
  (cond
    (symbol? expr) (if (= expr v) value expr)
    (literal? expr) expr
    (node? expr) (into [(op expr)]
                       (map #(substitute % v value) (rest expr)))
    :else expr))


(defn elim-impl
  "Убирает импликации из выражения, переписывая (a → b) как (¬a ∨ b)."
  [expr]
  (cond
    (literal? expr) expr

    (impl-node? expr)
    (let [[_ a b] expr]
      (or* (not* (elim-impl a))
           (elim-impl b)))

    (node? expr)
    (into [(op expr)] (map elim-impl (rest expr)))

    :else expr))



(defn nnf
  "Приводит выражение к ННФ (отрицания только над литералами)."
  [expr]
  (cond
    (literal? expr) expr

    (not-node? expr)
    (let [[_ e] expr]
      (cond
        (literal? e) [:not e]
        (not-node? e) (nnf (second e))                 ; ¬¬A => A
        (and-node? e)                                  ; ¬(A ∧ B) => (¬A ∨ ¬B)
        (apply or* (map #(nnf (not* %)) (rest e)))
        (or-node? e)                                   ; ¬(A ∨ B) => (¬A ∧ ¬B)
        (apply and* (map #(nnf (not* %)) (rest e)))
        :else (not* (nnf e))))

    (and-node? expr)
    (apply and* (map nnf (rest expr)))

    (or-node? expr)
    (apply or* (map nnf (rest expr)))

    (node? expr)
    (into [(op expr)] (map nnf (rest expr)))

    :else expr))


(defn distribute-and
  "Распределяет конъюнкцию по дизъюнкции:
   (A ∧ (B ∨ C)) и т.п. Возвращает логически эквивалентное выражение."
  [a b]
  (cond
    (and (or-node? a) (or-node? b))
    (apply or*
           (for [da (rest a)
                 db (rest b)]
             (and* da db)))

    (or-node? a)
    (apply or*
           (for [da (rest a)]
             (and* da b)))

    (or-node? b)
    (apply or*
           (for [db (rest b)]
             (and* a db)))

    :else
    (and* a b)))

(defn dnf*
  "Предполагает, что expr уже в ННФ. Возвращает выражение в ДНФ."
  [expr]
  (cond
    (literal? expr) expr

    (not-node? expr) expr   ; уже ¬литерал в ННФ

    (or-node? expr)
    (apply or* (map dnf* (rest expr)))

    (and-node? expr)
    (let [terms (map dnf* (rest expr))]
      (if (= 1 (count terms))
        (first terms)
        (reduce distribute-and (first terms) (rest terms))))

    :else expr))


(defn simplify
  "Упрощает выражение:
   - сворачивает операции с true/false,
   - убирает вложенные [:and ... [:and ...] ...] и [:or ... [:or ...] ...]."
  [expr]
  (cond
    (literal? expr) expr

    (not-node? expr)
    (let [[_ e] expr
          e' (simplify e)]
      (if (boolean? e')
        (not e')
        [:not e']))

    (and-node? expr)
    (let [args (->> (rest expr)
                    (map simplify)
                    (mapcat #(if (and-node? %)
                               (rest %)
                               [%])))
          args (remove true? args)]
      (cond
        (some false? args) false
        (empty? args) true
        (= 1 (count args)) (first args)
        :else (into [:and] args)))

    (or-node? expr)
    (let [args (->> (rest expr)
                    (map simplify)
                    (mapcat #(if (or-node? %)
                               (rest %)
                               [%])))
          args (remove false? args)]
      (cond
        (some true? args) true
        (empty? args) false
        (= 1 (count args)) (first args)
        :else (into [:or] args)))

    (node? expr)
    (into [(op expr)] (map simplify (rest expr)))

    :else expr))


(defn to-dnf
  "Приводит логическое выражение expr к ДНФ.
   Допускаются операции: :and, :or, :not, :impl."
  [expr]
  (-> expr
      elim-impl
      nnf
      dnf*
      simplify))

(defn substitute-and-dnf
  "Подставляет значение value вместо переменной v в выражение expr
   и приводит результат к ДНФ."
  [expr v value]
  (to-dnf (substitute expr v value)))
