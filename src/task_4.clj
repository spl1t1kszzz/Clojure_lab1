(ns task-4)

(defn make-var
  "Создаёт переменную-символ по строке."
  [name]
  (symbol name))


(defn and_
  "Конъюнкция: (and_ a b c) => [:and a b c]"
  [& args]
  (into [:and] args))

(defn or_
  "Дизъюнкция: (or_ a b c) => [:or a b c]"
  [& args]
  (into [:or] args))

(defn not_
  "Отрицание"
  [e]
  [:not e])

(defn impl_
  "Импликация: a → b"
  [a b]
  [:impl a b])

(defn is_literal
  "Истинно, если выражение - булева константа или переменная."
  [e]
  (or (symbol? e) (true? e) (false? e)))

(defn is_node
  "Истинно, если выражение - вектор вида [op & args]."
  [e]
  (and (vector? e) (keyword? (first e))))

(defn op
  "Возвращает операцию-ключевое слово или nil."
  [e] (when (is_node e) (first e)))

(defn is_and_node [e] (= (op e) :and))
(defn is_or_node  [e] (= (op e) :or))
(defn is_not_node [e] (= (op e) :not))
(defn is_impl_node [e] (= (op e) :impl))


(defn substitute
  "Подставляет значение value вместо переменной v в выражение expr."
  [expr v value]
  (cond
    (is_literal expr) (if (= expr v) value expr)
    (is_node expr) (into [(op expr)] (map #(substitute % v value) (rest expr)))
    :else expr))


(defn remove_impl
  "Убирает импликации из выражения, переписывая (a → b) как (¬a ∨ b)."
  [expr]
  (cond
    (is_literal expr) expr
    (is_impl_node expr) (let [[_ a b] expr] (or_ (not_ (remove_impl a)) (remove_impl b)))
    (is_node expr) (into [(op expr)] (map remove_impl (rest expr)))
    :else expr))



(defn replace_not
  "Заменяет отрицания по законам де Моргана.
   Отрицание над литералами обрабатывается напрямую."
  [expr]
  (cond
    (is_literal expr)
    expr

    (is_not_node expr)
    (let [[_ e] expr]
      (cond
        (true? e) false
        (false? e) true
        (is_literal e) [:not e]
        (is_not_node e) (replace_not (second e))
        (is_and_node e) (apply or_ (map #(replace_not (not_ %)) (rest e)))
        (is_or_node e) (apply and_ (map #(replace_not (not_ %)) (rest e)))
        :else (not_ (replace_not e))))

    (is_and_node expr)
    (apply and_ (map replace_not (rest expr)))

    (is_or_node expr)
    (apply or_ (map replace_not (rest expr)))

    (is_node expr)
    (into [(op expr)] (map replace_not (rest expr)))

    :else expr))



(defn distribute
  "Раскрывает скобки по закону дистрибутивности"
  [a b]
  (cond
    (and (is_or_node a) (is_or_node b)) (apply or_ (for [da (rest a) db (rest b)] (and_ da db)))
    (is_or_node a) (apply or_ (for [da (rest a)] (and_ da b)))
    (is_or_node b) (apply or_ (for [db (rest b)] (and_ a db)))
    :else
    (and_ a b)))

(defn dnf*
  "Возвращает выражение в ДНФ"
  [expr]
  (cond
    (is_literal expr) expr

    (is_not_node expr) expr ; [:not literal]

    (is_or_node expr) (apply or_ (map dnf* (rest expr)))

    (is_and_node expr)
    (let [terms (map dnf* (rest expr))]
      (if (= 1 (count terms))
        (first terms)
        (reduce distribute (first terms) (rest terms))))

    :else expr))


(defn simplify
  "Упрощает выражение"
  [expr]
  (cond
    (is_literal expr) expr

    (is_not_node expr)
    (let [[_ e] expr e' (simplify e)]
      (if (boolean? e')
        (not e')
        [:not e']))

    (is_and_node expr)
    (let [args (->> (rest expr)
                    (map simplify)
                    (mapcat #(if (is_and_node %)
                               (rest %)
                               [%])))
          args (remove true? args)]
      (cond
        (some false? args) false
        (empty? args) true
        (= 1 (count args)) (first args)
        :else (into [:and] args)))

    (is_or_node expr)
    (let [args (->> (rest expr)
                    (map simplify)
                    (mapcat #(if (is_or_node %)
                               (rest %)
                               [%])))
          args (remove false? args)]
      (cond
        (some true? args) true
        (empty? args) false
        (= 1 (count args)) (first args)
        :else (into [:or] args)))

    (is_node expr)
    (into [(op expr)] (map simplify (rest expr)))

    :else expr))


(defn to-dnf
  "Приводит логическое выражение expr к ДНФ"
  [expr]
  (-> expr
      remove_impl
      replace_not
      dnf*
      simplify))

(defn substitute-and-dnf
  "Подставляет значение value вместо переменной v в выражение expr и приводит результат к ДНФ."
  [expr v value]
  (to-dnf (substitute expr v value)))
