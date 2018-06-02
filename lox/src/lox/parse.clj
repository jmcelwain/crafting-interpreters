(ns lox.parse)

(defn init [tokens]
  {:tokens tokens :current 0 :statements []})

(defn is-finished? [{:keys [tokens current] :as parse}]
  (= :lox.token/eof (:type (nth tokens current))))

(defn parse [scan]
  (init (:tokens scan)))
