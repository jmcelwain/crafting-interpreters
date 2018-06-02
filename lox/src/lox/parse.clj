(ns lox.parse)

(defn init [tokens]
  {:tokens tokens :current 0 :statements []})

(defn is-finished? [{:keys [tokens current] :as parse}]
  (= :lox.token/eof (:type (peek parse))))

(defn peek [{:keys [tokens current] :as parse}]
  (nth tokens current))

(defn previous [{:keys [tokens current] :as parse}]
  (nth tokens (dec current)))

(defn advance [{:keys [current] :as parse}]
  (if (is-finished? parse)
    (previous parse)
    (assoc parse :current (inc current))))

(defn check [parse type]
  (if (is-finished? parse) false
      (= type (peek parse))))

(defn consume [{:keys [] :as parse} type message]
  (if (check parse type)
    (advance parse)
    (throw (Exception. message))))

(defn parse [scan]
  (init (:tokens scan)))
