(ns lox.parse.common)

(defn init [tokens]
  {:tokens tokens :current 0 :statements []})

(defn parse-identity [{:keys [tokens current statements]}]
  {:tokens tokens :current current :statements statements})

(defn peek-token [{:keys [tokens current] :as parse}]
  (nth tokens current))

(defn is-finished? [{:keys [tokens current] :as parse}]
  (= :lox.token/eof (:type (peek-token parse))))

(defn previous [{:keys [tokens current] :as parse}]
  (let [n (max 0 (dec current))]
    (nth tokens n)))

(defn advance [{:keys [current] :as parse}]
  (if (not (is-finished? parse))
    (assoc parse :current (inc current))
    parse))

(defn check [parse type]
  (if (is-finished? parse) false
      (= type (:type (peek-token parse)))))

(defn consume
  ([parse type]
   (consume parse type (str "Could not consume " (name type))))
  ([{:keys [] :as parse} type message]
   (if (check parse type)
     (let [parse (advance parse)]
       (assoc parse :previous (previous parse)))
       (throw (Exception. message)))))

(defn match? [parse & types]
  (some #(check parse %) types))

(defn add-statement [{:keys [statements] :as parse} statement]
  (assoc parse :statements (conj statements statement)))

