(ns lox.parse.common)

(defn init [tokens]
  "Initialize the current parser."
  {:tokens tokens :current 0 :statements []})

(defn parse-identity [{:keys [tokens current statements]}]
  "Sanitize the parser state to prevent state from escaping scope of statement constructors."
  {:tokens tokens :current current :statements statements})

(defn peek-token [{:keys [tokens current] :as parse}]
  "Return the current token without advancing the parser."
  (nth tokens current))

(defn is-finished? [{:keys [tokens current] :as parse}]
  "Check whether the parse is complete -- i.e. has encountered EOF."
  (= :lox.token/eof (:type (peek-token parse))))

(defn previous [{:keys [tokens current] :as parse}]
  "Retrieve the previous token from the current position of the parser."
  (let [n (max 0 (dec current))]
    (nth tokens n)))

(defn advance [{:keys [current] :as parse}]
  "Advance the parser one token, if not finished."
  (if (not (is-finished? parse))
    (assoc parse :current (inc current))
    parse))

(defn check [parse type]
  "Check that the current token matches the provided type."
  (if (is-finished? parse) false
      (= type (:type (peek-token parse)))))

(defn consume
  "Advance the parser, discarding (consuming) the current token in the process."
  ([parse type]
   (consume parse type (str "Could not consume " (name type))))
  ([{:keys [] :as parse} type message]
   (if (check parse type)
     (let [parse (advance parse)]
       (assoc parse :previous (previous parse)))
       (throw (Exception. message)))))

(defn match? [parse & types]
  "Check whether a type matches a provided set of types."
  (some #(check parse %) types))

(defn add-statement [{:keys [statements] :as parse} statement]
  "Add a fully constructed statement to the parser."
  (assoc parse :statements (conj statements statement)))

