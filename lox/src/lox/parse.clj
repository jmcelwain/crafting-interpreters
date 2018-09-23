(ns lox.parse
  (:require
   [lox.statement]
   [clojure.set]))

(declare check consume match? add-statement is-finished?
         ->Clazz ->Function ->Var function-declaration)

::function
::method

;; var

(defn- get-initializer [{:keys [] :as parse}]
  ;; TODO: Impl
  (if (or true (check parse :lox.token/equal))
    parse))

(defn ->Var [{:keys [] :as parse}]
  "Constructs an optionally initialized variable."
  (let [[parse name] (consume parse :lox.token/identifier "Expected a variable name.")
        [parse init] (get-initializer parse)])
  [parse nil])

;; declaration

(defn declaration [{:keys [] :as parse}]
  (cond
    (match? parse :lox.token/class) (->Clazz parse)
    (match? parse :lox.token/fun) (->Function parse :lox.parse/function)
    (match? parse :lox.token/var) (->Var parse)
    :else (add-statement parse (lox.statement/->Statement nil))))

;; common

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



;; class

(defn- get-methods [{:keys [] :as parse}]
  (loop [methods []]
    (if (and (not (check parse :lox.token/r-brace)) (not (is-finished? parse)))
      (recur (conj methods (function-declaration :lox.parse/method)))
      (assoc parse :methods methods))))

(defn- get-superclass [{:keys [] :as parse}]
  (assoc parse :super (if (match? :lox.token/less) (consume parse :lox.token/identifier))))

(defn ->Clazz [{:keys [current] :as parse}]
  "
  Consumes the following tokens in order to create a class statement:
    * A name identifier.
    * An optional superclas identifier.
    * An optional colleciton of functions (methods) representing the class's behavior.
  "
  (let [{:keys [name super methods] :as parse}
        (-> parse
            advance
            (consume :lox.token/identifier "Expected a class name.")
            (clojure.set/rename-keys {:previous :name})
            get-superclass
            (consume :lox.token/l-brace)
            get-methods
            (consume :lox.token/r-brace))]
    (parse-identity (add-statement parse (->Clazz name super methods)))))



;; function

(defn- add-params [{:keys [previous params] :as parse}]
  (assoc parse :params (conj params previous)))

(defn- get-params [{:keys [] :as parse}]
  (if (not (check parse :lox.token/r-paren))
    (loop [{:keys [] :as parse} parse]
      (let [parse (-> parse
                      (consume :lox.token/identifier)
                      add-params)]

        (if (match? parse :lox.token/comma)
          (recur (advance parse))
          (advance parse))))
    (assoc parse :params [])))

(defn- get-block-statements [parse]
  (loop [statements []]
    (if (and (not (check parse :lox.token/r-brace)) (not (is-finished? parse)))
      (recur (declaration parse))
      (assoc parse :statements statements))))

(defn- block [{:keys [] :as parse}]
  (let [parse
        (-> parse
            get-block-statements
            (consume :lox.token/r-brace "Expect } after block."))]
    parse))

(defn ->Function [{:keys [] :as parse} type]
  "
  Constructor for a function statement. This function can be first class, or a method in the body of a class. Consumes the following tokens:
    * An identifier for the function (or method name).
    * An optional collection of params (identifiers).
    * A function body / block.
  "
  (let [{:keys [name params body] :as parse}
        (-> parse
            advance
            (consume :lox.token/identifier (str "Expected a " (name type) " name."))
            (clojure.set/rename-keys {:previous :name})
            (consume :lox.token/l-paren (str "Expect ( after " (name type) " name."))
            get-params
            (consume :lox.token/r-paren (str "Expect ) after params."))
            (consume :lox.token/l-brace (str "Expect { " (name type) " body."))
            block
            advance)]
    (parse-identity (add-statement parse (->Function name params body)))))

(defn parse-statements [{:keys [statements] :as parse}]
  (if (is-finished? parse)
    (:statements parse)
    (parse-statements (declaration parse))))

(defn parse [tokens]
  (let [parse (init tokens)]
    (parse-statements parse)))
