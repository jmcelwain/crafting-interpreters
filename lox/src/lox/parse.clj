(ns lox.parse
  (:require
   [clojure.set]
   [lox.statement]))

::function
::method

(declare
 ;; util
 peek-token
 is-finished?
 previous
 advance
 consume
 match?

 ;; function
 function-declaration
 get-params
 block

 ;; class
 class-declaration
 get-methods
 get-superclass

 ;; var
 var-declaration
 get-initializer

 ;; main
 parse-statements
 parse)

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

(defn add-params [{:keys [previous params] :as parse}]
  (assoc parse :params (conj params previous)))

(defn get-params [{:keys [] :as parse}]
  (if (not (check parse :lox.token/r-paren))
    (loop [{:keys [] :as parse} parse]
      (let [parse (-> parse
                      (consume :lox.token/identifier)
                      add-params)]

        (if (match? parse :lox.token/comma)
          (recur (advance parse))
          (advance parse))))
    (assoc parse :params [])))

(defn get-methods [{:keys [] :as parse}]
  (loop [methods []]
    (if (and (not (check parse :lox.token/r-brace)) (not (is-finished? parse)))
      (recur (conj methods (function-declaration ::method)))
      (assoc parse :methods methods))))

(defn get-superclass [{:keys [] :as parse}]
  (assoc parse :super (if (match? :lox.token/less) (consume parse :lox.token/identifier))))

(defn add-statement [{:keys [statements] :as parse} statement]
  (assoc parse :statements (conj statements statement)))

(defn class-declaration [{:keys [current] :as parse}]
  (let [{:keys [name super methods] :as parse}
        (-> parse
            advance
            (consume :lox.token/identifier "Expected a class name.")
            (clojure.set/rename-keys {:previous :name})
            get-superclass
            (consume :lox.token/l-brace)
            get-methods
            (consume :lox.token/r-brace))]
    (parse-identity (add-statement parse (lox.statement/->Clazz name super methods)))))

(defn get-initializer [{:keys [] :as parse}]
  ;; TODO: Impl
  (if (or true (check parse :lox.token/equal))
    parse))

(defn var-declaration [{:keys [] :as parse}]
  (let [[parse name] (consume parse :lox.token/identifier "Expected a variable name.")
        [parse init] (get-initializer parse)])
  [parse nil])

(defn declaration [{:keys [] :as parse}]
  (cond
    (match? parse :lox.token/class) (class-declaration parse)
    (match? parse :lox.token/fun) (function-declaration parse ::function)
    (match? parse :lox.token/var) (var-declaration parse)
    :else (add-statement parse (lox.statement/->Statement nil))))

(defn get-block-statements [parse]
  (loop [statements []]
    (if (and (not (check parse :lox.token/r-brace)) (not (is-finished? parse)))
      (recur (declaration parse))
      (assoc parse :statements statements))))

(defn block [{:keys [] :as parse}]
  (let [parse
        (-> parse
            get-block-statements
            (consume :lox.token/r-brace "Expect } after block."))]
    parse))

(defn function-declaration [{:keys [] :as parse} type]
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
    (parse-identity (add-statement parse (lox.statement/->Function name params body)))))

(defn parse-statements [{:keys [statements] :as parse}]
  (if (is-finished? parse)
    (:statements parse)
    (parse-statements (declaration parse))))

(defn parse [tokens]
  (let [parse (init tokens)]
    (parse-statements parse)))
