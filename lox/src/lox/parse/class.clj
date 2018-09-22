(ns lox.parse.class
  (:require
   [lox.parse]
   [lox.parse.common]
   [lox.parse.function]))

(defn- get-methods [{:keys [] :as parse}]
  (loop [methods []]
    (if (and (not (lox.parse.common/check parse :lox.token/r-brace)) (not (lox.parse.common/is-finished? parse)))
      (recur (conj methods (lox.parse.function/function-declaration :lox.parse/method)))
      (assoc parse :methods methods))))

(defn- get-superclass [{:keys [] :as parse}]
  (assoc parse :super (if (lox.parse.common/match? :lox.token/less) (lox.parse.common/consume parse :lox.token/identifier))))

(defn ->Clazz [{:keys [current] :as parse}]
  "
  Consumes the following tokens in order to create a class statement:
    * A name identifier.
    * An optional superclas identifier.
    * An optional colleciton of functions (methods) representing the class's behavior.
  "
  (let [{:keys [name super methods] :as parse}
        (-> parse
            lox.parse.common/advance
            (lox.parse.common/consume :lox.token/identifier "Expected a class name.")
            (clojure.set/rename-keys {:previous :name})
            get-superclass
            (lox.parse.common/consume :lox.token/l-brace)
            get-methods
            (lox.parse.common/consume :lox.token/r-brace))]
    (lox.parse.common/parse-identity (lox.parse.common/add-statement parse (lox.statement/->Clazz name super methods)))))

