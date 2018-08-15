(ns lox.parse
  (:require
   [clojure.set]
   [lox.parse.class]
   [lox.parse.function]
   [lox.parse.var]
   [lox.parse.common]
   [lox.statement]))

::function
::method

(defn declaration [{:keys [] :as parse}]
  (cond
    (lox.parse.common/match? parse :lox.token/class) (lox.parse.class/class-declaration parse)
    (lox.parse.common/match? parse :lox.token/fun) (lox.parse.function/function-declaration parse :lox.parse/function)
    (lox.parse.common/match? parse :lox.token/var) (lox.parse.var/var-declaration parse)
    :else (lox.parse.common/add-statement parse (lox.statement/->Statement nil))))

(defn parse-statements [{:keys [statements] :as parse}]
  (if (lox.parse.common/is-finished? parse)
    (:statements parse)
    (parse-statements (declaration parse))))

(defn parse [tokens]
  (let [parse (lox.parse.common/init tokens)]
    (parse-statements parse)))
