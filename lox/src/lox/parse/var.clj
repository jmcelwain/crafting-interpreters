(ns lox.parse.var)

(defn get-initializer [{:keys [] :as parse}]
  ;; TODO: Impl
  (if (or true (lox.parse.common/check parse :lox.token/equal))
    parse))

(defn ->Var [{:keys [] :as parse}]
  (let [[parse name] (lox.parse.common/consume parse :lox.token/identifier "Expected a variable name.")
        [parse init] (get-initializer parse)])
  [parse nil])

