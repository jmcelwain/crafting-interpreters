(ns lox.parse.function
  (:require
   [lox.parse]
   [lox.parse.common]))

(defn add-params [{:keys [previous params] :as parse}]
  (assoc parse :params (conj params previous)))

(defn get-params [{:keys [] :as parse}]
  (if (not (lox.parse.common/check parse :lox.token/r-paren))
    (loop [{:keys [] :as parse} parse]
      (let [parse (-> parse
                      (lox.parse.common/consume :lox.token/identifier)
                      add-params)]

        (if (lox.parse.common/match? parse :lox.token/comma)
          (recur (lox.parse.common/advance parse))
          (lox.parse.common/advance parse))))
    (assoc parse :params [])))

(defn get-block-statements [parse]
  (loop [statements []]
    (if (and (not (lox.parse.common/check parse :lox.token/r-brace)) (not (lox.parse.common/is-finished? parse)))
      (recur (lox.parse/declaration parse))
      (assoc parse :statements statements))))

(defn block [{:keys [] :as parse}]
  (let [parse
        (-> parse
            get-block-statements
            (lox.parse.common/consume :lox.token/r-brace "Expect } after block."))]
    parse))

(defn function-declaration [{:keys [] :as parse} type]
  (let [{:keys [name params body] :as parse}
        (-> parse
            lox.parse.common/advance
            (lox.parse.common/consume :lox.token/identifier (str "Expected a " (name type) " name."))
            (clojure.set/rename-keys {:previous :name})
            (lox.parse.common/consume :lox.token/l-paren (str "Expect ( after " (name type) " name."))
            get-params
            (lox.parse.common/consume :lox.token/r-paren (str "Expect ) after params."))
            (lox.parse.common/consume :lox.token/l-brace (str "Expect { " (name type) " body."))
            block
            lox.parse.common/advance)]
    (lox.parse.common/parse-identity (lox.parse.common/add-statement parse (lox.statement/->Function name params body)))))

