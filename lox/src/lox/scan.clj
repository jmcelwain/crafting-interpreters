(ns lox.scan
  (:require [clojure.core.match :refer [match]]))

(def keywords
  { "and"    :lox.core/an
    "class"  :lox.core/class
    "else"   :lox.core/else
    "false"  :lox.core/false
    "fun"    :lox.token/fun
    "for"    :lox.token/for
    "if"     :lox.token/if
    "nil"    :lox.token/nil
    "or"     :lox.token/or
    "print"  :lox.token/print
    "return" :lox.token/return
    "super"  :lox.token/super
    "this"   :lox.token/this
    "true"   :lox.token/true
    "var"    :lox.token/var
    "while"  :lox.token/while })


(defn match-token [c]
  (match [c]
         [\(] :lox.token/l-paren
         [\)] :lox.token/r-paren
         [\{] :lox.token/l-brace
         [\}] :lox.token/r-brace
         :else nil
         ))


(defn scan
  ([text]
   (scan text []))

  ([text tokens]
   (if (string? text)
     (scan (first text) (rest text) tokens)
     text
     ))

  ([c cs tokens]
   (let [token (match-token c)]
     (scan cs (conj tokens token)))))
