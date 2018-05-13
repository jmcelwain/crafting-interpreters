(ns lox.scan
  (:require [clojure.core.match :refer [match]]))

(def keywords
  {"and"    :lox.core/an
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
   "while"  :lox.token/while})

(defn advance [{:keys [current text] :as scan}]
  (assoc scan :current (inc current) :char (get text current)))

(defn add-token [{:keys [tokens] :as scan} token]
  (assoc scan :tokens (conj tokens token)))

(defn match-token [{:keys [tokens] :as scan}]
  (let [scan  (advance scan)
        char  (:char scan)]
    (match [char]
           ;; one-char
           [\(] (add-token scan :lox.token/l-paren)
           [\)] (add-token scan :lox.token/r-paren)
           [\{] (add-token scan :lox.token/l-brace)
           [\}] (add-token scan :lox.token/r-brace)
           [\,] (add-token scan :lox.token/comma)
           [\.] (add-token scan :lox.token/dot)
           [\-] (add-token scan :lox.token/minus)
           [\+] (add-token scan :lox.token/plus)
           [\;] (add-token scan :lox.token/semicolon)
           [\*] (add-token scan :lox.token/star)
           ;; two-char
           :else (throw (Exception. (str "Could not scan " scan))))))

(defn init [text]
  {:start 0 :current 0 :line 1 :text text :tokens [] :char nil})

(defn is-finished? [{:keys [current text]}]
  (>= current (count text)))

(defn scan-tokens [{:keys [current tokens] :as scan}]
  (if (is-finished? scan)
    (conj tokens :lox.token/eof)
    (let [scan (assoc scan :start current)]
      (scan-tokens (match-token scan)))))

(defn tokenize [text]
  (let [scan (init text)]
    (scan-tokens scan)))

 
