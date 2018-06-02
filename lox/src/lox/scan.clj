(ns lox.scan
  (:require [clojure.core.match :refer [match]]))

(def keywords
  {"and"    :lox.token/and
   "class"  :lox.token/class
   "else"   :lox.token/else
   "false"  :lox.token/false
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

(defn advance-line [{:keys [line] :as scan}]
  (assoc scan :line (inc line)))

(defn is-finished? [{:keys [current text] :as scan}]
  (or (nil? scan) (>= current (count text))))

(defn get-text [{:keys [start current text]}]
  (subs text start current))

(defn get-string-literal [{:keys [start current text]}]
  (subs text (inc start) (dec current)))

(defn get-numeric-literal [{:keys [start current text]}]
  (read-string (subs text start current)))

(defn add-token
  ([scan type]
   (add-token scan type nil))
  ([{:keys [tokens line] :as scan} type literal]
   (assoc scan :tokens (conj tokens (lox.token/->Token type (get-text scan) literal line)))))

(defn match? [{:keys [text current char] :as scan} expected]
  (and (not (is-finished? scan)) (= expected (get text current))))

(defn digit? [c]
  (if (nil? c) false
      (= (java.lang.Character/getType ^Character c)
         (java.lang.Character/DECIMAL_DIGIT_NUMBER))))

(defn letter? [c]
  (if (nil? c) false
      (java.lang.Character/isLetter c)))

(defn match-digit? [{:keys [text current] :as scan}]
  (let [char (get text current)]
    (or (digit? char) (= char \.))))

(defn match-alphanumeric? [{:keys [text current] :as scan}]
  (let [char (get text current)]
    (or (letter? char) (digit? char))))

(defn add-comment [{:keys [char] :as scan}]
  (if (match? scan \/)
    (loop [{:keys [char] :as scan} (advance scan)]
      (if (or (is-finished? scan) (= char \newline))
        (advance-line scan)
        (recur (advance scan))))
    (add-token scan :lox.token/slash)))

(defn add-string [{:keys [char] :as scan}]
  (loop [{:keys [char] :as scan} (advance scan)]
    (cond
      (= char \") (add-token scan :lox.token/string (get-string-literal scan))
      (is-finished? scan) (throw (Exception. (str "Unterminated string")))
      :else (recur (advance scan)))))

(defn add-digit [{:keys [char] :as scan}]
  (if (match-digit? scan)
    (add-digit (advance scan))
    (add-token scan :lox.token/number (get-numeric-literal scan))))

(defn add-identifier [{:keys [char] :as scan}]
  (if (match-alphanumeric? scan)
    (add-identifier (advance scan))
    (if-let [type (get keywords (get-text scan))]
      (add-token scan type)
      (add-token scan :lox.token/identifier))))

(defn match-token [{:keys [tokens char] :as scan}]
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
         [\!] (if (match? scan \=)
                (add-token (advance scan) :lox.token/bang-equal)
                (add-token scan :lox.token/bang))
         [\=] (if (match? scan \=)
                (add-token (advance scan) :lox.token/equal-equal)
                (add-token scan :lox.token/equal))
         [\<] (if (match? scan \=)
                (add-token (advance scan) :lox.token/less-equal)
                (add-token scan :lox.token/less))
         [\>] (if (match? scan \=)
                (add-token (advance scan) :lox.token/greater-equal)
                (add-token scan :lox.token/greater))

         ;; slash
         [\/] (add-comment scan)

         ;; white space
         [\space] scan
         [\tab] scan
         [\return] scan
         [\newline] (advance-line scan)

         ;; string
         [\"] (add-string scan)

         ;; number
         [(_ :guard digit?)] (add-digit scan)

         ;; identifier
         [(_ :guard #(or (digit? %) (letter? %)))] (add-identifier scan)

         ;; something went wrong
         :else (throw (Exception. (str "Could not scan " scan)))))

(defn init [text]
  {:start 0 :current 0 :line 1 :text text :tokens [] :char nil})

(defn scan-tokens [{:keys [current tokens line] :as scan}]
  (if (is-finished? scan)
    (conj tokens (lox.token/->Token :lox.token/eof "" nil line))
    (let [scan (assoc scan :start current)]
      (scan-tokens (match-token (advance scan))))))

(defn tokenize [text]
  (let [scan (init text)]
    (scan-tokens scan)))

