(ns lox.parse-test
  (:require [clojure.test :as t]
            [lox.scan]
            [lox.statement]))

;; util
(defn- get-statement [str]
  (first (lox.parse/parse (lox.scan/tokenize str))))


;; main api
(t/deftest parse-class
  (t/testing "Parsing a class")
  (let [clazz (get-statement  "class Name {}")]
    (t/is (true? (instance? lox.statement.Clazz clazz)))
    (t/is (= (:lexeme (:name clazz)) "Name"))
    (t/is (nil? (:superclass clazz)))
    (t/is (= (count (:methods clazz)) 0))))

(t/deftest parse-function
  (t/testing "Parsing a function")
  (let [fn (get-statement "fun simple() {}")]
    (t/is (true? (instance? lox.statement.Function fn)))
    (t/is (= (:lexeme (:name fn)) "simple"))
    (t/is (= (count (:params fn)) 0))
    (t/is (= (count (:body fn)) 0))))

(t/deftest parse-var
  (t/testing "Parsing a var")
  (let [var (get-statement "var x = 1")]
    (t/is (= " var"))))

;; internal helpers

(defn- init-get-params [str]
  (let [parse (lox.parse/init (lox.scan/tokenize str))]
    (assoc parse :current 3))) ;; fun name(_
                              ;;  0    1 23

(t/deftest get-params
  (t/testing "Getting parameters from a function or method")

  (let [no-params (init-get-params "fun noParams() {}")
        {:keys [params current]} (lox.parse/get-params no-params)]
    (t/is (= 0 (count params)))
    (t/is (= 4 current)))

  (let [single-param (init-get-params "fun singleParam(a) {}")
        {:keys [params current]} (lox.parse/get-params single-param)]
    (t/is (= 1 (count params)))
    (t/is (= 5 current))))
