(ns lox.parse-test
  (:require [clojure.test :as t]
            [lox.scan]
            [lox.statement]))

(defn- get-statement [str]
  (first (lox.parse/parse (lox.scan/tokenize str))))

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


