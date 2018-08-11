(ns lox.parse-test
  (:require [clojure.test :as t]
            [lox.statement]))

(t/deftest parse-class
  (t/testing "Parsing a class")
  (let [clazz (first (lox.parse/parse (lox.scan/tokenize "class Name {}")))]
    (t/is (instance? lox.statement.Clazz clazz))
    (t/is (= (:lexeme (:name clazz)) "Name"))
    (t/is (nil? (:superclass clazz)))
    (t/is (= (count (:methods clazz)) 0))))
