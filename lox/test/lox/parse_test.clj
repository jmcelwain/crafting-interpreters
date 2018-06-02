(ns lox.parse-test
  (:require [clojure.test :as t]))

(t/deftest parse-class
  (t/testing "Parsing a class")
  (t/is (= {} (lox.parse/parse (lox.scan/tokenize "class Name {}")))))
