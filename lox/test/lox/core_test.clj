(ns lox.core-test
  (:require [clojure.test :refer :all]
            [lox.core :refer :all]))

(deftest scan-single-char
  (testing "Scanning a single character"
    (is (= [:lox.token/l-brace] (scan "{" [])))))
