(ns lox.scan-test
  (:require [lox.scan :as sut]
            [clojure.test :as t]))

(t/deftest scan-single-char
  (t/testing "Scanning a single character")
  (t/is (= [:lox.token/l-brace] (sut/scan "{")))
  )
