(ns lox.scan-test
  (:require [lox.scan :as sut]
            [clojure.test :as t]))

(t/deftest scan-single-token
  (t/testing "Scanning a single tokens")
  (t/is (= [:lox.token/l-brace :lox.token/eof] (sut/scan "{")))
  (t/is (= [:lox.token/r-brace :lox.token/eof] (sut/scan "}"))))

(t/deftest scan-two-single-token
  (t/testing "Scanning two individual tokens")
  (t/is (= [:lox.token/l-brace :lox.token/r-brace :lox.token/eof] (sut/scan "{}"))))
