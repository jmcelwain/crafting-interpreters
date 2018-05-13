(ns lox.scan-test
  (:require [lox.scan :as sut]
            [clojure.test :as t]))

(t/deftest scan-single-token
  (t/testing "Scanning a single tokens")
  (t/is (= [:lox.token/l-brace :lox.token/eof] (sut/tokenize "{")))
  (t/is (= [:lox.token/r-brace :lox.token/eof] (sut/tokenize "}"))))

(t/deftest scan-two-single-token
  (t/testing "Scanning two individual tokens")
  (t/is (= [:lox.token/l-brace :lox.token/r-brace :lox.token/eof] (sut/tokenize "{}"))))

(t/deftest scan-two-char-token
  (t/testing "Scanning a two character token")
  (t/is (= [:lox.token/bang-equal :lox.token/eof] (sut/tokenize "!="))))

(t/deftest scan-two-char-token-no-match
  (t/testing "Scanning a two character token with failed match")
  (t/is (= [:lox.token/bang :lox.token/l-brace :lox.token/eof] (sut/tokenize "!{"))))

(t/deftest scan-comments
  (t/testing "Scanning comment")
  (t/is (= [:lox.token/bang :lox.token/eof] (sut/tokenize "//this is a comment\n!"))))
