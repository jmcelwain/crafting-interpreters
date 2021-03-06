(ns lox.scan-test
  (:require [clojure.test :as t]))

(t/deftest scan-single-token
  (t/testing "Scanning a single tokens")
  (t/is (= [(lox.token/->Token :lox.token/l-brace "{" nil 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "{")))
  (t/is (= [(lox.token/->Token :lox.token/r-brace "}" nil 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "}"))))

(t/deftest scan-two-single-token
  (t/testing "Scanning two individual tokens")
  (t/is (= [(lox.token/->Token :lox.token/l-brace "{" nil 1) (lox.token/->Token :lox.token/r-brace "}" nil 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "{}"))))

(t/deftest scan-two-char-token
  (t/testing "Scanning a two character token")
  (t/is (= [(lox.token/->Token :lox.token/bang-equal "!=" nil 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "!="))))

(t/deftest scan-two-char-token-no-match
  (t/testing "Scanning a two character token with failed match")
  (t/is (= [(lox.token/->Token :lox.token/bang "!" nil 1) (lox.token/->Token :lox.token/l-brace "{" nil 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "!{"))))

(t/deftest scan-comments
  (t/testing "Scanning comment")
  (t/is (= [(lox.token/->Token :lox.token/bang "!" nil 1) (lox.token/->Token :lox.token/eof "" nil 2)] (lox.scan/tokenize "!// ok wow\n")))
  (t/is (= [(lox.token/->Token :lox.token/bang "!" nil 2) (lox.token/->Token :lox.token/eof "" nil 2)] (lox.scan/tokenize "//this is a comment\n!"))))

(t/deftest scan-newline
  (t/testing "Scanning newline")
  (t/is (= [(lox.token/->Token :lox.token/eof "" nil 2)] (lox.scan/tokenize "\n"))))

(t/deftest scan-white-space
  (t/testing "Scanning white space")
  (t/is (= [(lox.token/->Token :lox.token/eof "" nil 2)] (lox.scan/tokenize "   \n")))
  (t/is (= [(lox.token/->Token :lox.token/eof "" nil 2)] (lox.scan/tokenize (str \tab "\n"))))
  (t/is (= [(lox.token/->Token :lox.token/eof "" nil 2)] (lox.scan/tokenize (str \tab \return \tab "\n")))))

(t/deftest scan-string
  (t/testing "Scanning a string")
  (t/is (= [(lox.token/->Token :lox.token/string "\"str\"" "str" 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "\"str\"")))
  (t/is (thrown? Exception (lox.scan/tokenize "\"str\n"))))

(t/deftest scan-number
  (t/testing "Scanning a number")
  (t/is (= [(lox.token/->Token :lox.token/number "1" 1 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "1")))
  (t/is (= [(lox.token/->Token :lox.token/number "11" 11 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "11")))
  (t/is (= [(lox.token/->Token :lox.token/number "11.01" 11.01 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "11.01"))))

(t/deftest scan-identifier
  (t/testing "Scanning an identifier")
  (t/is (= [(lox.token/->Token :lox.token/class "class" nil 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "class")))
  (t/is (= [(lox.token/->Token :lox.token/this "this" nil 1) (lox.token/->Token :lox.token/eof "" nil 1)] (lox.scan/tokenize "this"))))

