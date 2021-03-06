(ns lox.statement)

(defrecord Statement [name superclass methods])
(defrecord Function [name params body])
(defrecord Block [statements])
(defrecord Clazz [name superclass methods])
(defrecord If [condition then-branch else-branch])
(defrecord Print [expression])
(defrecord Var [name initalizer])
(defrecord While [condition body])
