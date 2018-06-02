(ns lox.statement)

(defrecord Statement [name superclass methods])

(defrecord Function [name params body])
