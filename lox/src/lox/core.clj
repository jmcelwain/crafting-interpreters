(ns lox.core
  (:gen-class))

(defn usage []
  (println "Usage: clj-lox [script]"))

(defrecord Token [type lexeme literal line])

(defn scan
  [[c & cs] tokens]
  (if (nil? c)
    tokens
    (scan cs (conj tokens :lox.token/l-brace))))

(defn run [text]
  (let [tokens (scan text)]
    (doseq [token tokens]
      (println token))))


(defn file [file])

(defn prompt []
  (println "clj-lox:")
  (while true
    (print ">")
    (run (read-line))))

(defn -main
  "entrypoint to clj-lox"
  [& args]
  (cond
    (< 1 (count args)) (usage)
    (= 1 (count args)) (file (first args))
    :else (prompt)))

