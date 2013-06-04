(ns mentat.selDSL
  (:require [clojure.edn :only (read) :as e]))

(defn gen-fn
  [body]
  (let [as [(symbol "vs")]]
    (eval `(fn ~as ~body))))

(defn while-tag-parser [a]
  (println a)
  [(first a) (gen-fn (second a)) ])

(def tag-parsers {(symbol "mentat/while") while-tag-parser})

(defn read-config-file [filename]
  (with-open [r (java.io.PushbackReader.
                 (clojure.java.io/reader filename))]
    (e/read {:readers tag-parsers} r)))
