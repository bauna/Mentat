(ns mentat.selDSL
  (:require [clojure.edn :only (read) :as e]))

(defn read-config-file [filename]
  (with-open [r (java.io.PushbackReader.
                 (clojure.java.io/reader filename))]
    (e/read r)))

(defn gen-fn
  [body]
  (let [as [(symbol "vs")]]
    (eval `(fn ~as ~body))))
