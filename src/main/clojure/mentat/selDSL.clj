(ns mentat.selDSL
  (:require [mentat.core :as c]
            [mentat.trace :as t]
            [clojure.edn :only (read) :as e]
            [clojure.java.io :as io])
  (:import (java.net URL)))

(defn while-tag-parser [keys while-body]
  (assert (<= 2 (count while-body)))
  (assert (string? (first while-body)))
  [(first while-body) (c/gen-fn-key keys (second while-body))])

(defn tag-parsers [keys] 
  {'mentat/while (partial while-tag-parser keys)})

(defn read-config-file [keys ^URL config-file]
  (println keys)
  (with-open [r (java.io.PushbackReader.
                 (io/reader config-file))]
    (e/read {:readers (tag-parsers keys)} r)))

(defn rotate 
  [s]
  (concat (rest s) [(first s)]))

(defn choose-method 
  [instance fields script]
  (loop [n (count script)
         script script
         field-values (c/get-field-values instance fields)]
    (println "n: " n " script: " script)
    (println "field-values: " field-values)
    (if-not (zero? n) 
      (let [step (first script)]
        (if ((second step) field-values)
          [(first step) script]
          (recur (dec n) (rotate script) field-values))))))

(defn generate-selection-function
  [^Class clazz ^URL config-file]
  (let [fields (c/get-all-fields clazz)
        keys (map #(-> % .getName keyword) fields)
        script (atom (read-config-file keys config-file))]
    (fn [instance pres]
      (if-let [ret (choose-method instance fields @script)]
        (do 
          (reset! script (second ret))
          (let [method-name (first ret)
                sel (filter #(= method-name (-> % first :name)) pres)]
            (if (empty? sel) (t/random-sel instance pres) (first sel))))))))
