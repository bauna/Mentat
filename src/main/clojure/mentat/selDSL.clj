(ns mentat.selDSL
  (:require [mentat.core :as c]
            [mentat.trace :as t]
            [clojure.edn :only (read) :as e]
            [clojure.java.io :as io])
  (:import (java.net URL)))

(defn- while-tag-parser [keys while-body]
  (assert (<= 2 (count while-body)))
  (assert (string? (first while-body)))
  [::while (first while-body) (c/gen-fn-key keys (second while-body))])

(defn- random-tag-parser [& _]
  [::random])

(defn tag-parsers [keys] 
  {'mentat/while (partial while-tag-parser keys)
   'mentat/random random-tag-parser})

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
    (if (zero? n)
      [nil script]
      (let [[type & params] (first script)]
        ;(println "type: " type " params: " params)
        (cond
          (= ::random type) [nil (rotate script)]
          (= ::while type)
            (if ((second params) field-values)
              [(first params) script]
              (recur (dec n) (rotate script) field-values)))))))

(defn generate-selection-function
  [^Class clazz ^URL config-file]
  (let [fields (c/get-all-fields clazz)
        keys (map #(-> % .getName keyword) fields)
        script (atom (read-config-file keys config-file))]
    (fn [instance method-infos]
      (let [[method-name new-script] (choose-method instance fields @script)]
        (do
          ;(println "method-name: " method-name " new-script:" new-script)
          (reset! script new-script)
          (if-let [sel (and method-name (seq (filter #(= method-name (-> % first :name)) method-infos)))]
            (first sel)
            (t/random-sel instance method-infos)))))))
