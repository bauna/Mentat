(ns mentat.graph
  (:require [clojure.reflect :as r] 
            [mentat.core :as t]
            [clojure.string :only (join) :as s])
  (:import (java.lang.reflect Method Modifier Field)))

(defn method-label
  "build a string representation for a method"
  [^Method m]
  (str (.getSimpleName (.getDeclaringClass m)) "." 
       (.getName m) 
       "(" (s/join "," (map #(.getSimpleName %) (seq (.getParameterTypes m)))) ")"))

(defn pre-name
  "build a string for pre state"
  [pre-val]
  (let [m (first pre-val)
        v (second pre-val)]
    (str (if v "" "&not;") (method-label m))))

(defn state-name
  "generates a state name"
  [pres]
  (str "\"" (s/join "\\n" (map pre-name pres)) "\""))

(defn build-finite-state-machine
  "generates a finite state machine based on coll of pres"
  ([coll] 
    (if (empty? coll) #{}
      (let [pres (first coll)
            rcoll (rest coll) 
            [_ state] pres 
            st-name (state-name state)]
        (build-finite-state-machine #{(str "start -> " st-name "")} rcoll st-name))))
  ([so-far coll cur-state]
    (if (empty? coll) so-far
      (let [pres (first coll) 
            [method state] pres 
            rcoll (rest coll) 
            st-name (state-name state)
            m-label (method-label method)]
        (recur (conj so-far (str cur-state " -> " st-name " [ label = \"" m-label "\" ]")) rcoll st-name)))))
