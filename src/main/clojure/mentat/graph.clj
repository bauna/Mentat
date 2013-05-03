(ns mentat.graph
  (:require [clojure.reflect :as r] 
            [mentat.core :as t]
            [clojure.string :only (join) :as s])
  (:import (java.lang.reflect Method Modifier Field)))

(defn ^String method-label
  "build a string representation for a method"
  [^Method m]
  (str (.getName m) 
       "(" (s/join ", " (map #(.getSimpleName %) (seq (.getParameterTypes m)))) ")"))

(defn ^String state-name
  "generates a state name"
  [pres]
  (str "\"" (s/join "\\n" (map #(method-label (first %)) (filter second pres))) "\""))


(defn build-finite-state-machine
  "generates a finite state machine based on coll of pres"
  [coll]
  (let [mem-state-name (memoize state-name)
        func (fn f 
               ([coll] 
                 (if (empty? coll) {}
                   (let [pres (first coll)
                         rcoll (rest coll) 
                         [_ state] pres 
                         st-name (mem-state-name state)]
                     (f {(str "start -> " st-name) #{}} rcoll st-name))))
               ([so-far coll cur-state]
                 (if (empty? coll) so-far
                   (let [pres (first coll) 
                         [method state] pres 
                         rcoll (rest coll) 
                         st-name (mem-state-name state)
                         m-label (method-label method)
                         transition (str cur-state " -> " st-name)]
                     (recur (assoc so-far transition (if-let [labels (so-far transition)] 
                                            (conj labels m-label) 
                                            #{m-label})) 
                                  rcoll st-name)))))]
    (func coll)))

(defn ^String build-dot-file
  "generates the dot file string"
  [coll]
  (let [transitions (build-finite-state-machine coll)]
    (str "digraph finite_state_machine {\n\trankdir=LR;\n\tnode [shape = doublecircle]; start;\n\tnode [shape = circle];\n\t"
         (s/join "\n\t" 
                 (map #(str (first %) " [label = \"" (s/join "\\n" (second %)) "\"];") transitions)) 
         "\n}")))
 