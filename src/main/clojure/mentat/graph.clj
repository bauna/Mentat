(ns mentat.graph
  (:require [clojure.reflect :as r] 
            [mentat.core :as t]
            [flatland.ordered.map :only (ordered-map) :as om]
            [clojure.string :only (join) :as s]
            [dorothy.core :as d])
  (:import (java.lang.reflect Method Modifier Field)))

(defn ^String method-label
  "build a string representation for a method"
  [^Method m]
  (str (.getName m) 
       "(" (s/join ", " (map #(.getSimpleName %) (seq (.getParameterTypes m)))) ")"))

(defn ^String state-name
  "generates a state name"
  [pres]
  (if (= :failed pres) :trap
    (s/join "\\n" (map #(method-label (first %)) (filter second pres)))))

(defn build-finite-state-machine
  "generates a finite state machine based on coll of pres"
  [coll]
  (let [mem-state-name (memoize state-name)
        func (fn f 
               ([coll] 
                 (if-let [s (seq coll)] 
                   (let [pres (first s)
                         rcoll (rest s) 
                         [_ state] pres 
                         st-name (mem-state-name state)]
                     (if (= :failed state) 
                       [(om/ordered-map [:start :trap]  #{}) true]
                       (f (om/ordered-map [:start  st-name] #{}) rcoll st-name)))))
               ([so-far coll cur-state]
                 (if-let [s (seq coll)]
                   (let [pres (first coll) 
                         [method state] pres
                         m-label (method-label method)]
                     (if (= :failed state)
                       [(assoc so-far [cur-state :trap] #{m-label}) true]  
                       (let [rcoll (rest coll) 
                             st-name (mem-state-name state)
                             transition [cur-state st-name]]
                         (recur (assoc so-far transition
                                       (if-let [labels (so-far transition)] 
                                         (conj labels m-label) #{m-label})) 
                                      rcoll st-name))))
                   [so-far false])))]
    (func coll)))

(defn- mk-label
  [labels]
  (s/join "\\n" labels))

(defn build-dot-file
  [coll]
  (let [[transitions has-trap] (build-finite-state-machine coll)]
    (d/digraph (into [{:rankdir "LR"} [:start {:shape "doublecircle"}]] 
                     (map #(conj (first %) (if (empty? (second %)) {} {:label (mk-label (second %))})) transitions)))))
