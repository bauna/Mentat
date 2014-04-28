(ns mentat.graph
  (:require [clojure.reflect :as r]
            [clojure.set :as set]
            [mentat.core :as t]
            [flatland.ordered.map :only (ordered-map) :as om]
            [clojure.string :only (join) :as s]
            [dorothy.core :as d])
  (:import (java.lang.reflect Method Modifier Field)))

(defn ^String method-label
  "build a string representation for a method"
  [^Method m]
  (if m (str (.getName m) 
            "(" (s/join ", " (map #(.getSimpleName %) (seq (.getParameterTypes m)))) ")")))

(defn- failed? 
  [pre] 
  (= :failed (second pre)))

(defn ^String state-name
  "generates a state name"
  [pre]
  (if (failed? pre) :trap
    (s/join "\\n" (map #(method-label (first %)) (filter second pre)))))

(defn build-finite-state-machine
  ([max-steps traces]
    (let [mem-state-name (memoize state-name)
          mem-method-label (memoize method-label)
          gen-step (fn [[method state :as pres]] [(mem-state-name state) (mem-method-label method)])
          union #(let [s1 (if %1 %1 #{})] (if %2 (set/union s1 #{%2}) s1))
          gen-state-machine (fn [trace] 
                              (first (reduce 
                                       (fn [[m curr-state :as _] pre] 
                                         (let [[state-name label] (gen-step pre)
                                               transition [curr-state state-name]]
                                           [(update-in m [transition] union label) state-name])) 
                                       [(om/ordered-map) :start] trace)))] 
      (reduce (fn [full-machine m] 
                (reduce (fn [full-machine [k v :as _]] 
                          (update-in full-machine [k] set/union v)) full-machine m)) 
              (map #(gen-state-machine (take max-steps %)) traces)))))

(def ^:dynamic *max-steps* 100) 

(defn build-dot-file
  ([traces] (build-dot-file *max-steps* traces))
  ([max-steps traces] (let [transitions (build-finite-state-machine max-steps traces)]
     (d/digraph (into [{:rankdir "LR"} [:start {:shape "doublecircle"}]] 
                      (map (fn [[transition labels :as _]] 
                             (conj transition (if (empty? labels) {} {:label (s/join "\\n" labels)}))) 
                           transitions))))))
