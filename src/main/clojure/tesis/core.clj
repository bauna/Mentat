(ns tesis.core
  (:require [clojure.reflect :as r])
  (:import (java.lang.reflect Method Modifier) 
           (ar.com.maba.tesis.preconditions Pre)))

(defn interface?
  "returns if the class is an interface."
  [c]
  (if (class? c) 
    (.isInterface c) 
    false))

(defn public-methods1
  "gets public methods of a java class."
  [c]
  (cond 
    (= c Object) nil
    (interface? c) nil
    :else 
    (let [{members :members} (r/reflect c :ancestors true)] 
      (filter #(and (contains? (:flags %) :public) (contains? % :return-type)) members))))  
  
(defn public? 
  "checks if methods is public"
  [m]
  (. Modifier isPublic (.getModifiers m)))

(def object-methods (set (.getMethods Object)))

(defn public-methods
  "returns public methods"
  [c]
  (if (class? c) 
    (filter #(and (not (contains? object-methods %)) (public? %)) (.getMethods c)) 
    (recur (class c))))
  
(defn pre-value
  [^Method m]
  "get the value of @Pre Annotation. if it don't have the annotation returns 'false' so it never calls this method"
  (let [pre (.getAnnotation m Pre)] 
    (if (nil? pre)
      "false" (.value pre))))

(defn pre->fn
  ""
  [^Method m]
  (eval (binding [*read-eval* false] (read-string (str "#(" (pre-value m) ")")))))

(defn methods-pre-fn
  "generates a map that containd a function for each methos that contains @Pre annotation"
  [methods]
  (reduce {} #(assoc %1 %2 (pre->fn %2)) methods))
