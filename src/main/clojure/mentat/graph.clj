(ns mentat.graph
  (:require [clojure.reflect :as r] 
            [mentat.core :as t]
            [clojure.string :only (join)])
  (:import (java.lang.reflect Method Modifier Field)))
  
  (defn short-method-name
    "shorts the name of a method"
    [pre]
    (let [m (first pre)
          v (second pre)]
      (str (if v "" "&not;") 
         (.getSimpleName (.getDeclaringClass m)) "." 
         (.getName m) "("
         (join "," (map #(.getSimpleName %) (.getParameterTypes m)))
         ")")))
    

  (defn state-name
    "generates a state name"
    [pres]
    (join "\\n" (map short-method-name pres)))
  

