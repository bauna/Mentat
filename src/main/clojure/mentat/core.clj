(ns mentat.core
  (:require [clojure.reflect :as r])
  (:import (java.lang.reflect Method Modifier Field) 
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
  [^Method m]
  (. Modifier isPublic (.getModifiers m)))

(def object-methods (set (.getMethods Object)))

(defn public-methods
  "returns public methods that are not present on java.lang.Object"
  [c]
  (if (class? c) 
    (filter #(and (not (contains? object-methods %)) (public? %)) (.getMethods c)) 
    (recur (class c))))

(defn get-fields
  "returns public methods that are not present on java.lang.Object"
  [c]
  (let [fs (map (fn [^Field f] (.setAccessible f true) f) (.getDeclaredFields c))]
    (if (empty? fs) nil fs)))

(defn get-all-fields 
  "returns a list of all fields class including super classes"
  [c]
  (if (= c Object) nil
    (concat (get-fields c) 
           (get-all-fields (.getSuperclass c)))))

(defn get-field-values
  [o fields]
  "creates a map where key are field names and key are the field values"
  ((reduce {} #(assoc %1 (keyword (.getName %2)) (.get %2 o)) fields)))

(defn pre->fn
  "convert a @Pre.value into a function"
  [pre]
  (eval (binding [*read-eval* false] (read-string (str "(fn [x] " pre ")")))))

(defn method-info
  [^Method m]
  "get the value of @Pre Annotation. 
   if it don't have the annotation returns 'false' so it never calls this method"
  (let [pre (.getAnnotation m Pre)] 
    (if (nil? pre)
      nil {:pre (pre->fn (.value pre)), :enabled (.enabled pre), :method m})))

(defn methods-pre-fn
  "generates a map that containd a function for each methos that contains @Pre annotation"
  [methods]
  (reduce {} #(assoc %1 %2 (pre->fn %2)) methods))

(defn get-class-info 
  "Geneartes ")

(defn str-invoke [instance method-str & args]
  (clojure.lang.Reflector/invokeInstanceMethod 
    instance 
    method-str 
    (to-array args)))
