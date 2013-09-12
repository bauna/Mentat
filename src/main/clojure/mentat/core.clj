(ns mentat.core
  (:require [clojure.reflect :as r])
  (:import (java.lang.reflect Method Modifier Field) 
           (ar.com.maba.tesis.preconditions Pre ClassDefinition)))

(defn mk-symb [xs]
  (map #(-> % name symbol) xs))

(defn gen-fn-key
  [keys body]
  (let [ksymb (mk-symb keys)
        vs (symbol "vs")]
    (eval `(fn [m#] 
             (let [{:keys ~ksymb :as ~vs} m#] ~@body)))))

(defn interface?
  "returns if the class is an interface."
  [c] (and (class? c) (.isInterface c)))
  
(defn public? 
  "checks if methods is public"
  [^Method m]
  (. Modifier isPublic (.getModifiers m)))

(def object-methods (set (.getMethods Object)))

(defn public-methods
  "returns public methods that are not present on java.lang.Object"
  [c]
  (if (class? c) 
    (filter #(and (not (contains? object-methods %)) (public? %) (not (.isBridge %))) (.getMethods c)) 
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
  (reduce #(assoc %1 (keyword (.getName %2)) (.get %2 o)) {} fields))

(defn gen-fn
  "convert a @Pre.value into a function"
  [fn-body]
  (eval (binding [*read-eval* false] (read-string (str "(fn [vs] " fn-body ")")))))

(defn gen-builder-fn
  "convert a @Pre.value into a function"
  [fn-body]
  (eval (binding [*read-eval* false] (read-string (str "(fn [& args] " fn-body ")")))))

(defn class-info
  "builds class info from a class annotated with @ClassDefinition"
  [^Class clazz]
  (let [def (.getAnnotation clazz ClassDefinition)
        invariant (.invariant def)]
    {:invariant (if (empty? invariant)
                  (fn [_] true)
                  (gen-fn-key
                    (map #(-> % .getName keyword) (get-all-fields clazz))
                    (binding [*read-eval* false] (read-string invariant))))
     :builder (gen-builder-fn (.builder def))}))

(defn method-info
  [^Method m]
  "get the value of @Pre Annotation. 
   if it don't have the annotation returns 'false' so it never calls this method"
  (let [pre (.getAnnotation m Pre) 
        pre-val (.value pre) 
        data-val (.data pre)
        name (.name pre)] 
    (if (or (nil? pre) (not (.enabled pre)))
      nil 
      {
       :pre (read-string pre-val)
       :data (if-not (empty? data-val) 
               (binding [*read-eval* false] (read-string data-val)) 
               nil)
       :method m
       :name (if (empty? name) (.getName m) name)})))

(defn methods-pre-fn
  "generates a map that containd a function for each method that contains @Pre annotation"
  [methods]
  (reduce {} #(assoc %1 %2 (gen-fn %2)) methods))

(defn str-invoke [instance method-str & args]
  (clojure.lang.Reflector/invokeInstanceMethod 
    instance 
    method-str 
    (to-array args)))

(defn all-method-infos
  "generates all method-infos"
  [methods]
  (vec (filter #(not (nil? %)) (map method-info methods))))
