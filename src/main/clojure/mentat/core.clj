(ns mentat.core
  (:require [clojure.reflect :as r])
  (:import (java.lang.reflect Method Modifier Field) 
           (ar.com.maba.tesis.preconditions Pre)))

(defn interface?
  "returns if the class is an interface."
  [c] (and (class? c) (.isInterface c)))

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
  (reduce #(assoc %1 (keyword (.getName %2)) (.get %2 o)) {} fields))

(defn gen-fn
  "convert a @Pre.value into a function"
  [fn-body]
  (eval (binding [*read-eval* false] (read-string (str "(fn [vs] " fn-body ")")))))

(defn- nil-fn [& args] nil)

(defn method-info
  [^Method m]
  "get the value of @Pre Annotation. 
   if it don't have the annotation returns 'false' so it never calls this method"
  (let [pre (.getAnnotation m Pre) 
        pre-val (.value pre) 
        data-val (.data pre)] 
    (if (or (nil? pre) (not (.enabled pre)))
      nil 
      {:pre (gen-fn pre-val), 
       :data (if (empty? data-val) nil-fn (gen-fn data-val)), 
       :method m})))

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

(defn eval-pre
  "evaluates all preconditions"
  [value-map method-infos]
  (map #(vector % (apply (:pre %) [value-map])) method-infos))

;------------------
(defn random-sel
  "throw a coin an selects a method"
  [pres]
  (let [xs (filter #(nth % 1) pres)] 
    (nth xs (rand-int (count xs)))))

(defn invoke-method
  "invokes a method using an strategy function"
  [o sel-fn mis]
  (let [m (sel-fn mis)]
    (.invoke m o )))


(defn trace-fn 
  "generates a fn that returns evals pres and invoke a method"
  [o sel-fn]
  (let [fs (get-all-fields (class o)) 
        mis (all-method-infos (public-methods o))
        lastm (atom nil)]
    (fn [] 
      (let [pres (eval-pre (get-field-values o fs) mis)
            mi (first (sel-fn pres))
            oldm @lastm 
            newm (:method mi) 
            data-val (apply (:data mi) [o])]
        (reset! lastm newm)
        (.invoke newm o (if (nil? data-val) nil (to-array [data-val])))
        [oldm (map #(vector (:method (first %)) (second %)) pres)]))))
  
(defn trace-gen
  "generate a trace of invocations "
  ([o sel-fn] (trace-gen (trace-fn o sel-fn)))
  ([gen-fn] (lazy-seq (cons (gen-fn) (trace-gen gen-fn)))))
