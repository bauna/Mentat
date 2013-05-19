(ns mentat.core
  (:require [clojure.reflect :as r])
  (:import (java.lang.reflect Method Modifier Field) 
           (ar.com.maba.tesis.preconditions Pre ClassDefinition)))

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

(defn gen-builder-fn
  "convert a @Pre.value into a function"
  [fn-body]
  (eval (binding [*read-eval* false] (read-string (str "(fn [& args] " fn-body ")")))))

(defn- nil-fn [& args] nil)

(defn class-info
  "builds class info from a class annotated with @ClassDefinition"
  [^Class clazz]
  (let [def (.getAnnotation clazz ClassDefinition)]
    {:invariant (gen-fn (.invariant def)) 
     :builder (gen-builder-fn (.builder def))}))

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
       :data (if-let [s (seq data-val)] 
               (gen-fn data-val) 
               nil-fn), 
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
  (if-let [xs (seq (filter second pres))]
    (nth xs (rand-int (count xs))))) 
  
(defn invoke-method
  "invokes a method using an strategy function"
  [o sel-fn mis]
  (let [m (sel-fn mis)]
    (.invoke m o )))

(defn create-sorted-map
  []
  (sorted-map-by #(compare (.toString %1) (.toString %2))))

(defn invoke-method
  "invoke a object instance method if it throws an exception returns false or allows to specify a error value"
  ([o ^Method method params] (invoke-method o method params false))
  ([o ^Method method params error-result] 
    (try 
      (.invoke method o (if (nil? params) nil (to-array [params])))
      true
      (catch Throwable t error-result))))

(defn trace-fn 
  "generates a fn that returns evals pres and invoke a method"
  [^Class clazz sel-fn]
  (let [cl-info (class-info clazz)
        o (apply (:builder cl-info) nil)
        fields (get-all-fields clazz)
        inv-fn (:invariant cl-info)
        mis (all-method-infos (public-methods o))
        lastm (atom nil)]
    (fn [] 
      (let [pres (eval-pre (get-field-values o fields) mis)]
        (if-let [sel (seq (sel-fn pres))]
          (let [mi (first sel)
                oldm @lastm
                newm (:method mi)
                data-val (apply (:data mi) [o])]
                  (reset! lastm newm)
                  (if (and (invoke-method o newm data-val) 
                           (apply inv-fn [(get-field-values o fields)]))
                      [oldm (reduce #(assoc %1 (:method (first %2)) (second %2)) 
                                    (create-sorted-map) pres)]
                      
                      [oldm :failed])))))))

(defn trace-gen
  "generate a trace of invocations "
  ([^Class clazz sel-fn] (trace-gen (trace-fn clazz sel-fn)))
  ([gen-fn] 
    (let [exec (gen-fn)] 
      (if-not (= :failed (second exec)) 
        (lazy-seq (cons exec (trace-gen gen-fn)))
        (cons exec nil)))))

