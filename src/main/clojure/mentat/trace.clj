(ns mentat.core
  (:require [clojure.reflect :as r
             mentat.core :as c
             mentat.z3 :as z3
             mentat.javaZ3 :as jz3])
  (:import (java.lang.reflect Method Modifier Field) 
           (ar.com.maba.tesis.preconditions Pre ClassDefinition)))

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

(defn- create-sorted-map
  []
  (sorted-map-by #(compare (.toString %1) (.toString %2))))

(def ^:dynamic *execute-timeout* 3000)

(defn invoke-method
  "invoke a object instance method if it throws an exception returns false or allows to specify a error value"
  ([o ^Method method params] (invoke-method o method params false))
  ([o ^Method method params error-result]
    (let [fut (future (try 
        (.invoke method o (if (nil? params) nil (to-array [params])))
        true
        (catch Throwable t error-result)))]
      (deref fut *execute-timeout* error-result))))

(defn check-invariant 
  "checks whethet the invariant of the class is valid or not"
  [instance inv-fn fields]
  (apply inv-fn [(c/get-field-values instance fields)]))

(defn enabled? 
  "checks is a method is enabled"
  [method-info solver instance z3-inst-consts inst-state z3-ctx]
  (let [ret (atom nil)
        bool-expr (z3/z3 (:pre-expr method-info) z3-inst-consts inst-state z3-ctx)]
    (.push solver)
    (.add solver (into-array BoolExpr [bool-expr])
    (reset! ret (z3/sat? solver))
    (.pop solver 1)
    @ret)))

(defn get-enabled-methods
  "returns a collection of the methods that are enabled to be called in the currect state of instance"
  [instance method-infos inst-state z3-inst-consts z3-ctx]
  (let [solver (mk-solver z3-ctx)]
    (map #(vector % (enabled? % solver instance z3-inst-consts inst-state z3-ctx)) method-infos)))

(defn trace-fn2 
  "generate a new trace step on each invocation"
  [^Class clazz sel-fn]
  (let [cl-info (c/class-info clazz)
        o (apply (:builder cl-info) nil)
        fields (c/get-all-fields clazz)
        inv-fn (:invariant cl-info)
        mis (c/all-method-infos (c/public-methods o))
        lastm (atom nil)]
    (fn [] 
      (if-not (check-invariant o inv-fn fields) 
        [@lastm :failed]
        (let [ctx (z3/create-context)
              inst-state (c/get-field-values instance fields) 
              z3-inst-consts (jz3/mk-instance z3-ctx instance fields inst-state)]
          (if-let [pres (seq (get-enabled-methods o mis inst-state z3-inst-consts ctx))]
            (if-let [sel (seq (sel-fn pres))]
              (let [mi (first sel)
                    oldm @lastm
                    newm (:method mi)
                    data-val (generate-params o mi inst-state z3-inst-consts ctx)]
                (reset! lastm newm)
                ))))))))

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

