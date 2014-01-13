(ns mentat.trace
  (:require [clojure.reflect :as r]
            [mentat.core :as c]
            [mentat.z3 :as z3]
            [mentat.javaZ3 :as jz3])
  (:import (java.lang.reflect Method Modifier Field TypeVariable) 
           (ar.com.maba.tesis.preconditions Pre ClassDefinition)
           (com.microsoft.z3 Context Status Solver BoolExpr ArithExpr)))

(defn random-sel
  "throw a coin an selects a method"
  [instance pres]
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
    (println "will exec: " (.toString method))
    (let [fut (future (try 
        (.invoke method o (to-array params))
        true
        (catch Throwable t error-result)))]
      (deref fut *execute-timeout* error-result))))

(defn check-invariant 
  "checks whethet the invariant of the class is valid or not"
  [instance inv-fn fields]
  (println "check-invariant: " (c/get-field-values instance fields))
  (let [r (inv-fn (c/get-field-values instance fields))]
    (println "check.inv: " r)
    r))

(defn enabled? 
  "checks is a method is enabled"
  [method-info instance inst-state fields]
  (let [ctx (z3/create-context)
        z3-inst-data (jz3/mk-instance ctx instance fields inst-state)
        z3-inst-consts (reduce #(assoc %1 (first %2) (-> %2 second :const)) {} z3-inst-data)
        z3-params-consts (jz3/mk-constants-for-params ctx (:method method-info))
        solver (.mkSolver ctx)
        bool-expr (z3/z3 (:pre method-info) (merge z3-inst-consts z3-params-consts) inst-state ctx)]
    (.add solver (into-array BoolExpr (flatten [bool-expr (map #(-> % second :exprs) z3-inst-data)])))
    (z3/sat? solver)))

(defn get-enabled-methods
  "returns a collection of the methods that are enabled to be called in the currect state of instance"
  [instance method-infos inst-state fields]
    (map #(vector % (enabled? % instance inst-state fields)) method-infos))

(defn generate-params 
  "generate vector params for invoking a method"
  [instance method-info inst-state fields]
  (let [ctx (z3/create-context)
        method (:method method-info)
        z3-params-consts (jz3/mk-constants-for-params ctx method)]
    (if (empty? z3-params-consts) nil
      (let [z3-inst-data (jz3/mk-instance ctx instance fields inst-state)
            z3-inst-consts (reduce #(assoc %1 (first %2) (-> %2 second :const)) {} z3-inst-data)
            solver (.mkSolver ctx)
            bool-expr (z3/z3 (:pre method-info) (merge z3-inst-consts z3-params-consts) inst-state ctx)]
        (.add solver (into-array BoolExpr (flatten [bool-expr (map #(-> % second :exprs) z3-inst-data)])))
        (loop [params-map (merge
                            (z3/model-to-map (z3/get-model solver) z3-params-consts)
                            (apply (c/gen-fn-key (keys inst-state) (:data method-info)) [inst-state]))
               index 0
               count (-> method .getParameterTypes count)
               params []]
          (if (< index count) 
            (recur params-map (inc index) count (conj params ((keyword (str "p" index)) params-map))) 
            params))))))

(defn trace-fn
  "generate a new trace step on each invocation"
  [^Class clazz sel-fn]
  (let [cl-info (c/class-info clazz)
        o (apply (:builder cl-info) nil)
        fields (c/get-all-fields clazz)
        inv-fn (c/gen-fn-key (map #(-> % .getName keyword) fields) (:invariant cl-info))
        mis (c/all-method-infos (c/public-methods o))
        lastm (atom nil)]
    (fn [] 
      (if-not (check-invariant o inv-fn fields) 
        [false [@lastm :failed]]
        (let [inst-state (c/get-field-values o fields)]
          (if-let [pres (seq (get-enabled-methods o mis inst-state fields))]
            (if-let [sel (seq (sel-fn o pres))]
              (let [mi (first sel)
                    oldm @lastm
                    methods-status (reduce #(assoc %1 (:method (first %2)) (second %2)) (create-sorted-map) pres) 
                    newm (:method mi)
                    data-val (generate-params o mi inst-state fields)]
                (reset! lastm newm)
                (if (and (invoke-method o newm data-val) (check-invariant o inv-fn fields))
                      [true [oldm methods-status]]
                      [false (list [oldm methods-status] [newm :failed])])))))))))

(defn trace-gen
  "generate a trace of invocations "
  ([^Class clazz sel-fn] (trace-gen (trace-fn clazz sel-fn)))
  ([gen-fn] 
    (let [[continue step] (gen-fn)] 
      (if continue 
        (lazy-seq (cons step (trace-gen gen-fn)))
        step))))
