(ns mentat.z3
  (:import (java.lang.reflect Method Modifier Field) 
           (com.microsoft.z3 Context Status Solver BoolExpr)
           (ar.com.maba.tesis.preconditions Pre ClassDefinition)))

(def ^:dynamic *z3-config* 
  {"model" "true"})

(defn create-context 
  ([] (create-context *z3-config*))
  ([config] (Context. config)))

(defn mk-int-const
  [^Context ctx symbol]
  (.mkIntConst ctx (str symbol)))

(defn check-sat 
  "check sat on a Context"
  [^Context ctx ^BoolExpr bool-expr ]
  (let [solver (.mkSolver ctx)]
    (.add solver (into-array BoolExpr [bool-expr]))
    (let [^Solver status (.check solver)]
      (println "status:" status (.ordinal status))
      (case (.ordinal status) 
        2 :sat
        0 :unsat
        1 :unknown
        (throw (IllegalArgumentException. (str "unknow Status: " status)))))))

(def z3-single-expr)
(def z3)
;----------------------------------------------------------
(defmulti z3-single-symbol (fn [ident-fn params symbols ^Context ctx] (symbol ident-fn)))
(defmethod z3-single-symbol (symbol "=") 
  [ident-fn params symbols ^Context ctx]
  (.mkEq ctx 
    (z3-single-expr (first params) nil symbols ctx) 
    (z3-single-expr (second params) nil symbols ctx)))

(defmethod z3-single-symbol (symbol "mod") 
  [ident-fn params symbols ^Context ctx]
  (.mkMod ctx 
    (z3-single-expr (first params) nil symbols ctx) 
    (z3-single-expr (second params) nil symbols ctx)))

(defmethod z3-single-symbol :default
  [ident-fn params symbols ^Context ctx]
  (if-let [z3-obj (symbols ident-fn)]
    z3-obj
    (throw (IllegalArgumentException. (str "symbol:" ident-fn " unknow")))))
;-------------------------------------------------------------
(defmulti z3-single-expr (fn [ident-fn params symbols ^Context ctx] (class ident-fn)))
(defmethod z3-single-expr clojure.lang.Symbol 
  [ident-fn params symbols ^Context ctx]
  (z3-single-symbol ident-fn params symbols ctx))

(defmethod z3-single-expr clojure.lang.ISeq 
  [ident-fn params symbols ^Context ctx]
  (z3-single-symbol (first ident-fn) (rest ident-fn) symbols ctx))

(defmethod z3-single-expr java.lang.Integer 
  [ident-fn params symbols ^Context ctx]
  (println "int: " ident-fn)
  (.mkInt ctx ident-fn))

(defmethod z3-single-expr java.lang.Long 
  [ident-fn params symbols ^Context ctx]
  (println "long: " ident-fn)
  (.mkInt ctx ident-fn))

(defmethod z3-single-expr :default 
  [ident-fn params symbols ^Context ctx]
  (if-let [z3-obj (symbols ident-fn)]
    z3-obj
    (throw (IllegalArgumentException. (str "symbol:" ident-fn " unknow")))))
;---------------------------------------------
(defmulti z3 (fn [expr symbols ^Context ctx] (class expr)))

(defmethod z3 clojure.lang.Sequential
  [expr symbols ^Context ctx]
  
  (if-let [inner-expr (first expr)] 
    (if (seq? inner-expr)
      (z3 inner-expr)
      (z3-single-expr inner-expr (rest expr) symbols ctx))))

