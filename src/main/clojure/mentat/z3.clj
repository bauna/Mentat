(ns mentat.z3
  (:require [mentat.core :only gen-fn-key :as mc ])
  (:import (java.lang.reflect Method Modifier Field) 
           (com.microsoft.z3 Context Status Solver BoolExpr ArithExpr)
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
      (case (.toInt status) 
        1 :sat
        0 :unknown
        -1 :unsat
        (throw (IllegalArgumentException. (str "unknow Status: " status)))))))

(def z3-single-expr)
(def z3)
;----------------------------------------------------------
(defmulti z3-single-symbol 
  (fn [ident-fn params symbols inst-state ^Context ctx] (symbol ident-fn)))

(defmethod z3-single-symbol (symbol "=") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkEq ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "mod") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkMod ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "=>") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkImplies ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "iff") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkIff ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "xor") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkXor ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "<") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkLt ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "<=")
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkLe ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol ">") 
  [ident-fn params symbols inst-state ^Context ctx]
  (println ident-fn params symbols inst-state ctx)
  (.mkGt ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol ">=")
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkGe ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "and") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkAnd ctx (into-array BoolExpr (map #(z3-single-expr % nil symbols inst-state ctx) params))))

(defmethod z3-single-symbol (symbol "or") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkOr ctx (into-array BoolExpr (map #(z3-single-expr % nil symbols inst-state ctx) params))))

(defmethod z3-single-symbol (symbol "+") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkAdd ctx (into-array ArithExpr (map #(z3-single-expr % nil symbols inst-state ctx) params))))

(defmethod z3-single-symbol (symbol "*") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkMul ctx (into-array ArithExpr (map #(z3-single-expr % nil symbols inst-state ctx) params))))

(defmethod z3-single-symbol (symbol "-") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkSub ctx (into-array ArithExpr (map #(z3-single-expr % nil symbols inst-state ctx) params))))

(defmethod z3-single-symbol (symbol "not") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkNot ctx (z3-single-expr (first params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "/") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkDiv ctx 
    (z3-single-expr (first params) nil symbols inst-state ctx) 
    (z3-single-expr (second params) nil symbols inst-state ctx)))

(defmethod z3-single-symbol (symbol "true") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkTrue ctx))

(defmethod z3-single-symbol (symbol "false") 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkFalse ctx))

(defmethod z3-single-symbol (symbol "eval")
  [ident-fn params symbols inst-state ^Context ctx]
  (let [evalfn (mc/gen-fn-key (keys inst-state) params)
        ret (evalfn inst-state)]
    (z3-single-expr (evalfn inst-state) nil symbol inst-state ctx)))

(defmethod z3-single-symbol :default
  [ident-fn params symbols inst-state ^Context ctx]
  (if-let [z3-obj (symbols ident-fn)]
    z3-obj
    (throw (IllegalArgumentException. (str "symbol: '" ident-fn "' unknow")))))

;-------------------------------------------------------------
(defmulti z3-single-expr (fn [ident-fn params symbols inst-state ^Context ctx] (class ident-fn)))
(defmethod z3-single-expr clojure.lang.Symbol 
  [ident-fn params symbols inst-state ^Context ctx]
  (z3-single-symbol ident-fn params symbols inst-state ctx))

(defmethod z3-single-expr clojure.lang.ISeq 
  [ident-fn params symbols inst-state ^Context ctx]
  (z3-single-symbol (first ident-fn) (rest ident-fn) symbols inst-state ctx))

(defmethod z3-single-expr java.lang.Integer 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkInt ctx ident-fn))

(defmethod z3-single-expr java.lang.Long 
  [ident-fn params symbols inst-state ^Context ctx]
  (.mkInt ctx ident-fn))

(defmethod z3-single-expr java.lang.Boolean 
  [ident-fn params symbols inst-state ^Context ctx]
  (if ident-fn 
    (.mkTrue ctx) 
    (.mkFalse ctx)))

(defmethod z3-single-expr :default 
  [ident-fn params symbols inst-state ^Context ctx]
  (if-let [z3-obj (symbols ident-fn)]
    z3-obj
    (throw (IllegalArgumentException. (str "symbol:" ident-fn " unknow")))))
;---------------------------------------------
(defmulti z3 (fn [expr symbols inst-state ^Context ctx] (class expr)))

(defmethod z3 clojure.lang.Sequential
  [expr symbols inst-state ^Context ctx]
  
  (if-let [inner-expr (first expr)] 
    (if (seq? inner-expr)
      (z3 inner-expr)
      (z3-single-expr inner-expr (rest expr) symbols inst-state ctx))))
