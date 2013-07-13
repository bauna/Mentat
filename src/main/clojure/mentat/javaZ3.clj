(ns mentat.javaZ3
  (:require [mentat.core :as mc ]
            [mentat.z3 :as z3])
  (:import (java.lang.reflect Method Modifier Field) 
           (com.microsoft.z3 Context Status Solver BoolExpr ArithExpr)
           (ar.com.maba.tesis.preconditions Pre ClassDefinition)
           (java.util.concurrent.atomic AtomicInteger AtomicLong AtomicBoolean)))

(defn def-boolean-field
  [^Context ctx ^String name ^Boolean value]
   (let [const (.mkBoolConst ctx name)] 
    (.mkEq ctx const (.mkBool ctx value))
    const))
  
(defn def-int-field
  [^Context ctx ^String name ^long value]
   (let [const (.mkIntConst ctx name)] 
    (.mkEq ctx const (.mkInt ctx (int value)))
    const))

(defn def-long-field
  [^Context ctx ^String name ^long value]
   (let [const (.mkIntConst ctx name)] 
    (.mkEq ctx const (.mkInt ctx (long value)))
    const))

(defn def-bigInt-field
  [^Context ctx ^String name ^String value]
   (let [const (.mkIntConst ctx name)] 
    (.mkEq ctx const (.mkInt ctx value))
    const))

(defn def-double-field
  [^Context ctx ^String name ^String value]
   (let [const (.mkIntConst ctx name)] 
    (.mkEq ctx const (.mkReal ctx value))
    const))

(defn field-to-z3 
  [o ^Context ctx ^Field f]
  (let [field-type (.getType f)
        name (.getName f)
        v (.get f o)]
           (cond
             (= java.lang.Boolean field-type) (def-boolean-field ctx name v)
						 (= java.util.concurrent.atomic.AtomicBoolean field-type) (def-boolean-field ctx name (.get v))
						 (= java.lang.Byte field-type) (def-int-field ctx name v)
						 (= java.lang.Short field-type) (def-int-field ctx name v)
						 (= java.lang.Integer field-type) (def-int-field ctx name v)
						 (= java.util.concurrent.atomic.AtomicInteger field-type) (def-int-field ctx name (.get v))
						 (= java.lang.Long field-type) (def-long-field ctx name v)
						 (= java.util.concurrent.atomic.AtomicLong field-type) (def-long-field ctx name (.get v))
						 (= java.math.BigInteger field-type) (def-bigInt-field ctx name (str v))
						 (= java.lang.Float field-type) (def-double-field ctx name (str v))
						 (= java.lang.Double field-type) (def-double-field ctx name (str v))
						 (= java.math.BigDecimal field-type) (def-double-field ctx name (str v))
						 (= Boolean/TYPE field-type) (def-boolean-field ctx name v)
						 (= Byte/TYPE field-type) (def-int-field ctx name v)
						 (= Short/TYPE field-type) (def-int-field ctx name v)
						 (= Integer/TYPE field-type) (def-int-field ctx name v)
						 (= Long/TYPE field-type) (def-long-field ctx name v)
						 (= Float/TYPE field-type) (def-double-field ctx name (str v))
						 (= Double/TYPE field-type) (def-double-field ctx name (str v))
             :else (throw (IllegalArgumentException. (str "unsupported field type: " field-type ", name: " name))))))

(defn mk-instance
  "create a constant in the context for each instance field setting the value of the constant"
  [^Context ctx o]
  (let [fields (mc/get-all-fields (class o))
        fvalues (mc/get-field-values o fields)]
    (reduce #(assoc %1 (keyword (.getName %2)) (field-to-z3 o ctx %2)) {} fields)))
