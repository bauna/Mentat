(ns mentat.javaZ3
  (:require [mentat.core :as mc]
            [mentat.z3 :as z3])
  (:import (java.lang.reflect Method Modifier Field TypeVariable) 
           (com.microsoft.z3 Context Status Solver BoolExpr ArithExpr)
           (ar.com.maba.tesis.preconditions Pre ClassDefinition)
           (java.util.concurrent.atomic AtomicInteger AtomicLong AtomicBoolean)
           (java.util Set List Collection)))

(defn get-generic-type 
  [^Field f]
  (try  
    (-> f .getGenericType .getActualTypeArguments (aget 0))
    (catch java.lang.Throwable t java.lang.Integer)))

(defmulti get-sort-for-class 
  (fn [^Context ctx ^Class klass] klass))

(defmethod get-sort-for-class java.lang.Boolean
  [^Context ctx ^Class klass]
  [(.getBoolSort ctx) #(.mkBool ctx %)])

(defmethod get-sort-for-class java.util.concurrent.atomic.AtomicBoolean 
  [^Context ctx ^Class klass] 
  [(.getBoolSort ctx) #(.mkBool ctx (.get %))])

(defmethod get-sort-for-class java.lang.Byte 
  [^Context ctx ^Class klass] 
  [(.getIntSort ctx) #(.mkInt ctx (int %))])

(defmethod get-sort-for-class java.lang.Short 
  [^Context ctx ^Class klass] 
  [(.getIntSort ctx) #(.mkInt ctx (int %))])

(defmethod get-sort-for-class java.lang.Integer 
  [^Context ctx ^Class klass] 
  [(.getIntSort ctx) #(.mkInt ctx (int %))])

(defmethod get-sort-for-class java.util.concurrent.atomic.AtomicInteger 
  [^Context ctx ^Class klass] 
  [(.getIntSort ctx) #(.mkInt ctx (int %))])

(defmethod get-sort-for-class java.lang.Long 
  [^Context ctx ^Class klass] 
  [(.getIntSort ctx) #(.mkInt ctx (long %))])

(defmethod get-sort-for-class java.util.concurrent.atomic.AtomicLong 
  [^Context ctx ^Class klass] 
  [(.getIntSort ctx) #(.mkInt ctx (long (.get %)))])

(defmethod get-sort-for-class java.math.BigInteger 
  [^Context ctx ^Class klass] 
  [(.getIntSort ctx) #(.mkInt ctx (int %))])

(defmethod get-sort-for-class java.lang.Float 
  [^Context ctx ^Class klass] 
  [(.getRealSort ctx) #(.mkReal ctx (str %))])

(defmethod get-sort-for-class java.lang.Double 
  [^Context ctx ^Class klass] 
  [(.getRealSort ctx) #(.mkReal ctx (str %))])

(defmethod get-sort-for-class java.math.BigDecimal 
  [^Context ctx ^Class klass] 
  [(.getRealSort ctx) #(.mkReal ctx (str %))])

(defn def-list-field 
  [^Context ctx ^String name elems-sort const-fun array]
  (let [const (.mkArrayConst ctx name (.getIntSort ctx) elems-sort)
        func (fn [[exprs idx] val] 
               (let [val-expr (const-fun val)]
                 [(conj exprs (.mkEq ctx val-expr (.mkSelect ctx const (.mkInt ctx idx))))
                  (inc idx)]))]
    {:exprs (first (reduce func [[] 0] array))
     :const const}))

(defn def-java-util-list-field 
  [^Context ctx ^String name ^Field f ^Collection coll] 
  ;(println ctx name f array (get-generic-type f))
  (let [[elems-sort const-fun] (get-sort-for-class ctx (get-generic-type f))] 
    (def-list-field ctx name elems-sort const-fun coll)))

(defn def-boolean-array-field
  [^Context ctx ^String name ^List array]
  (let [[elems-sort const-fun] (get-sort-for-class ctx java.lang.Boolean)] 
    (def-list-field ctx name elems-sort const-fun array)))

(defn def-int-array-field
  [^Context ctx ^String name ^List array]
  (let [[elems-sort const-fun] (get-sort-for-class ctx java.lang.Integer)] 
    (def-list-field ctx name elems-sort const-fun array)))

(defn def-long-array-field
  [^Context ctx ^String name array]
  (let [[elems-sort const-fun] (get-sort-for-class ctx java.lang.Long)] 
    (def-list-field ctx name elems-sort const-fun array)))

(defn def-float-array-field
  [^Context ctx ^String name array]
  (let [[elems-sort const-fun] (get-sort-for-class ctx java.lang.Float)] 
    (def-list-field ctx name elems-sort const-fun array)))

(defn def-double-array-field
  [^Context ctx ^String name  array]
  (let [[elems-sort const-fun] (get-sort-for-class ctx java.lang.Double)] 
    (def-list-field ctx name elems-sort const-fun array)))


(defn def-boolean-field
  [^Context ctx ^String name ^Boolean value]
   (let [const (.mkBoolConst ctx name)] 
    {:exprs (.mkEq ctx const (.mkBool ctx value))
     :const const}))
  
(defn def-int-field
  [^Context ctx ^String name ^long value]
   (let [const (.mkIntConst ctx name)] 
    {:exprs [(.mkEq ctx const (.mkInt ctx (int value)))] 
     :const const}))

(defn def-long-field
  [^Context ctx ^String name ^long value]
   (let [const (.mkIntConst ctx name)] 
    {:exprs(.mkEq ctx const (.mkInt ctx (long value)))
     :const const}))

(defn def-bigInt-field
  [^Context ctx ^String name ^String value]
   (let [const (.mkIntConst ctx name)] 
    {:exprs (.mkEq ctx const (.mkInt ctx value))
     :const const}))

(defn def-double-field
  [^Context ctx ^String name ^String value]
   (let [const (.mkRealConst ctx name)] 
    {:exprs (.mkEq ctx const (.mkReal ctx value))
     :const const}))

(defn field-to-z3 
  [o ^Context ctx ^Field f]
  (let [field-type (.getType f)
        name (.getName f)
        v (.get f o)]
    (if (instance? Collection v)
      (def-java-util-list-field ctx name f v)
      (condp = field-type
        java.lang.Boolean (def-boolean-field ctx name v)
	      java.util.concurrent.atomic.AtomicBoolean (def-boolean-field ctx name (.get v))
	      java.lang.Byte (def-int-field ctx name v)
	      java.lang.Short (def-int-field ctx name v)
	      java.lang.Integer (def-int-field ctx name v)
	      java.util.concurrent.atomic.AtomicInteger (def-int-field ctx name (.get v))
	      java.lang.Long (def-long-field ctx name v)
	      java.util.concurrent.atomic.AtomicLong  (def-long-field ctx name (.get v))
	      java.math.BigInteger  (def-bigInt-field ctx name (str v))
	      java.lang.Float (def-double-field ctx name (str v))
	      java.lang.Double (def-double-field ctx name (str v))
	      java.math.BigDecimal (def-double-field ctx name (str v))
	      Boolean/TYPE (def-boolean-field ctx name v)
	      Byte/TYPE (def-int-field ctx name v)
	      Short/TYPE  (def-int-field ctx name v)
	      Integer/TYPE (def-int-field ctx name v)
	      Long/TYPE (def-long-field ctx name v)
	      Float/TYPE (def-double-field ctx name (str v))
	      Double/TYPE (def-double-field ctx name (str v))
	      (Class/forName "[Ljava.util.concurrent.atomic.AtomicBoolean;")  (def-boolean-array-field ctx name v)
	      (Class/forName "[Ljava.lang.Integer;")  (def-int-array-field ctx name v)
	      (Class/forName "[I")  (def-int-array-field ctx name v)
	      (Class/forName "[B")  (def-int-array-field ctx name v)
	      (Class/forName "[J")  (def-long-array-field ctx name v)
	      (Class/forName "[Z")  (def-boolean-array-field ctx name v)
	      (Class/forName "[F")  (def-float-array-field ctx name v)
	      (Class/forName "[D")  (def-double-array-field ctx name v)))))

(defn mk-instance
  "create a constant in the context for each instance field setting the value of the constant"
  ([^Context ctx o] (mk-instance ctx o (mc/get-all-fields (class o))))
  ([^Context ctx o fields] (mk-instance ctx o fields (mc/get-field-values o fields)))
  ([^Context ctx o fields fvalues]
    (reduce #(assoc %1 (keyword (.getName %2)) (field-to-z3 o ctx %2)) {} fields)))

(defn mk-constants-for-params
  "creates constants from method parameters"
  [^Context ctx ^Method m]
  (let [classes (.getParameterTypes m)
        f (fn [[params index] ^Class class] 
            (try 
              (let [[sort _] (get-sort-for-class ctx class)
                    name (str "p" index) 
                    key (keyword name) 
                    const (.mkConst ctx name sort)]
                [(assoc params key const ) (inc index)])
            (catch IllegalArgumentException e [params (inc index)])))]
    (first (reduce f [{} 0] classes))))
