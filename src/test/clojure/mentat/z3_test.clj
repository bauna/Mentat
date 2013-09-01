(ns mentat.z3-test
  (:use clojure.test)
  (:require [mentat.core :as mc]
            [mentat.z3 :as z3]
            [mentat.javaZ3 :as jz3])
  (:import (ar.com.maba.tesis.collections NumbersToZ3)))


(deftest java-z3-mappings-test
  (testing "FAILED: Testing different supported java to Z3 mappings"
  (let [ctx (z3/create-context)
        i2z3 (NumbersToZ3.)]
    (jz3/mk-instance ctx i2z3))))

