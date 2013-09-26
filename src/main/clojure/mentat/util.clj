(ns mentat.util)

(defn ==>
  "implies function"
  [p q]
  (or (not p) q))