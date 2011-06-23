(ns moravec.five
  (:require [clojure.zip :as z])
  (:refer-clojure :exclude [eval]))

(defmulti branch? type)

(defmulti children type)

(defmulti make-node (fn [o & _] (type o)))

(def gzip (partial z/zipper branch? children make-node))

(defmethod branch? clojure.lang.ISeq [form]
  (not= 'quote (first form)))

(defmethod children clojure.lang.ISeq [form] form)

(defmethod branch? clojure.lang.Symbol [form] false)

(defmethod branch? Number [form] false)

(defmethod branch? clojure.lang.IPersistentVector [form] true)

(defmethod children clojure.lang.IPersistentVector [form] (seq form))

(defn closed-over? [x form]
  (loop [zz (gzip form) new-fn false]
    (cond
     (z/end? zz) false
     (= 'let* (z/node zz)) (let [bindings (z/node (z/next zz))
                                 names (set (take-nth 2 bindings))]
                             (if (contains? names x)
                               (recur (z/right (z/prev zz)) new-fn)
                               (recur (z/next zz) new-fn)))
     (= 'fn* (z/node zz))  (recur (z/next zz) true)
     (and new-fn (= (z/node zz) x)) true
     :else (recur (z/next zz) new-fn))))
