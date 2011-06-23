(ns moravec.def
  (:use [moravec.utils :only [lookup-var]]
        [moravec.vars])
  (:require [moravec.expr :as expr])
  (:refer-clojure :exclude [eval *ns* *source-path*]))

(defn extract-parts [form]
  (if (and (= 4 (count form))
           (string? (second (rest form))))
    {:doc (second (rest form))
     :form (list* (first form)
                  (second form)
                  (drop 3 form))}
    {:form form}))

(defn check-arity! [form]
  (cond
   (> (count form) 3) (throw (dce.Exception. "Too many arguments to def" :compiler {:form form}))
   (> 2 (count form)) (throw (dce.Exception. "Too few arguments to def" :compiler {:form form}))
   (not (symbol? (second form))) (throw
                                  (dce.Exception.
                                   "First argument to def must be a symbol"
                                   :compiler {:form form})))
  form)

(defn get-var [sym]
  (let [v (lookup-var sym true)]
    (if-not v
      (throw (dce.Exception. "Can't refer to qualified var that doesn't exist" :compiler
                             {:sym sym}))
      (let [v  (if (not= *ns* (.ns v))
                 (if (nil? v)
                   (.intern *ns* sym)
                   (throw
                    (dce.Exception. "Can't create defs outside of current ns"
                                    :compiler
                                    {:sym sym :ns *ns*})))
                 v)]
        v))))

(defn def-parser [context form]
  (let [{:keys [doc form]} (extract-parts form)
        _ (check-arity! form)
        sym (second form)
        v (get-var sym)
        mm (meta sym)
        dynamic? (boolean (:dynamic mm))
        _ (when (:arglists mm)
            (alter-meta! v assoc :arglists (second (:arglists mm))))
        mm (assoc mm
             :line *line*
             :file (or "NO_SOURCE_FILE" *source-path*))
        mm (if doc
             (assoc mm :doc doc)
             mm)
        meta (when (empty? mm)
               ((resolve 'moravec.one/analyze)
                (if (= context :eval)
                  context
                  :expression)
                mm))]
    (list 'def
          {:source *source*
           :line *line*
           :var v
           :init ((resolve 'moravec.one/analyze)
                  (if (= context :eval)
                    context
                    :expression)
                  (second (rest form))
                  (name (.sym v)))
           :meta meta
           :init-provided? (= 3 (count form))
           :dynamic? dynamic?})))

(defmethod expr/eval 'def [[_ {:keys [var init-provided? init meta dynamic?]}]]
  (when init-provided?
    (.bindRoot var (expr/eval init)))
  (when meta
    (alter-meta! var (constantly (expr/eval meta))))
  (if (.startsWith (clojure-version) "1.3")
    (.setDynamic var dynamic?)
    var))

