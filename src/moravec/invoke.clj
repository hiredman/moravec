(ns moravec.invoke
  (:use [moravec.utils :only [tag-of]]
        [moravec.vars])
  (:require [moravec.expr :as expr])
  (:refer-clojure :exclude [eval *ns* *source-path*]))

(defn parse-invoke [context form]
  (let [context (if (= context :eval)
                  :eval
                  :expression)
        fexpr ((resolve 'analyze) context (first form))]
    (with-meta
      {:source *source*
       :line *line*
       :tag (tag-of form)
       :fexpr fexpr
       :args (vec (map (fn [arg] ((resolve 'analyze) context arg)) (next form)))}
      {:type :invoke})))

(defmethod expr/eval :invoke [{:keys [fexpr args] :as m}]
  (try
    (apply (expr/eval fexpr) (map expr/eval args))
    (catch Throwable e
      (cond
       (not (instance? dce.Exception e)) (throw
                                          (dce.Exception.
                                           (.getMessage e)
                                           e :compiler {:source-path *source-path*
                                                        :line *line*}))
       (not= :compiler (type (.payload e))) (throw
                                             (dce.Exception.
                                              (.getMessage e)
                                              e :compiler (assoc (.payload e)
                                                            :source-path *source-path*
                                                            :line *line*)))
       :else (throw e)))))
