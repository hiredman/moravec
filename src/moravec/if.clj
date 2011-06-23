(ns moravec.if
  (:use [moravec.vars])
  (:require [moravec.expr :as expr])
  (:refer-clojure :exclude [eval *ns* *source-path*]))

(defn if-parser [context form]
  ;;TODO: all kinds of crazy path stuff
  (when (> (count form) 4)
    (throw (dce.Exception "Too many arguments to if" :compiler {:source-path *source-path*
                                                                :line *line*
                                                                :context context
                                                                :form form})))
  (when (< (count form) 3)
    (throw (dce.Exception "Too few arguments to if" :compiler {:source-path *source-path*
                                                               :line *line*
                                                               :context context
                                                               :form form})))
  (with-meta
    (list 'if
          ((resolve 'analyze)
           (if (= :eval context)
             :eval
             :expression)
           (second form))
          ((resolve 'analyze) context (second (rest form)))
          ((resolve 'analyze) context (second (rest (rest form)))))
    {:line *line*}))

(defmethod expr/eval 'if [[_ test then else]]
  (if (expr/eval test)
    (expr/eval then)
    (expr/eval else)))
