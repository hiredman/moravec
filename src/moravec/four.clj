(ns moravec.four
  (:refer-clojure :exclude [eval])
  (:use [match.core :only [defmn mn]]))

;; TODO: deal with primitive args somehow?
;; TODO: handle apply, handle var args, handle everything.
;; TODO: needs to generate a replacement for AFn
;; TODO: find closed over locals, turn into fields
;; TODO: args need to be (arg ...)
;; TODO: clojure.lang.RestFn
;; TODO: once support
(defn fn->deftype [[_ & body] env]
  (let [class-name (gensym 'x.fn)]
    `(do
       (deftype*
         IFn
         ~class-name
         []
         :implements [clojure.lang.Fn
                      clojure.lang.IFn]
         ~@(for [[args & bs] body]
             `(~'invoke ~(vec (cons '_ args))
                        ~@bs)))
       (new ~class-name))))

(defprotocol CodeGenerator
  (new-procedure-group [cg name interfaces])
  (new-procedure [cg name args])
  (end-procedure [cg])
  (new-type [cw name arity])
  (parameter [cg name])
  (native-call [cg target name])
  (finish [cw name]))

(defmn secd
  [(?gen . nil) ?e () ((:proc-group ?s ?c) . ?d)]
  (do
    (end-procedure gen)
    (recur s e c d))

  ;; Arguments
  [?s ?e ((m/arg ?n) . ?c) ?d]
  (do
    (parameter (first s) n)
    (recur s e c d))

  [?s ?e ((m/dot . ?args) . ?c) ?d]
  ;; TODO: use top of stack to create native call generator
  ;; TODO: native call generator
  (let [stack s
        env e
        control c
        dump d]
    (recur stack env control dump))

  ;; (new ... ...)
  [(?gen . ?s) ?e ((new ?n) . ?c) ?d]
  (do
    ;;TODO: need something in place of nil here
    (new-type gen n nil)
    (recur (cons gen s) e c d))

  ;; TODO: deftype* needs to support static fields and static init
  [(?gen . ?s) ?e ((deftype* . ?body) . ?cs) ?d]
  (let [[tag-name class-name fields _ interfaces & bodies] body
        npg (new-procedure-group gen class-name interfaces)
        dump (cons [cs (cons gen s) class-name] d)
        stack (list npg)
        bodies (for [body bodies]
                 (cons 'proc body))]
    (recur stack e bodies dump))

  [?s ?e ((proc ?name ?args . ?body) . ?c) ?d]
  (recur (list (new-procedure (first s) name (rest args)))
         e
         body
         (cons [:proc-group s c] d))

  [(?gen . nil) ?e () ((?c ?s ?cn) . ?d)]
  (do
    (finish gen cn)
    (recur s e c d))

  ;; (do ...)
  [?s ?e ((do . ?body) . ?cs) ?d]
  (recur s e (concat body cs) d)

  [(?x . nil) ?_ () ()]
  x

  [?s ?e ?c ?d]
  (prn s e c d))
