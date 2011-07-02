(ns moravec.four
  (:refer-clojure :exclude [eval])
  (:use [match.core :only [defmn mn]]
        [moravec.five]))

(defn normalize-fn [form]
  (loop [[cur & forms] form result {:bodies []}]
    (if cur
      (cond
       (and (= cur 'fn*)
            (symbol? (first forms)))
       (recur (rest forms) (assoc result :fn-name (first forms)))
       (= cur 'fn*)
       (do
         (when-not (or (every? seq? forms)
                       (vector? (first forms)))
           (throw (Exception. "malformed fn form")))
         (recur forms result))
       (vector? cur)
       (recur nil (assoc result :bodies [(list* cur forms)]))
       (seq? cur)
       (recur forms (update-in result [:bodies] conj cur))
       :else
       (throw (Exception. "unknown fn form")))
      (with-meta
        (if (:fn-name result)
          `(fn* ~(:fn-name result)
                ~@(:bodies result))
          `(fn* ~@(:bodies result)))
        (meta form)))))

;; TODO: generate proper constructors for closed over stuff
;; TODO: general macro for walking code and rewriting
;; TODO: deal with primitive args somehow?
;; TODO: handle apply, handle var args, handle everything.
;; TODO: needs to generate a replacement for AFn
;; TODO: clojure.lang.RestFn
;; TODO: once support
;; TODO: let support
(defn fn->deftype [form env]
  (let [class-name (gensym (str *ns* "$" 'fn))
        closed-over (vec (filter #(closed-over? % form) (keys env)))
        [_ & body] (normalize-fn form)
        bodies (for [[args & bs] body]
                 `(~'invoke ~(vec (cons '_ args)) ~@bs))]
    `(do
       (deftype*
         IFn
         ~class-name
         ~closed-over
         :implements [clojure.lang.Fn
                      clojure.lang.IFn]
         ~@bodies)
       (new ~class-name ~@closed-over))))

(defprotocol CodeGenerator
  (new-procedure-group [cg name interfaces])
  (new-procedure [cg name args])
  (end-procedure [cg])
  (parameter [cg name])
  (native-call [cg target name])
  (finish [cw name])
  (local [cw name env])
  (field [cw name])
  (constructor [cw name])
  (write-constructor [cw fields env]))

(defprotocol ConstructorBuilder
  (arguments [cw])
  (end-ctor-call [cw argc]))

;; TODO: s doesn't seem to need to be a stack

(defmn secd
  
  ;; methods for deftype
  [(?gen . nil) ?e () ((:proc-group ?s ?c) . ?d)]
  (do
    (prn 'methods)
    (end-procedure gen)
    (prn 'methods2)
    (recur s e c d))

  [?s ?e ((proc ?name ?args . ?body) . ?c) ?d]
  (do
    (prn 'new-proc)
    (recur (list (new-procedure (first s) name (rest args)))
           (into e (map vector args
                        (map (partial vector :arg) (map dec (range)))))
           body
           (cons [:proc-group s c] d)))

  [?s ?e ((make-field ?f) . ?c) ?d]
  (do
    (prn 'make-field)
    (field (first s) f)
    (recur s e c d))

  [?s ?e ((fn* . ?args) . ?c) ?d]
  (do
    (prn 'fn*)
    (recur s e (list* (fn->deftype `(fn* ~@args) e) c) d))
  
  [(?gen . ?s) ?e ((m/dot . ?args) . ?c) ?d]
  ;; TODO: use top of stack to create native call generator
  ;; TODO: native call generator
  (let [stack []
        env e
        control c
        dump (list* [:native e c s]  d)]
    (recur stack env control dump))

  [?sx ?ex ((m/new ?n ?argc) . nil) ((?s ?e ?c) . ?d)]
  (do
    (prn 'm/new)
    (prn 'stack sx)
    #_(new-type (first sx) n argc)
    (throw (Exception. "m/new"))
    (recur s e c d))

  ;; (new ... ...)
  [?s ?e ((new ?n . ?args) . ?c) ?d]
  (let [constructor-builder (constructor (first s) n)
        stack (list (arguments constructor-builder) constructor-builder)
        env e
        control (concat args [`(m/end-constructor ~(count args))])
        dump (list* [s e c] d)]
    (prn 'control-new control)
    (recur stack env control dump))

  [(?sx ?cx . nil) ?ex ((m/end-constructor ?argc) . nil) ((?s ?e ?c) . ?d)]
  (do
    (end-ctor-call cx argc)
    (recur s e c d))

  [?s ?e ((m/create-constructor ?fields) . ?c) ?d]
  (do
    (write-constructor (first s) fields e)
    (recur s e c d))
  
  ;; TODO: deftype* needs to support static fields and static init
  [(?gen . ?s) ?e ((deftype* . ?body) . ?cs) ?d]
  (let [[tag-name class-name fields _ interfaces & bodies] body
        npg (new-procedure-group gen class-name interfaces)
        dump (cons [cs (cons gen s) class-name e] d)
        stack (list npg)
        bodies (for [body bodies]
                 (cons 'proc body))
        fields-ins (for [f fields]
                 (list 'make-field f))
        e (assoc (->> (for [[n [kind idx]] e]
                        [n [:closed-over -1]])
                      (into {}))
            :this-class ['_ class-name])]
    (recur stack e
           (concat fields-ins [`(m/create-constructor ~fields)] bodies) dump))


  ;; (do ...)
  [?s ?e ((do . ?body) . ?cs) ?d]
  (do
    (prn 'do)
    (recur s e (concat body cs) d))

  [(?x . nil) ?_ () ()]
  x
  
  [?s ?e (?c . ?cs) ?d]
  (do (println 'stack s)
      (let [gen (first s)
            s (rest s)]
        (do
          (prn 'something)
          (prn 'stack s)
          (prn 'gen gen)
          (condp isa? (type c)
            clojure.lang.Symbol (if (contains? e c)
                                  (do
                                    (local gen c e)
                                    (recur (cons gen s) e cs d))
                                  (do
                                    (println 'control c cs)
                                    (println 'stack s gen)
                                    (throw (Exception. "var resultion"))))
            (throw (Exception. "blarg"))))))

  [(?gen . nil) ?_ () ((?c ?s ?cn ?e) . ?d)]
  (do
    (prn 'finish)
    (prn 'gen gen)
    (prn 'c c 's s 'cn cn 'e e 'd d)
    (finish gen cn)
    (prn 'post-finish)
    (recur s e c d))
  
  [?s ?e ?c ?d]
  (prn 'fallback s e c d)
)
