(ns moravec.one
  (:refer-clojure :exclude [eval *ns* *source-path* compile])
  (:use [moravec.def :only [def-parser]]
        [moravec.if :only [if-parser]]
        [moravec.invoke :only [parse-invoke]]
        [moravec.utils :only [lookup-var register-var tag-of constant?
                              constant-type constant-name]]
        [moravec.vars])
  (:require [moravec.expr :as expr]
            [moravec.fn])
  (:import (clojure.lang Namespace Symbol RT)
           (clojure.asm Type)))

(defn number-parser [n]
  (if (or (instance? Integer n)
          (instance? Double n)
          (instance? Long n))
    (with-meta {:n n} {:type  ::number})
    (throw
     (dce.Exception. "wtf" :compiler {:number n :line *line* :source *source*}))))

(defmethod expr/eval ::number [{:keys [n]}] n)

(defn emit-constant [method gen id]
  (.getStatic gen
              (Type/getType (str "L" (:internal-name (second method)) ";"))
              (constant-name id)
              (constant-type id (:constants (second method)))))

(defmethod expr/emit ::number [{:keys [n]} context method gen]
  (when-not (= context :statement)
    (emit-constant method gen (get (:constants (second method)) n))))

(defmethod expr/eval :var-expr [{:keys [var]}] @var)

(defmethod expr/eval :boolean [{:keys [v]}] v)

(defmethod expr/eval :string [{:keys [string]}] string)

(defmethod expr/eval :nil [_])

(defn var-parser [context [_ sym]]
  (if-let [v (lookup-var sym false)]
    (list 'var v)
    (throw (dce.Exception. (str "Unabled to resolve var: " sym " in this context")
                           :compiler {:sym sym :line *line* :source *source*}))))

(defmethod expr/eval 'var [[_ v]] v)

(defn do-parser [context [_ & forms]]
  (condp = (count forms)
      0 (with-meta {} {:type :nil})
      1 ((resolve 'analyze) context (first forms))
      (let [lform (last forms)
            forms (butlast forms)]
        (list 'do
              (concat (map (partial (resolve 'analyze) :statement) forms)
                      [((resolve 'analyze) context lform)])))))

(defmethod expr/eval 'do [[_ forms]]
  (->> (map expr/eval forms)
       doall
       last))

(defn get-and-inc-local-num! []
  (let [a *next-local-num*]
    (set! *next-local-num* (inc *next-local-num*))
    a))

(defn register-local [sym tag init arg?]
  (let [num (get-and-inc-local-num!)
        b {:num num
           :sym sym
           :tag tag
           :init init
           :arg? arg?}]
    (set! *env* (assoc *env* sym b))
    (swap! *method* update-in [:locals] assoc b b)
    (swap! *method* update-in [:index-locals] assoc num b)
    b))

(defn query-constants [constants form]
  (->> (tree-seq coll? seq form)
       (remove coll?)
       (reduce
        (fn [constants form]
          (if (and (constant? form)
                   (not (contains? constants form)))
            (assoc constants form (count constants))
            constants))
        constants)))

(defn fn-parse-method [internal-name constants [params & form]]
  ;; TODO: all kinds of complicated path stuff
  (binding [*method* (atom {:parent *method*
                            :line *line*
                            :locals {}
                            :index-locals []})
            *env* *env*
            *loop-locals* nil
            *next-local-num* 0]
    (let [arg-locals (reduce
                      (fn [arg-locals p]
                        (when-not (symbol? p)
                          (throw
                           (dce.Exception. "fn params must be symbols"
                                           :compiler {:params params :form form
                                                      :line *line* :source *source*})))
                        (when-not (nil? (namespace p))
                          (dce.Exception. (str "Can't use qualified name as parameter: " p)
                                          :compiler {:params params :form form
                                                     :line *line* :source *source*}))
                        (conj arg-locals (register-local p (tag-of p) nil true)))
                      [] params)]
      (set! *loop-locals* arg-locals)
      (with-meta
        (list 'fn-method
              {:tag (tag-of params)
               :method *method*
               :constants (query-constants constants form)
               :internal-name internal-name}
              arg-locals
              (do-parser :return `(do ~@form)))
        {:line *line*}))))

(defn fn-parser [context form fn-name]
  )

(defmethod expr/eval 'fn [fn-desc]
  (expr/compile fn-desc)
  (.newInstance (expr/get-compiled-class fn-desc)))

(defmethod expr/get-compiled-class 'fn [[_ {:keys [fn-name]}]]
  (Class/forName fn-name))

(def specials
  `{def ~def-parser
    var ~var-parser
    if ~if-parser
    do ~do-parser})

(defn analyze-seq [context form name]
  (binding [*line* (or (:line (meta form)) *line*)]
    (try
      (let [[op] form]
        (if (= op 'fn*)
          (fn-parser context form name)
          (if-let [p (specials op)]
            (p context form)
            (parse-invoke context form))))
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
         :else (throw e))))))

(defn close-over [b method]
  (when (and b method)
    (if (nil? (get (:locals @method) b))
      (do
        (swap! method :closes b b)
        (recur b (:parent @method)))
      (when *in-catch-finally*
        (swap! method :used-in-catch-finally conj (:num b))))))

(defn reference-local [sym]
  (when (bound? #'*env*)
    (let [b (get *env* sym)]
      (when b
        (close-over b *method*))
      b)))

(defn emit-local [fn-desc gen b]
  (cond
   (contains? (:closes @*method*) b)
   (throw (dce.Exception. "cannot close over for now" :compiler {:b b
                                                                 :source *source*
                                                                 :line *line*}))
   (:arg? b) (expr/emit (list 'arg b fn-desc gen))
   :else (throw (dce.Exception. "unhandled local" :compiler {:b b
                                                             :source *source*
                                                             :line *line*}))))

(defmethod expr/emit 'local [[_ b tag] context fn-desc gen]
  (when-not (= :statement context)
    (emit-local fn-desc gen b)))

(defn analyze-symbol [sym]
  (let [tag (tag-of sym)]
    ;;TODO: locals
    (if-let [b (and (nil? (namespace sym))
                    (reference-local sym))]
      (list 'local b tag)
      (let [o (resolve sym)]
        (if (var? o)
          (if (:macro (meta o))
            (throw
             (dce.Exception. (str "Can't take the value of a macro: " o) :compiler {:sym sym}))
            (if (and (.startsWith (clojure-version) "1.3")
                     (:const (meta o)))
              ((resolve 'analyze) :expression (list 'quote @o))
              (do
                (register-var o)
                (with-meta
                  {:var o
                   :tag tag}
                  {:type :var-expr})))))))))

(defn analyze-dispatch [context form & _] (type form))

(defmulti analyze #'analyze-dispatch)

(defmethod analyze :default [context form & [name]]
  (try
    (cond
     (or (true? form)
         (false? form)) (with-meta {:v form} {:type :boolean})
         (string? form) (with-meta {:string form} {:type :string})
         (symbol? form) (analyze-symbol form)
         (seq? form) (analyze-seq context form name)
         :else (throw (dce.Exception. (str "wtf " form) :compiler {:form form})))))

(defmethod analyze Number [context form & [name]]
  (number-parser form))

(defn eval
  ([form] (eval form true))
  ([form fresh-loader?]
     (binding [*loader* (RT/makeClassLoader)
               *line* (or (:line (meta form)) *line*)]
       (expr/eval (doto (analyze :eval form) prn)))))

(comment



  )
