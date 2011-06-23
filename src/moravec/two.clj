(ns moravec.two
  (:use [moravec.vars]
        [moravec.types]
        [moravec.utils :only [get-var]]
        [moravec.fn :only [class-writer]])
  (:refer-clojure :exclude [eval compile *ns* *source-path*])
  (:import (clojure.lang RT ISeq)
           (clojure.asm Type Opcodes)
           (clojure.asm.commons GeneratorAdapter Method)))

(defn analyze-dispatch [context form]
  (type form))

(defmulti analyze #'analyze-dispatch)

(defn analyze-seq-dispatch [context form]
  (first form))

(defmulti analyze-seq #'analyze-seq-dispatch)

(defmethod analyze ISeq [context form]
  (analyze-seq context form))

(defmethod analyze Number [context form]
  `(number {:v ~form :constant true}))

(defmethod analyze-seq :default [context form]
  (throw (dce.Exception. "unknown form" :compiler {:context context
                                                   :form form
                                                   :source *source*
                                                   :line *line*})))

(defmethod analyze-seq 'def [context form]
  (let [m {:line *line*
           :source *source*
           :var (get-var (second form))
           :dynamic? (boolean (:dynamic (meta (second form))))}
        m (if-let [init (second (rest form))]
            (assoc m :init (analyze
                            (if (= context :eval)
                              context
                              :expression)
                            init))
            m)]
    `(def ~(second form) ~m)))

(defn constant? [[_ m]]
  (:constant m))

(defmethod analyze-seq 'fn* [context form]
  (let [bodies (doall (map (partial analyze context)
                           (map (fn [x] `(fn-body ~x)) (rest form))))
        constants (set (filter constant? (filter seq? (tree-seq coll? seq bodies))))]
    `(moravec.two/fn ~{:form form
                       :line *line*
                       :bodies bodies
                       :constants constants})))

(defmethod analyze-seq `fn-body [context form]
  `(fn-body ~(first (second form))
            ~(analyze context (cons 'do (rest (second form))))))

(defmethod analyze-seq 'do [context form]
  `(do ~@(doall (map (partial analyze context) (rest form)))))

(defn evalx-dispatch [form] (first form))

(defmulti emit-value (fn [_ x] (first x)))

(defmethod emit-value `number [clinitgen [_ {:keys [v]}]]
  (doto clinitgen
    (.push (.intValue v))
    (.invokeStatic (Type/getType Integer) (Method/getMethod "Integer valueOf(int)"))))

(defmulti constant-type first)

(defmethod constant-type `number [{:keys [v]}]
  (Type/getType Integer))

(defmulti emit (fn [o & _] (first o)))

(defmethod emit 'moravec.two/fn-body [[_ args body] expr cw]
  (println "expr" expr)
  (let [m (Method. "invoke" object-type (into-array Type (repeat (count args) object-type)))
        gen (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)
              (.visitCode))
        loop-label (.mark gen)]
    (.visitLineNumber gen *line* loop-label)
    (binding [*loop-label* loop-label]
      (println "body" body)
      (emit body expr :return))
    (doto gen
      (.returnValue)
      (.endMethod))))

(defmethod emit 'do [[_ & forms] expr cw]
  (println "forms" forms)
  (condp = (count forms)
      1 (emit forms expr cw :return)
      (let [lb (last forms)
            st (butlast forms)]
        (doseq [s st]
          (emit s expr cw :statement))
        (emit lb expr cw :return))))

(defmulti evalx #'evalx-dispatch)

(defmethod evalx 'moravec.two/fn [[_ form]]
  (let [fn-name (str "moravec." (gensym))
        fn-internal-name (.replace fn-name "." "/")
        cw (class-writer fn-internal-name (Type/getInternalName clojure.lang.AFn))
        m (Method. "<init>" Type/VOID_TYPE (into-array Type []))
        gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)
        constants (reduce
                   (fn [accum form]
                     (if (contains? accum form)
                       accum
                       (assoc accum form (count accum))))
                   {}
                   (:constants form))
        clinitgen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC
                                        Opcodes/ACC_STATIC)
                                     (Method/getMethod "void <clinit>()")
                                     nil nil cw)
        form (assoc form :name fn-name)]
    (doto gen
      (.visitCode)
      (.loadThis)
      (.invokeConstructor (Type/getType clojure.lang.AFn) (Method/getMethod "void <init>()"))
      (.returnValue)
      (.endMethod))
    (doseq [[c id] constants]
      (.visitField cw (+ Opcodes/ACC_PUBLIC
                         Opcodes/ACC_STATIC
                         Opcodes/ACC_FINAL)
                   (str "const__" id)
                   (.getDescriptor (Type/getType (class (:v (second c)))))
                   nil
                   nil))
    (when-not (empty? constants)
      (doseq [[c id] (sort-by second constants)]
        (emit-value clinitgen c)
        (doto clinitgen
          (.checkCast (constant-type c))
          (.putStatic (Type/getObjectType fn-name) (str "const__" id) (constant-type c)))))
    (doto clinitgen
      (.returnValue)
      (.endMethod))
    (doseq [b (:bodies form)]
      (emit b form cw))))

(defn eval
  ([form] (eval form true))
  ([form fresh-loader?]
     (binding [*loader* (RT/makeClassLoader)
               *line* (or (:line (meta form)) *line*)]
       (evalx (analyze :eval form)))))
