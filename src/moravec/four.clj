(ns moravec.four
  (:refer-clojure :exclude [eval])
  (:use [match.core :only [defmn mn]])
  (:import (clojure.asm ClassWriter Opcodes Type)
           (clojure.asm.commons Method GeneratorAdapter)))

;; transforms clojure code into abstract opcodes
(defmulti line-up type)

(defmulti line-up-seq first)

(defmethod line-up clojure.lang.ISeq [n]
  (line-up-seq n))

(defmethod line-up-seq '. [[_ class-or-obj & args]]
  (let [[method-name & args] (if (and (seq? (first args))
                                      (= 1 (count args)))
                               (first args)
                               args)
        class-or-obj (line-up class-or-obj)]
    (if (and (= 1 (count class-or-obj))
             (= :name (first (first class-or-obj))))
      (let [n (resolve (second (first class-or-obj)))]
        (if (and n (class? n))
          (concat (mapcat line-up args)
                  [[:call-proc (symbol (name (second (first class-or-obj)))
                                       (name method-name)) (count args)]])
          (concat class-or-obj
                  (mapcat line-up args)
                  [[:call-method method-name (count args)]])))
      (concat class-or-obj
              (mapcat line-up args)
              [[:call-method method-name (count args)]]))))

(defmulti line-up-quote type)

(defmethod line-up-quote clojure.lang.IPersistentMap [form]
  (line-up (zipmap (map #(list 'quote %) (keys form))
                   (map #(list 'quote %) (vals form)))))

(defmethod line-up-quote clojure.lang.IPersistentVector [form]
  (line-up (vec (map #(list 'quote %) form))))

(defmethod line-up-quote clojure.lang.Symbol [form]
  [[::symbol form]])

(defmethod line-up-quote clojure.lang.Keyword [form]
  (line-up form))

(defmethod line-up-seq 'quote [[_ form]] (line-up-quote form))

(defn line-up-fn-body [[args & body]]
  [[:invoke (count args)]
   (concat [[:push-method-frame args]]
           (mapcat line-up body)
           [[:pop-frame (set args)]])])

(defmethod line-up-seq 'fn* [form]
  (let [[this-fn form] (if (symbol? (second form))
                         [(second form) (list* (first form) (rest (rest form)))]
                         [(gensym "fn__") form])
        n this-fn
        b (into {} (map line-up-fn-body (rest form)))]
    [(list
      'fn
      {:name n
       :procs b})]))

(defmethod line-up-seq 'do [[_ & forms]]
  (mapcat line-up forms))

(defmethod line-up-seq 'def [[_ var-name init]]
  (concat (line-up init)
          [[:bind-var var-name]]))

(defmethod line-up-seq 'let* [[_ bindings & body]]
  (let [locals (set (take-nth 2 bindings))
        x (concat (->> (for [[n v] (map vector (take-nth 2 bindings) (take-nth 2 (rest bindings)))
                             x [(->> (line-up v)
                                     (map (fn [[t v :as stmt]]
                                            (if (= t :name)
                                              (if (locals v)
                                                [:local v]
                                                [:name v])
                                              stmt))))
                                [[:bind n]]]]
                         x)
                       (apply concat))
                  (->> (mapcat line-up body)
                       (map (fn [[t v :as stmt]]
                              (if (= t :name)
                                (if (locals v)
                                  [:local v]
                                  [:name v])
                                stmt)))))]
    (concat [[:push-frame locals]]
            x
            [[:pop-frame locals]])))

(defmethod line-up-seq 'if [[_ test then else]]
  (let [else-label (gensym 'else__)
        end-if (gensym 'end-if__)]
    (concat (line-up test)
            [[:jump-nif else-label]]
            (line-up then)
            [[:jump end-if]]
            [[:label else-label]]
            (line-up else)
            [[:label end-if]])))

(defmethod line-up-seq 'try [form]
  (let [lst (last form)
        finally (when (= 'finally (first lst))
                  (mapcat line-up (rest lst)))
        form (when finally
               (butlast form))
        try-label (gensym 'try__)
        try-end-label (gensym 'try-end__)
        finally-label (gensym 'finally__)]
    (concat [[:label try-label]]
            (mapcat line-up (rest form))
            [[:label try-end-label]
             [:handle Throwable try-label try-end-label]
             [:jump finally-label]
             [:label finally-label]]
            finally)))

(defmethod line-up-seq 'var [[_ name]]
  [[::var (resolve name)]])

(defmethod line-up-seq :default [form]
  (let [form1 (loop [form form form1 (macroexpand form)]
                (if-not (= form1 form)
                  (recur form1 (macroexpand form1))
                  form1))]
    (if-not (= form1 form)
      (line-up form1)
      (let [[op & args] form1
            argc (count args)]
        (concat (line-up op)
                [[:fn-call]]
                (mapcat line-up args)
                [[:invoke argc]])))))

;; tags derived from ::constant are added to the constant pool
(derive ::number ::constant)

(derive ::integer ::number)

(derive ::string ::constant)

(derive ::var ::constant)

(derive ::var-resolution ::compiler)

;; (derive ::keyword ::constant)

;; (derive ::symbol ::constant)

(defmethod line-up Integer [form]
  [[::integer form]])

(defmethod line-up String [form]
  [[::string form]])

(defmethod line-up clojure.lang.Keyword [form]
  [[::keyword form]])

(defmethod line-up clojure.lang.Symbol [form]
  [[:name form]])

(defmethod line-up clojure.lang.PersistentHashMap [form]
  (concat (mapcat line-up (apply concat form))
          [[:array (* 2 (count form))]]
          [[:call-method 'clojure.lang.RT/map 1]]))

(defmethod line-up clojure.lang.PersistentVector [form]
  (concat (mapcat line-up form)
          [[:array (count form)]]
          [[:call-method 'clojure.lang.RT/vector 1]]))

;; returns the jvm type for a given tag
(defmulti constant-type identity)

(defmethod constant-type ::integer [_] Integer)

(defmethod constant-type ::string [_] String)

(defmethod constant-type ::keyword [_] clojure.lang.Keyword)

(defmethod constant-type ::var [_] clojure.lang.Var)

;; writes a given value of with a given tag to the gen
(defmulti write-value (fn [x & _] (type x)))

(defmethod write-value String [value gen]
  (.push gen value))

(defmethod write-value Integer [value gen]
  (.push gen value)
  (.invokeStatic gen
                 (Type/getType Integer)
                 (Method/getMethod "Integer valueOf(int)")))

(defmethod write-value ::keyword [_ value gen]
  (.push gen (namespace value))
  (.push gen (name value))
  (.invokeStatic gen
                 (Type/getType clojure.lang.Keyword)
                 (Method/getMethod "clojure.lang.Keyword intern(String,String)")))

(defmethod write-value clojure.lang.Var [value gen]
  (.push gen (name (ns-name (.ns value))))
  (.push gen (name (.sym value)))
  (.invokeStatic gen
                 (Type/getType clojure.lang.RT)
                 (Method/getMethod "clojure.lang.Var var(String,String)")))

(defn define-class [class-name bytes]
  (.defineClass @clojure.lang.Compiler/LOADER
                class-name
                bytes
                nil))

(defn load-class [class-name]
  (.loadClass @clojure.lang.Compiler/LOADER class-name))

(defn resolve-vars [command-seq]
  (loop [[now & later] command-seq stack () out []]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [body]}] now]
                       (recur body (conj stack later out now) []))
          :create-method (let [[_ n arity body] now]
                           (recur body (conj stack later out n arity) []))
          :return (let [[arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later stack (conj out2 [:create-method n arity out])))
          :free (recur later stack (conj out [:var-value (or (resolve (second now))
                                                             (throw
                                                              (dce.Exception.
                                                               "Unable to resolve var"
                                                               ::var-resolution
                                                               {:var-name (second now)
                                                                :namespace *ns*
                                                                :now now
                                                                :later later
                                                                :command-seq command-seq})))]))
          ::var (recur later stack (conj out [::var (or (resolve (second now))
                                                        (throw
                                                         (dce.Exception.
                                                          "Unable to resolve var"
                                                          ::var-resolution
                                                          {:var-name (second now)
                                                           :namespace *ns*
                                                           :now now
                                                           :later later
                                                           :command-seq command-seq})))]))
          (recur later stack (conj out now)))
      (if-not (empty? stack)
        (let [[old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat
                                                   (assoc (apply hash-map (rest old-now))
                                                     :body out))])))
        out))))

(defn pool-constants [command-seq]
  (loop [[now & later] command-seq stack () out []]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [body]}] now]
                       (recur body (conj stack later out now {}) []))
          :create-method (let [[_ n arity body] now
                               [constants & stack] stack]
                           (recur body (conj stack later out n arity constants) []))
          :return (let [[constants arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later (conj stack constants) (conj out2 [:create-method n arity out])))
          (cond
           (isa? (first now) ::constant) (let [[constants & stack] stack
                                               constants (if (contains? constants now)
                                                           constants
                                                           (assoc constants now (count constants)))]
                                           (recur later
                                                  (conj stack constants)
                                                  (conj out [:load-constant (get constants now)
                                                             (constant-type (first now))])))
           :else (recur later stack (conj out now))))
      (if-not (empty? stack)
        (let [[constants old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat
                                                   (assoc (apply hash-map (rest old-now))
                                                     :body out
                                                     :constants constants))])))
        out))))

(defn pool-vars [command-seq]
  (loop [[now & later] command-seq stack () out []]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [body]}] now]
                       (recur body (conj stack later out now #{}) []))
          :create-method (let [[_ n arity body] now
                               [vars & stack] stack]
                           (recur body (conj stack later out n arity vars) []))
          :return (let [[vars arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later (conj stack vars) (conj out2 [:create-method n arity out])))
          :var-value (let [[vars & stack] stack
                           [_ v] now
                           vars (conj vars v)]
                       (recur later
                              (conj stack vars)
                              (conj out now)))
          (recur later stack (conj out now)))
      (if-not (empty? stack)
        (let [[vars old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat
                                                   (assoc (apply hash-map (rest old-now))
                                                     :body out
                                                     :vars vars))])))
        out))))

(defn rewrite-field-access [command-seq]
  (loop [[now & later] command-seq stack () out []]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [body]}] now]
                       (recur body (conj stack later out now) []))
          :create-method (let [[_ n arity body] now]
                           (recur body (conj stack later out n arity) []))
          :return (let [[arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later stack (conj out2 [:create-method n arity out])))
          :var-value (let [[_ v] now
                           var-name (-> (munge (symbol (name (ns-name (.ns v)))
                                                       (name (.sym v))))
                                        str
                                        (.replace "." "_DOT_"))]
                       (recur later
                              stack
                              (conj out
                                    [:get-field var-name clojure.lang.Var]
                                    [:call-method [Object 'deref clojure.lang.IDeref]])))
          :load-constant (let [[_ idx type] now]
                           (recur later
                                  stack
                                  (conj out [:get-field (str "const__" idx) type])))
          (recur later stack (conj out now)))
      (if-not (empty? stack)
        (let [[old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat
                                                   (assoc (apply hash-map (rest old-now))
                                                     :body out))])))
        out))))

(defn explicit-fn-casts [command-seq]
  (loop [[now & later] command-seq stack () out []]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [body]}] now]
                       (recur body (conj stack later out now) []))
          :create-method (let [[_ n arity body] now]
                           (recur body (conj stack later out n arity) []))
          :return (let [[arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later stack (conj out2 [:create-method n arity out])))
          :fn-call (recur later stack (conj out [:cast clojure.lang.IFn]))
          (recur later stack (conj out now)))
      (if-not (empty? stack)
        (let [[old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat
                                                   (assoc (apply hash-map (rest old-now))
                                                     :body out))])))
        out))))

(defn merge-pools [command-seq]
  (loop [[now & later] command-seq stack () out []]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [body constants vars] :as opts}] now
                           fields (into (into {} (for [[[tag value] idx] (sort-by second constants)]
                                                   [(str "const__" idx)
                                                    [value (constant-type tag)]]))
                                        (for [v (sort vars)]
                                          [(-> (munge (symbol (name (ns-name (.ns v)))
                                                              (name (.sym v))))
                                               str
                                               (.replace "." "_DOT_"))
                                           [v clojure.lang.Var]]))
                           opts (-> opts (dissoc :constants :vars) (assoc :fields fields))
                           now `[:define-fn ~@(apply concat opts)]]
                       (recur body (conj stack later out now) []))
          :create-method (let [[_ n arity body] now]
                           (recur body (conj stack later out n arity) []))
          :return (let [[arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later stack (conj out2 [:create-method n arity out])))
          (recur later stack (conj out now)))
      (if-not (empty? stack)
        (let [[old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat
                                                   (assoc (apply hash-map (rest old-now))
                                                     :body out))])))
        out))))


(defn find-closed-over-locals [command-seq]
  (loop [[now & later] command-seq stack () out [] locals [] closed-over #{}]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [body constants vars] :as opts}] now]
                       (recur body (conj stack later out now closed-over) [] locals #{}))
          :create-method (let [[_ n arity body] now]
                           (recur body (conj stack later out n arity) [] locals closed-over))
          :return (let [[arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later stack (conj out2 [:create-method n arity out]) locals
                           closed-over))
          :push-method-frame (let [[_ args] now]
                               (recur later stack (conj out now)
                                      (conj locals (set args)) closed-over))
          :push-frame (let [[_ args] now]
                        (recur later stack (conj out now)
                               (conj locals (set args)) closed-over))
          :pop-frame (let [[_ args] now]
                       (recur later stack (conj out now) (pop locals) closed-over))
          :free (let [[_ a-name] now]
                  (if (some #(contains? % a-name) locals)
                    (recur later stack (conj out [:local a-name])  locals (conj closed-over a-name))
                    (recur later stack (conj out now) locals closed-over)))
          (recur later stack (conj out now) locals closed-over))
      (if-not (empty? stack)
        (let [[old-closed-over old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat
                                                   (assoc (apply hash-map (rest old-now))
                                                     :body out
                                                     :closed-over closed-over))])
                 locals
                 old-closed-over))
        out))))

(defn qualify-fns [command-seq]
  (loop [[now & later] command-seq stack () out []]
    (if now
      (condp = (first now)
          :define-fn (let [[_ & {:keys [name constants body] :as opts}] now
                           [parent-class-name] stack
                           n (if parent-class-name
                               (symbol (str parent-class-name "$" name))
                               (symbol (str (ns-name *ns*) "$" name)))]
                       (recur body (conj stack later out (assoc opts :name n) n) []))
          :create-method (let [[_ n arity body] now
                               [parent-class-name] stack]
                           (recur body (conj stack later out n arity parent-class-name) []))
          :return (let [[pc arity n out2 later & stack] stack
                        out (conj out [:return])]
                    (recur later stack (conj out2 [:create-method n arity out])))
          (recur later stack (conj out now)))
      (if-not (empty? stack)
        (let [[_ old-now out2 later & stack] stack]
          (recur later stack (conj out2 `[:define-fn
                                          ~@(apply concat (assoc old-now :body out))])))
        out))))

;; TODO: deal with primitive args somehow?
;; TODO: handle apply, handle var args, handle everything.
;; TODO: needs to generate a replacement for AFn
;; TODO: find closed over locals, turn into fields
;; TODO: args need to be (arg ...)
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
  (parameter [cg name]))

(defn class-writer [a-name interfaces]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_MAXS)
        interfaces (into-array String (map #(.getName %) interfaces))]
    (.visit cw Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER)
            (name a-name)
            nil
            (.replace
             (.getName clojure.lang.AFn)
             "." "/")
            interfaces)
    cw))

(extend-type ClassWriter
  CodeGenerator
  (new-procedure [class-writer a-name args]
    (let [m (Method. (name a-name) (Type/getType Object)
                     (into-array Type (repeat (count args)
                                              (Type/getType Object))))
          gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil class-writer)
          env (into {} (map vector args (range)))]
      (reify
        CodeGenerator
        (parameter [cg a-name]
          (.loadArg gen (get env a-name)))))))

(defn x []
  [(reify
     CodeGenerator
     (new-procedure-group [cg name interfaces]
       (class-writer name interfaces)))])


(defmn secd
  "executes a primitve secd machine, takes a data stack, environment map
  control stack, and dump stack"
  [?s ?e ((arg ?n) . ?c) ?d]
  (do
    (parameter (first s) n)
    (recur s e c d))
  [(?gen . ?s) ?e ((deftype* . ?body) . ?cs) ?d]
  (let [[tag-name class-name fields _ interfaces & bodies] body
        npg (new-procedure-group gen class-name interfaces)
        dump (cons [cs (cons gen s)] d)
        stack (list npg)
        bodies (for [body bodies]
                 (cons 'proc body))]
    (recur stack e bodies dump))
  [?s ?e ((proc ?name ?args . ?body) . ?c) ?d]
  (recur (list (new-procedure (first s) name args))
         e
         body
         (cons [:proc-group s c] d))
  [?s ?e ((do . ?body) . ?cs) ?d]
  (recur s e (concat body cs) d)
  [?s ?e ?c ?d]
  (prn s e c d))
