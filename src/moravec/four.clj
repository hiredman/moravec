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

;; re-write as trampolining multimethod
;; state machine for interpreting abstract clojure opcodes
(defn code-gen [command-seq]
  (loop [[now & later] command-seq stack ()]
    (condp = (first now)
        :define-fn (let [[_ & {:keys [name fields body closed-over]}] now
                         later (rest later)]
                     (println "define-fn" name)
                     (recur (concat [[:create-class-writer]
                                     [:create-static-init]
                                     [:write-static-fields]
                                     [:end-static-init]
                                     [:write-instance-fields]
                                     [:write-constructor]]
                                    body
                                    [[:close-class-writer]
                                     [:vivify-fun name closed-over]]
                                    later)
                            (conj stack
                                  {:class-name name
                                   :fields fields
                                   :closed-over closed-over
                                   :vars (->> (mapcat (comp (partial map resolve)
                                                            (partial map second)
                                                            (partial filter #(= (first %) :free))
                                                            #(nth % 3))
                                                      body)
                                              (reduce
                                               (fn [accum v]
                                                 (if (contains? accum v)
                                                   accum
                                                   (assoc accum v (count accum))))
                                               {}))})))
        :create-class-writer (let [[{:keys [class-name] :as state} & states] stack]
                               (let [internal-name (.replace (name class-name) "." "/")
                                     cw (doto (ClassWriter. ClassWriter/COMPUTE_MAXS)
                                          (.visit Opcodes/V1_5
                                                  (+ Opcodes/ACC_PUBLIC
                                                     Opcodes/ACC_SUPER)
                                                  internal-name
                                                  nil
                                                  (.replace (.getName clojure.lang.AFn)
                                                            "." "/")
                                                  nil))]
                                 (recur later (conj states (assoc state
                                                             :class-writer cw
                                                             :internal-name internal-name)))))
        :create-static-init (let [[{:keys [class-writer] :as state} & states] stack]
                              (let [clinitgen (GeneratorAdapter.
                                               (+ Opcodes/ACC_PUBLIC
                                                  Opcodes/ACC_STATIC)
                                               (Method/getMethod "void <clinit>()")
                                               nil nil class-writer)]
                                (recur later (conj states (assoc state :clinitgen clinitgen)))))
        :write-static-fields (let [[{:keys [fields class-writer constants internal-name clinitgen]}] stack]
                               (doseq [[field-name [value type]] (concat (->> fields
                                                                              (filter #(.startsWith
                                                                                        (first %) "const__"))
                                                                              (sort-by first))
                                                                         (->> fields
                                                                              (remove #(.startsWith
                                                                                        (first %) "const__"))
                                                                              (sort-by first)))]
                                 (.visitField class-writer (+ Opcodes/ACC_PUBLIC
                                                              Opcodes/ACC_STATIC
                                                              Opcodes/ACC_FINAL)
                                              field-name
                                              (.getDescriptor (Type/getType type))
                                              nil nil)
                                 (write-value value clinitgen)
                                 (.putStatic clinitgen
                                             (Type/getObjectType internal-name)
                                             field-name
                                             (Type/getType type)))
                               (recur later stack))
        :write-instance-fields (let [[{:keys [closed-over class-writer internal-name]}] stack]
                                 (doseq [field-name closed-over]
                                   (.visitField class-writer (+ Opcodes/ACC_PUBLIC
                                                                Opcodes/ACC_FINAL)
                                                (name field-name)
                                                (.getDescriptor (Type/getType Object))
                                                nil nil))
                                 (recur later stack))
        :cast (let [[{:keys [gen]}] stack
                    [_ type] now]
                (.checkCast gen (Type/getType type))
                (recur later stack))
        :invoke (let [[{:keys [gen]}] stack
                      [_ arity] now]
                  (.invokeInterface gen
                                    (Type/getType clojure.lang.IFn)
                                    (Method. "invoke"
                                             (Type/getType Object)
                                             (into-array Type
                                                         (repeat arity (Type/getType Object)))))
                  (recur later stack))
        :end-static-init (let [[{:keys [clinitgen] :as state} & states] stack]
                           (.returnValue clinitgen)
                           (.endMethod clinitgen)
                           (recur later (conj states (dissoc state :clinitgen))))
        :write-constructor (let [[{:keys [class-writer closed-over internal-name]}] stack
                                 m (Method. "<init>" Type/VOID_TYPE (into-array Type []))]
                             (println "write-constr" internal-name)
                             (if (empty? closed-over)
                               (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil class-writer)
                                 (.visitCode)
                                 (.loadThis)
                                 (.invokeConstructor (Type/getType clojure.lang.AFn) m)
                                 (.returnValue)
                                 (.endMethod))
                               (let [m2 (Method. "<init>" Type/VOID_TYPE
                                                 (into-array Type (repeat (count closed-over)
                                                                          (Type/getType Object))))
                                     gen (GeneratorAdapter.
                                          Opcodes/ACC_PUBLIC m2 nil nil class-writer)]
                                 (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil class-writer)
                                   (.visitCode)
                                   (.loadThis)
                                   (.invokeConstructor (Type/getType clojure.lang.AFn) m)
                                   (.returnValue)
                                   (.endMethod))
                                 (doto gen
                                   (.visitCode)
                                   (.loadThis)
                                   (.invokeConstructor (Type/getObjectType internal-name) m))
                                 (doall
                                  (keep-indexed
                                   (fn [idx the-name]
                                     (.loadArg gen idx)
                                     (.putField gen
                                                (Type/getObjectType internal-name)
                                                (name the-name)
                                                (Type/getType Object)))
                                   (sort closed-over)))
                                 (doto gen
                                   (.returnValue)
                                   (.endMethod))))
                             (recur later stack))
        :create-method (let [[{:keys [class-writer] :as state} & states] stack
                             [_ method-name arity body] now
                             m (Method. "invoke" (Type/getType Object)
                                        (into-array Type
                                                    (repeat arity (Type/getType Object))))
                             gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil class-writer)]
                         (recur (concat body later)
                                (conj states (assoc state :gen gen))))
        :push-frame (let [[{:keys [gen internal-name] :as state} & states] stack
                          [_ frame] now]
                      (if (empty? frame)
                        (recur later stack)
                        (let [locals (zipmap
                                      (sort frame)
                                      (map
                                       (fn [_]
                                         (.newLocal gen (Type/getType Object))) (sort frame)))]
                          (recur later (conj stack (update-in state [:locals] merge locals))))))
        :push-method-frame (let [[state & states] stack
                                 [_ frame] now
                                 args (zipmap frame (range (count frame)))]
                             (if (empty? frame)
                               (recur later stack)
                               (recur later (conj stack (assoc state :args args)))))
        :bind (let [[{:keys [gen locals] :as state} & states] stack
                    [_ local-name] now]
                (.storeLocal gen (get locals local-name))
                (recur later stack))
        :local (let [[{:keys [gen locals args closed-over internal-name] :as state} & states] stack
                     [_ local-name] now
                     tag (resolve (:tag (meta ((set (keys locals)) local-name)) 'Object))]
                 (if (contains? locals local-name)
                   (.loadLocal gen (get locals local-name))
                   (if (contains? args local-name)
                     (.loadArg gen (get args local-name))
                     (.getField gen
                                (Type/getObjectType internal-name)
                                (name local-name)
                                (Type/getType Object))))
                 (when tag
                   (.checkCast gen (Type/getType tag)))
                 (recur later stack))
        :this-fn (let [[{:keys [gen]}] stack]
                   (.loadThis gen)
                   (recur later stack))
        :pop-frame (let [[{:keys [gen internal-name locals args] :as state} & states] stack
                         [_ frame] now]
                     (if (empty? frame)
                       (recur later stack)
                       (do
                         (doseq [local-name frame]
                           (.visitInsn gen Opcodes/ACONST_NULL)
                           (if (contains? locals local-name)
                             (.storeLocal gen (get locals local-name))
                             (.storeArg gen (get args local-name))))
                         (recur later states))))
        :get-field (let [[{:keys [gen internal-name] :as state} & states] stack
                         [_ field-name field-type] now]
                     (.getStatic gen
                                 (Type/getObjectType internal-name)
                                 field-name
                                 (Type/getType field-type))
                     (recur later stack))
        ::keyword (let [[{:keys [gen]}] stack
                        [_ k] now]
                    (if (nil? (namespace k))
                      (.visitInsn gen Opcodes/ACONST_NULL)
                      (.push gen (namespace k)))
                    (doto gen
                      (.push (name k))
                      (.invokeStatic (Type/getType clojure.lang.Keyword)
                                     (Method/getMethod
                                      "clojure.lang.Keyword intern(String,String)")))
                    (recur later stack))
        ::symbol (let [[{:keys [gen]}] stack
                       [_ k] now]
                   (if (nil? (namespace k))
                     (.visitInsn gen Opcodes/ACONST_NULL)
                     (.push gen (namespace k)))
                   (doto gen
                     (.push (name k))
                     (.invokeStatic (Type/getType clojure.lang.Symbol)
                                    (Method/getMethod
                                     "clojure.lang.Symbol intern(String,String)")))
                   (recur later stack))
        :return (let [[{:keys [gen class-name internal-name] :as state} & states] stack]
                  (doto gen
                    (.returnValue)
                    (.endMethod))
                  (recur later (conj states (dissoc state :gen))))
        :bind-var (let [[{:keys [gen]}] stack
                        [_ var-name] now
                        var-name (name var-name)
                        var-ns-name (name (ns-name *ns*))]
                    (doto gen
                      (.push var-ns-name)
                      (.swap)
                      (.push var-name)
                      (.swap)
                      (.invokeStatic (Type/getType clojure.lang.RT)
                                     (Method/getMethod
                                      "clojure.lang.Var var(String,String,Object)")))
                    (recur later stack))
        :call-method (let [[{:keys [gen]}] stack
                           [_ [return method-name target & args]] now]
                       (doto gen
                         (.invokeInterface (Type/getType target)
                                           (Method/getMethod
                                            (format "%s %s()"
                                                    (.getName return)
                                                    method-name))))
                       (recur later stack))
        :close-class-writer (let [[{:keys [class-writer class-name]}] stack]
                              (.visitEnd class-writer)
                              (let [f (java.io.File.
                                       (str "/tmp/foo/"
                                            (.replace (name class-name) "." "/")
                                            ".class"))]
                                (.mkdirs (.getParentFile f))
                                (clojure.java.io/copy
                                 (.toByteArray class-writer)
                                 f))
                              (define-class (name class-name) (.toByteArray class-writer))
                              (recur later (pop stack)))
        ::vivify-fun (let [[{:keys [gen internal-name] :as state} & states] stack
                           [_ class-name closed-over] now]
                       (.invokeConstructor gen (Type/getObjectType
                                                (.replace (name class-name) "." "/"))
                                           (Method. "<init>" Type/VOID_TYPE
                                                    (into-array Type (repeat (count closed-over)
                                                                             (Type/getType Object)))))
                       (recur later stack))
        :vivify-fun (let [[{:keys [gen internal-name] :as state} & states] stack
                          [_ class-name closed-over] now]
                      (if gen
                        (do
                          (doto gen
                            (.newInstance (Type/getObjectType (.replace (name class-name) "." "/")))
                            (.dup))
                          (if (empty? closed-over)
                            (do
                              (.invokeConstructor gen (Type/getObjectType
                                                       (.replace (name class-name) "." "/"))
                                                  (Method/getMethod "void <init>()"))
                              (recur later stack))
                            (recur (concat (map #(vector :local %)
                                                (sort closed-over))
                                           [[::vivify-fun class-name closed-over]]
                                           later)
                                   stack)))
                        (recur later (conj stack (.newInstance (load-class (name class-name)))))))
        :halt [now later stack]
        (do
          (println "stack")
          (clojure.pprint/pprint stack)
          (println "later")
          (clojure.pprint/pprint later)
          (println "now")
          (clojure.pprint/pprint now)
          (throw
           (Exception.
            "unknown op"))))))

(defn eval [form]
  (let [x (->> (line-up `(fn* ([] ~form)))
               find-closed-over-locals
               resolve-vars
               qualify-fns
               pool-constants
               pool-vars
               rewrite-field-access
               merge-pools
               explicit-fn-casts
               identity)]
    (clojure.pprint/pprint x)
    (.invoke (first (last (code-gen (conj x [:halt])))))))

(defprotocol Value
  (closure? [v]))

(defprotocol Closure
  (get-args [c])
  (get-env [c])
  (get-body [c]))

(defprotocol Machine
  (closure [m env args body]))

(defprotocol Environment
  (lookup-in [env name])
  (extend-with [env name value]))

(defn secd-dispatch [stack env control dump]
  (cond
   (every? empty? [(rest stack) control dump])
   :return-value
   (and (every? empty? [(rest stack) control])
        (first stack)
        (= (count (first dump)) 3))
   :continue-from-dump
   (= 'fn (first (first control)))
   :push-fn
   (let [[y [x _]] stack
         [z] control]
     (and (= x :int)
          (= y :succ)
          (= z :apply)))
   :inc
   (let [[c] stack
         [z] control]
     (and c
          (closure? c)
          (= z :apply)))
   :run-closure
   (let [[f [s _]] (first control)]
     (and (= f :term)
          (= s :lit)))
   :push-literal-int
   (let [[f [s _]] (first control)]
     (and (= f :term)
          (= s :var)))
   :look-up-name
   (let [[f [s _]] (first control)]
     (and (= f :term)
          (= s :lam)))
   :push-closure
   (let [[f [s _]] (first control)]
     (and (= f :term)
          (= s :app)))
   :application))

(defn m []
  (reify
    Machine
    (closure [m env args body]
      (reify
        Value
        (closure? [_] true)
        Closure
        (get-args [_] args)
        (get-env [_] env)
        (get-body [_] body)))))

(extend-type clojure.lang.IPersistentVector
  Value
  (closure? [v] (= :closure (first v))))

(extend-type clojure.lang.IPersistentMap
  Environment
  (lookup-in [env a-name]
    (get env a-name (resolve a-name)))
  (extend-with [env a-name value]
    (assoc env a-name value)))

(defn secd [machine stack env control dump]
  (println "secd" stack env control dump)
  ;; make everything a list and just dispatch on (first ...) ? dunno
  (condp = (secd-dispatch stack env control dump)
      :return-value (first stack)
      :continue-from-dump (let [[[s e c] & dump] dump
                                stack (conj s (first stack))
                                env e
                                control c]
                            (recur machine stack env control dump))
      :push-literal-int (let [[[_ [_ n]] & control] control
                              stack (conj stack [:int n])]
                          (recur machine stack env control dump))
      :look-up-name (let [[[_ [_ x]] & control] control
                          stack (conj stack (lookup-in env x))]
                      (recur machine stack env control dump))
      :push-closure (let [[[_ [_ [x t]]] & control] control
                          stack (conj stack (closure machine env x t))]
                      (recur machine stack env control dump))
      :push-fn (let [[[_ opts] & control] control
                     stack (conj stack (closure machine env nil opts))]
                 ;;TODO: tag env as being closed over
                 ;; need to run code generation for closure body here
                 ;; possibly do something just like run-closure here
                 ;; to generate code
                 ;;
                 ;; so create class writer here
                 ;; create method writer
                 ;; class-writer creation will need to happen in the
                 ;; Machine
                 ;; - machine state may need to be saved to the dump
                 ;; may need access to both, so I can lazily write
                 ;; fields, etc
                 ;; so the class-writer and method-writer go on the
                 ;; stack somehow
                 ;; continuation goes on the dump
                 ;; the continuation is the current state + the rest
                 ;; of the procs
                 ;; the first proc goes on control
                 ;; extend env with args? maybe push-method-frame can
                 ;; take care of that
                 ;; maybe "stack" can be replaced completely by the machine
                 (recur machine stack env control dump))
      :application (let [[[_ [_ [t0 t1]]] & control] control
                         control (conj control :apply [:term t0] [:term t1])]
                     (recur machine stack env control dump))
      :inc (let [[_ [_ n] & stack] stack
                 [_ & control] control
                 stack (conj stack [:int (inc n)])]
             (recur machine stack env control dump))
      :run-closure (let [[c v & stack] stack
                         e (get-env c)
                         x (get-args c)
                         t (get-body c)
                         [_ & control] control
                         dump (conj dump [stack env control])
                         env (extend-with e x v)
                         control (list [:term t])]
                     (recur machine stack env control dump))))

(defmn secd
  "executes a primitve secd machine, takes a data stack, environment map
  control stack, and dump stack"
  [(?v . nil) ?e' () ()]
  v

  [(?v . nil) ?e' () ([?s ?e ?c] . ?d)]
  (recur (cons v s) e c d)

  [?s ?e ([:term [:lit ?n]] . ?c) ?d]
  (recur (cons [:int n] s) e c d)

  [?s ?e ([:term [:var ?x]] . ?c) ?d]
  (recur (cons (get e x) s) e c d)

  [?s ?e ([:term [:lam [?x ?t]]] . ?c) ?d]
  (recur (cons [:closure [e x t]] s) e c d)

  [?s ?e ([:term [:app [?t0 ?t1]]] . ?c) ?d]
  (recur s e (list* [:term t1] [:term t0] :apply c) d)

  [(:succ [:int ?n] . ?s) ?e (:apply . ?c) ?d]
  (recur (cons [:int (inc n)] s) e c d)

  [([:closure [?e' ?x ?t]] ?v . ?s) ?e (:apply . ?c) ?d]
  (recur () (assoc e' x v) (list [:term t]) (cons [s e c] d))

  [() ?e ((fn ?args . nil) . ?c) ?d]
  (let [{:keys [name procs]} args
        frame {:name name
               :class-writer 'foo}
        procs (for [[signature code] procs]
                [:create-method signature code])]
    ;;pull fn off control stack, put fn writer on value stack,
    (recur (list*  frame current s)
           e
           (concat procs c)
           d)))

(comment

  [:fun-spec
   {:name "..."
    :constants #{}
    :closed-over #{}
    :procedures {[signature] body}}]

  )
