(ns moravec.three
  (:use [moravec.fn :only [class-writer]])
  (:import (clojure.asm Type Opcodes ClassWriter)
           (clojure.asm.commons Method GeneratorAdapter)))

(def *context*)

(def *constants*)

(defmacro statement [& forms]
  `(binding [*context* :statement]
     ~@forms))

(defmacro return [& forms]
  `(binding [*context* :return]
     ~@forms))

;; expand

(defmulti expand type)

;; expand Number

(defmethod expand Number [form]
  `(if (= *context* :statement)
     `identity
     (load-constant ~form)))

;; expand Seq

;; ;; expand-seq

(defmulti expand-seq first)

;; ;; expand-seq fn*

(defn expand-fn-body [[args & body]]
  `(create-method [:invoke [] Object] ~@(expand (cons 'do body))))

(defmethod expand-seq 'fn* [[_ & bodies]]
  `(create-class :name ~(keyword (name (gensym (ns-name *ns*))))
                 :super clojure.lang.AFn
                 :methods ~(vec (map expand-fn-body bodies))))

(defmethod expand-seq 'do [[_ & forms]]
  (let [l (last forms)
        fs (butlast forms)]
    (concat (map #(list `statement %) (map expand fs))
            [`(return ~(expand l))])))

;; ;;

(defmethod expand clojure.lang.ISeq [form]
  (expand-seq form))


;;

(defn create-method [[method-name args return] & body]
  `(doto ((fn [gen#]
            (let [method# (Method/getMethod ~(format "%s %s (%s)"
                                                     (.getName return)
                                                     (name method-name)
                                                     ""))
                  gen# (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC
                                                method#
                                                nil
                                                nil
                                                gen#)
                         .visitCode)]
              (doto gen#
                ~@body
                .returnValue
                .endMethod))))))

(defn create-class [& {:keys [methods name super]}]
  `(binding [*constants* {}]
     (let [~'internal-name ~(.replace (clojure.core/name name) "." "/")
           cw# (doto (ClassWriter. ClassWriter/COMPUTE_MAXS)
                 (.visit Opcodes/V1_5
                         (+ Opcodes/ACC_PUBLIC
                            Opcodes/ACC_SUPER)
                         ~'internal-name
                         nil
                         ~(.replace (.getName super) "." "/")
                         nil))
           m# (Method. "<init>" Type/VOID_TYPE (into-array Type []))
           gen# (GeneratorAdapter. Opcodes/ACC_PUBLIC m# nil nil cw#)
           clinitgen# (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC
                                            Opcodes/ACC_STATIC)
                                         (Method/getMethod "void <clinit>()")
                                         nil nil cw#)]
       (doto cw# ~@methods)
       (doseq [[c# id#] (sort-by second *constants*)]
         (.visitField cw# (+ Opcodes/ACC_PUBLIC
                             Opcodes/ACC_STATIC
                             Opcodes/ACC_FINAL)
                      (str "const__" id#)
                      (.getDescriptor (Type/getType Integer))
                      nil nil)
         (doto clinitgen#
           (.push c#)
           (.invokeStatic (Type/getType Integer) (Method/getMethod "Integer valueOf(int)"))
           (.putStatic (Type/getObjectType ~'internal-name)
                       (str "const__" id#)
                       (Type/getType Integer))))
       (.returnValue clinitgen#)
       (.visitEnd clinitgen#)
       (doto gen#
         (.visitCode)
         (.loadThis)
         (.invokeConstructor (Type/getObjectType ~(.replace (.getName super) "." "/"))
                             (Method/getMethod "void <init>()"))
         (.returnValue)
         (.endMethod))
       (.visitEnd cw#)
       (.defineClass @clojure.lang.Compiler/LOADER
                     ~(clojure.core/name name)
                     (.toByteArray cw#)
                     nil)
       (clojure.java.io/copy
        (.toByteArray cw#)
        (clojure.java.io/file "/tmp/foo.class"))
       (.newInstance (.loadClass @clojure.lang.Compiler/LOADER
                                 ~(clojure.core/name name))))))

(defn load-constant [form]
  `(doto ((fn [gen#]
            (if-let [id# (get *constants* ~form)]
              (.getStatic gen#
                          (Type/getType (str "L" ~'internal-name ";"))
                          (str "const__" id#)
                          (Type/getObjectType (.replace (.getName Integer) "." "/")))
              (let [id# (count *constants*)]
                (set! *constants* (assoc *constants* ~form id#))
                (.getStatic gen#
                            (Type/getType (str "L" ~'internal-name ";"))
                            (str "const__" id#)
                            (Type/getObjectType (.replace (.getName Integer) "." "/")))))))))
