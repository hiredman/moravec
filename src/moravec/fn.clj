(ns moravec.fn
  (:refer-clojure :exclude [eval *ns* *source-path*])
  (:use [moravec.types]
        [moravec.vars]
        [moravec.utils :only [constant-type constant-name]])
  (:require [moravec.expr :as expr])
  (:import (clojure.lang LineNumberingPushbackReader RT)
           (clojure.asm.commons GeneratorAdapter Method)
           (clojure.asm Opcodes ClassWriter Type)
           (java.io File)))


(defn class-writer [internal-name super-name]
  (doto (ClassWriter. ClassWriter/COMPUTE_MAXS)
    (.visit Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER)
            internal-name
            nil
            super-name
            nil)))

(defn fn-super-type [fn-desc]
  (Type/getObjectType (.replace (.getName (expr/get-java-class fn-desc)) "." "/")))

(defn fn-type [fn-desc]
  (Type/getObjectType (:fn-internal-name (second fn-desc))))

(defn emit-value [v clinitgen]
  (condp instance? v
    Integer (doto clinitgen
              (.push (.intValue v))
              (.invokeStatic (Type/getType Integer) (Method/getMethod "Integer valueOf(int)")))
    (throw (dce.Exception. "dunno how to emit-value" :compiler {:v v}))))

(defn emit-constants [fn-desc constants clinitgen]
  (binding [*print-dup* true]
    (doseq [[v id] constants]
      (emit-value v clinitgen)
      (doto clinitgen
        (.checkCast (constant-type id constants))
        (.putStatic (fn-type fn-desc) (constant-name id) (constant-type id constants))))))

(defmethod expr/compile 'fn [[_ {:keys [form fn-name fn-internal-name constants line]} methods
                              :as fn-desc]]
  (let [super-class (expr/get-java-class fn-desc)
        cw (class-writer fn-internal-name (Type/getInternalName super-class))
        m (Method. "<init>" Type/VOID_TYPE (into-array Type []))
        gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)
        clinitgen (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC
                                        Opcodes/ACC_STATIC)
                                     (Method/getMethod "void <clinit>()")
                                     nil nil cw)]
    (doto gen
      (.visitCode)
      (.loadThis)
      (.invokeConstructor (fn-super-type fn-desc) (Method/getMethod "void <init>()"))
      (.returnValue)
      (.endMethod))
    (doseq [[v id] constants]
      (.visitField cw (+ Opcodes/ACC_PUBLIC
                         Opcodes/ACC_STATIC
                         Opcodes/ACC_FINAL)
                   (constant-name id)
                   (.getDescriptor (constant-type id constants))
                   nil
                   nil))
    (doto clinitgen
      (.visitCode)
      (.visitLineNumber line (.mark clinitgen)))
    (when constants
      (emit-constants fn-desc constants clinitgen))
    (doto clinitgen
      (.returnValue)
      (.endMethod))
    (doseq [m methods]
      (expr/emit m cw))
    (.visitEnd cw)
    (let [bytecode (.toByteArray cw)
          loader @Compiler/LOADER]
      (clojure.java.io/copy bytecode (clojure.java.io/file "/tmp/foo.class"))
      (.defineClass loader fn-name bytecode form))))

(defmethod expr/get-java-class 'fn [_]
  clojure.lang.AFn)

(defmethod expr/emit 'fn-method [[_ {:keys [tag method constants internal-name]}
                                  arg-locals body :as fn-expr] cw]
  (let [m (Method. "invoke" object-type (into-array Type (repeat (count arg-locals) object-type)))
        gen (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)
             (.visitCode))
        loop-label (.mark gen)]
    (.visitLineNumber gen (:line @method) loop-label)
    (binding [*loop-label* loop-label
              *method* method]
      (expr/emit body :return fn-expr gen)
      (let [end (.mark gen)]
        (.visitLocalVariable gen "this" "Ljava/lang/Object;" nil loop-label end 0)
        (doseq [arg arg-locals]
          (.visitLocalVariable gen (munge (name (:sym arg)))
                               "Ljava/lang/Object;" nil loop-label end 0))))
    (doto gen
      (.returnValue)
      (.endMethod))))
