(ns moravec.jvm
  (:use [moravec.four])
  (:import (clojure.asm ClassWriter Opcodes Type)
           (clojure.asm.commons Method GeneratorAdapter)))

(defn define-class [class-name bytes]
  (.defineClass @clojure.lang.Compiler/LOADER
                class-name
                bytes
                nil))

(defn load-class [class-name]
  (.loadClass @clojure.lang.Compiler/LOADER class-name))

(defn class-writer [a-name interfaces]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_MAXS)
        interfaces (->> interfaces
                        (map #(.getName %))
                        (map #(.replace % "." "/"))
                        (into-array String ))]
    (.visit cw Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER)
            (.replace (name a-name) "." "/")
            nil
            (.replace
             (.getName clojure.lang.AFn)
             "." "/")
            interfaces)
    cw))

(extend-type ClassWriter
  CodeGenerator
  (finish [class-writer a-name]
    (.visitEnd class-writer)
    (.defineClass @clojure.lang.Compiler/LOADER
                  (name a-name)
                  (.toByteArray class-writer)
                  nil))
  (new-procedure [class-writer a-name args]
    (let [m (Method. (name a-name) (Type/getType Object)
                     (into-array Type (repeat (count args)
                                              (Type/getType Object))))
          gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil class-writer)
          env (into {} (map vector args (range)))]
      (reify
        CodeGenerator
        (end-procedure [_]
          (doto gen
            (.returnValue)
            (.endMethod)))
        (parameter [cg a-name]
          (.loadArg gen (get env a-name)))))))

(defn x []
  (let [v (atom nil)]
    [(reify
       clojure.lang.IDeref
       (deref [_] @v)
       CodeGenerator
       (new-procedure-group [cg name interfaces]
         (let [cw (class-writer name interfaces)
               m (Method. "<init>" Type/VOID_TYPE (into-array Type []))
               gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)]
           ;;TODO: ctor needs to take into account closed over fields
           (doto gen
             (.visitCode)
             (.loadThis)
             (.invokeConstructor (Type/getType clojure.lang.AFn)
                                 (Method/getMethod "void <init>()"))
             (.returnValue)
             (.endMethod))
           cw))
       (new-type [cw a-name arity]
         (reset! v (.newInstance (Class/forName (name a-name))))))]))
