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
                        (into-array String))]
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

;; TODO: native call needs to track types
;; type stack? very stack specific, but maybe most vms are

(extend-type ClassWriter
  CodeGenerator
  (native-call [cw target name]
    )
  (finish [class-writer a-name]
    (.visitEnd class-writer)
    (clojure.java.io/copy
     (.toByteArray class-writer)
     (clojure.java.io/file "/tmp/foo1.class"))
    (.defineClass @clojure.lang.Compiler/LOADER
                  (name a-name)
                  (.toByteArray class-writer)
                  nil))
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
  (field [cw the-name]
    (.visitField cw (+ Opcodes/ACC_FINAL) (name the-name) (.getDescriptor (Type/getType Object)) nil nil))
  (arguments [this] this)
  (new-type [this name argc]
    (println 'foo this name argc))
  (new-procedure [this-class-writer a-name args]
    (let [m (Method. (name a-name) (Type/getType Object)
                     (into-array Type (repeat (count args)
                                              (Type/getType Object))))
          gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil this-class-writer)]
      (reify
        CodeGenerator
        (finish [cw n] (prn 'finish 1))
        (arguments [this] this)
        (new-type [this name argc]
          (println 'foo this name argc))
        (new-procedure-group [cg class-name interfaces]
          (println 'new-proc)
          (let [cw (class-writer class-name interfaces)
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
        (end-procedure [_]
          (doto gen
            (.returnValue)
            (.endMethod)))
        (local [cw local-name env]
          (let [[kind place] (get env local-name)]
            (cond
             (= kind :arg)
             (.loadArg gen place)
             (= kind :closed-over)
             (.getField gen
                        (Type/getObjectType (.replace (name (second (:this-class env))) "." "/"))
                        (name local-name)
                        (Type/getType Object))
             :else (throw (Exception. "load arg")))))))))

;; This is kind of a hack for eval
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
       (arguments [this] this)
       (new-type [cw a-name arity]
         (reset! v (.newInstance (Class/forName (name a-name))))))]))
