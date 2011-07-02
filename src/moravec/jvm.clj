(ns moravec.jvm
  (:use [moravec.four])
  (:import (clojure.asm ClassWriter Opcodes Type)
           (clojure.asm.commons Method GeneratorAdapter)))

(def counter (atom 0))

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
     (clojure.java.io/file (format "/tmp/foo%s.class" @counter)))
    (swap! counter inc)
    (.defineClass @clojure.lang.Compiler/LOADER
                  (name a-name)
                  (.toByteArray class-writer)
                  nil))
  (new-procedure-group [cg name interfaces]
    (class-writer name interfaces))
  (write-constructor [cw fields env]
    (prn 'write-constructor)
    (let [m (Method. "<init>" Type/VOID_TYPE
                     (into-array Type (repeat (count fields)
                                              (Type/getType Object))))
          gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil cw)]
      (doto gen
        (.visitCode)
        (.loadThis)
        (.invokeConstructor (Type/getType clojure.lang.AFn)
                            (Method/getMethod "void <init>()")))
      (doseq [[f idx] (map vector fields (range))]
        (.loadThis gen)
        (.loadArg gen idx)
        (.putField gen
                   (Type/getObjectType
                    (.replace (name (second (:this-class env))) "." "/"))
                   (name f)
                   (Type/getType Object)))
      (doto gen
        (.returnValue)
        (.endMethod))
      cw))
  (field [cw the-name]
    (.visitField cw (+ Opcodes/ACC_FINAL) (name the-name) (.getDescriptor (Type/getType Object)) nil nil))
  (new-procedure [this-class-writer a-name args]
    (let [m (Method. (name a-name) (Type/getType Object)
                     (into-array Type (repeat (count args)
                                              (Type/getType Object))))
          gen (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil this-class-writer)]
      (reify
        CodeGenerator
        (finish [cw n] (prn 'finish 1))
        (constructor [cw type-name]
          (.newInstance
           gen (Type/getObjectType (.replace (name type-name) "." "/")))
          (.dup gen)
          (reify
            ConstructorBuilder
            (arguments [_] cw)
            (end-ctor-call [_ argc]
              (.invokeConstructor
               gen (Type/getObjectType (.replace (name type-name) "." "/"))
               (Method. "<init>" Type/VOID_TYPE (into-array Type (repeat argc (Type/getType Object))))))))
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
             (doto gen
               (.loadThis)
               (.getField (Type/getObjectType (.replace (name (second (:this-class env))) "." "/"))
                          (name local-name)
                          (Type/getType Object)))
             :else (throw (Exception. "load arg")))))))))

;; This is kind of a hack for eval
(defn x []
  (let [v (atom nil)]
    [(reify
       clojure.lang.IDeref
       (deref [_] @v)
       CodeGenerator
       (new-procedure-group [cg name interfaces]
         (class-writer name interfaces))
       (constructor [cg class-name]
         (reify
           ConstructorBuilder
           (arguments [_] cg)
           (end-ctor-call [_ argc]
             (reset! v (.newInstance (Class/forName (name class-name))))))))]))
