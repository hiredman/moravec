(ns moravec.core
  (:refer-clojure :exclude [compile])
  (:use [moravec.objx :only [constant-name constant-type]])
  (:import (clojure.lang LineNumberingPushbackReader RT)
           (clojure.asm.commons GeneratorAdapter Method)
           (clojure.asm Opcodes ClassWriter)
           (java.io File)))

(def ^{:private true} eof (Object.))

(def ^{:private true} inits-per 100)

(defn- push-back-reader [rdr]
  (if (instance? LineNumberingPushbackReader rdr)
    rdr
    (LineNumberingPushbackReader. rdr)))

(defn- generator-adapter [cw]
  (GeneratorAdapter. (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                     (Method/getMethod "void load()")
                     nil nil cw))

(defn- generate-constant-fields [objx cw]
  (dotimes [i (count (:constants objx))]
    (.visitField cw (+ Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_STATIC)
                 (constant-name objx i)
                 (.getDescriptor (constant-type i))
                 nil nil)))

(defn- munge-internal-name [source-path]
  (-> (.replace source-path
                File/separator
                "/")
      (.substring 0 (.lastIndexOf source-path \.))
      (str RT/LOADER_SUFFIX)))

(defn- class-writer [internal-name]
  (doto (ClassWriter. ClassWriter/COMPUTE_MAXS)
    (.visit Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER)
            internal-name
            nil
            "java/lang/Object"
            nil)))

(defmacro ^{:private true} with-generator [[name ga] & body]
  `(let [~name ~ga]
     (.visitCode ~name)
     (try
       ~@body
       (finally
        (doto ~name
          (.returnValue)
          (.endMethod))))))

(defn- generate-static-inits [objx cw number-of-inits]
  (doseq [n number-of-inits]
    (with-generator [clinitgen (GeneratorAdapter.
                                (+ Opcodes/ACC_PUBLIC
                                   Opcodes/ACC_STATIC)
                                (Method/getMethod
                                 (str "void __init" n "()"))
                                nil nil cw)]
      (binding [*print-dup* true]
        (loop [i (* n inits-per)]
          (when (and (< i (count (:constants objx)))
                     (< i (* inits-per
                             (inc n))))
            (objx-emit-value objx (nth (:constants objx) i) clinitgen)
            (.checkCast clinitgen (constant-type i))
            (.putStatic clinitgen
                        (:obj-type objx)
                        (constant-name objx i)
                        (constant-type objx i))
            (recur (inc i))))))))

(defn- run-static-inits [objx cw number-of-inits]
  (with-generator [clinitgen (GeneratorAdapter.
                              (+ Opcodes/ACC_PUBLIC
                                 Opcodes/ACC_STATIC)
                              (Method/getMethod "void <clinit> ()")
                              nil nil cw)]
    (let [start-try (.newLabel clinitgen)
          end-try (.newLabel clinitgen)
          end (.newLabel clinitgen)
          finally-label (.newLabel clinitgen)]
      (dotimes [n number-of-inits]
        (.invokeStatic clinitgen
                       (:obj-type objx)
                       (Method/getMethod
                        (str "void __init" n "()"))))
      (doto clinitgen
        (.push (-> objx :internal-name (.replace \/ \.)))
        ;;TODO: class-type
        (.invokeStatic class-type
                       (Method/getMethod "Class forName(String)"))
        (.invokeVirtual class-tye
                        (Method/getMethod "ClassLoader getClassLoader()"))
        ;;TODO: need a class with pushNSandLoader(ClassLoader)
        (.invokeStatic (Type/getType Compiler) (Method/getMethod "void pushNSandLoader(ClassLoader)"))
        (.mark start-try)
        (.invokeStatic (:obj-type objx) (Method/getMethod "void load()"))
        (.mark end-try)
        ;;TODO: var-type
        (.invokeStatic var-type (Method/getMethod "void popThreadBindings()"))
        (.throwException)
        (.mark end)
        (.visitTryCatchBlock start-try end-try finally-label nil)))))

(defn compile [rdr source-path source-name]
  (when-not *compile-path*
    (throw (RuntimeException. "*compile-path* not set")))
  (let [rdr (push-back-reader rdr)]
    (try
      (let [internal-name (munge-internal-name source-path)
            objx (assoc (create-obx nil)
                   :internal-name internal-name
                   :obj-type (Type/getType internal-name))
            cw (class-writer internal-name)
            objx (with-generator [gen (generator-adapter cw)]
                   (->> (repeatedly #(read rdr false eof false))
                        (take-while #(not= eof %))
                        (reduce
                         (fn [objx form]
                           (compile1 gen objx form))
                         objx)))
            number-of-inits (if (zero? (mod (count (:constants objx))
                                            inits-per))
                              (/ (count (:constants objx)) inits-per)
                              (inc (/ (count (:constants objx)) inits-per)))]
        (generate-constant-fields objx cw)
        ;;TODO: should return a list of static inits methods to call
        (generate-static-inits objx cw number-of-inits)
        (run-static-inits objx cw number-of-inits)
        (.visitEnd cw)
        (objx-write-class-file objx (:internal-name objx) (.toByteArray cw)))
      (catch LispReader$ReaderException e
        (throw (CompilerException. source-path (.line e) (.getCause e))))))
  nil)
