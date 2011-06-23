(ns moravec.objx
  (:use [moravec.types :only [object-type]])
  (:refer-clojure :exclude [compile])
  (:import (clojure.lang LineNumberingPushbackReader ILookupSite ILookupThunk KeywordLookupSite)
           (java.lang.reflect Modifier)
           (clojure.asm.commons GeneratorAdapter Method)
           (clojure.asm Opcodes Type ClassWriter)))

(def constant-prefix "const__")

(def void-ctor (Method/getMethod "void <init>()"))

(def keyword-intern
  (Method/getMethod "clojure.lang.Keyword intern(String, String)"))

(def symbol-intern
  (Method/getMethod "clojure.lang.Symbol intern(String)"))

(def var-intern
  (Method/getMethod
   "clojure.lang.Var intern(clojure.lang.Symbol, clojure.lang.Symbol)"))

(def dynamic-classloader-type (Type/getType clojure.lang.DynamicClassLoader))

(def get-class-method
  (Method/getMethod "Class getClass()"))

(def get-classloader-method
  (Method/getMethod "ClassLoader getClassLoader()"))

(def get-constants-method
  (Method/getMethod "Object[] getConstants(int)"))

(def read-string-method
  (Method/getMethod "Object readString(String)"))

(def ilookup-site-type (Type/getType ILookupSite))

(def ilookup-thunk-type (Type/getType ILookupThunk))

(def keyword-lookupsite-type (Type/getType KeywordLookupSite))

(def proto-objx
  {:name nil ;String
   :internal-name nil ;String
   :this-name nil ;String
   :obj-type nil ;Type
   :tag nil ;Object
   :closes {}
   :closes-exprs []
   :volatiles #{}
   :fields nil ; {}
   :keywords {}
   :vars {}
   :compiled-class nil ;Class
   :line 0;int
   :constants nil; []
   :constants-id 0;int
   :alt-ctor-drops 0;int
   :keyword-callsites nil;[]
   :protocol-callsites nil; []
   :var-callsites nil;#{}
   :once-only false;boolean
   :src nil;Object
   :class-meta nil;{}
   :static? false;boolean
   :loader nil;DynamicClassLoader
   :bytecode nil;byte[]
   })

(defn create-objx [tag]
  (assoc proto-objx :tag tag))

(defn constant-name [objx i]
  (str constant-prefix i))


(defn constant-type [{:keys [constants]} id]
  (let [o (nth constants id)
        c (clojure.lang.Util/classOf o)]
    (if (and c (Modifier/isPublic (.getModifiers c)))
      ;;NOTE: ordering of cases is slightly different here
      (condp #(.isAssignableFrom % %2) c
        clojure.lang.LazySeq
        (Type/getType clojure.lang.ISeq)
        clojure.lang.RestFn
        (Type/getType clojure.lang.RestFn)
        clojure.lang.AFn
        (Type/getType clojure.lang.AFn)
        (condp = c
            clojure.lang.Keyword
          (Type/getType clojure.lang.Keyword)
          clojure.lang.Var
          (Type/getType clojure.lang.Var)
          String
          (Type/getType String)
          object-type))
      object-type)))

;;should be a multimethod
(defn emit-value [objx value gen]
  (let [partial? true]
    (if (nil? value)
      (.visitInsn gen Opcodes/ACONST_NULL)
      (condp instance? value
        String (.push gen value)
        Boolean (if (.booleanValue value)
                  (.getStatic gen boolean-object-type "TRUE" boolean-object-type)
                  (.getStatic gen boolean-object-type "FALSE" boolean-object-type))
        Integer (doto gen
                  (.push (.intValue value))
                  (.invokeStatic (Type/getType Integer) (Method/getMethod "Integer valueOf(int)")))
        Long (doto gen
               (.push (.longValue value))
               (.invokeStatic (Type/getType Long) (Method/getMethod "Long valueOf(long)")))
        Double (doto gen
                 (.push (.doubleValue value))
                 (.invokeStatic (Type/getType Double) (Method/getMethod "Double valueOf(double)")))
        Character (doto gen
                    (.push (.charValue value))
                    (.invokeStatic (Type/getType Character) (Method/getMethod "Character valueOf(char)")))
        Class (if (.isPrimitive value)
                (.getStatic gen
                            (condp = value
                                Boolean/TYPE (Type/getType Boolean)
                                Byte/TYPE (Type/getType Byte)
                                Character/TYPE (Type/getType Character)
                                Double/TYPE (Type/getType Double)
                                Float/TYPE (Type/getType Float)
                                Integer/Type (Type/getType Integer)
                                Long/Type (Type/getType Long)
                                Short/Type (Type/getType Short)
                                (throw
                                 (RuntimeException. (str "Can't embed unknown primitive in code: " value))))
                            "TYPE"
                            (Type/getType Class))
                (doto gen
                  (.push (destub-class-name (.getName value)))
                  (.invokeStatic (Type/getType Class)
                                 (Method/getMethod "Class forName(String)"))))
        clojure.lang.Symbol (doto gen
                              (.push (namespace value))
                              (.push (name value))
                              (.invokeStatic (Type/getType clojure.lang.Symbol)
                                             (Method/getMethod "clojure.lang.Symbol intern(String,String)")))
        clojure.lang.Keyword (doto gen
                               (.push (namespace value))
                               (.push (name value))
                               (.invokeStatic rt-type (Method/getMethod "clojure.lang.Keyword keyword(String, String)")))
        clojure.lang.Var (doto gen
                           (.push (name (.ns value)))
                           (.push (name (.sym value)))
                           (.invokeStatic rt-type (Method/getMethod "clojure.lang.Var var(String,String)")))
        clojure.lang.IPersistentMap (let [entries (ArrayList.)]
                                      (doseq [[k v] value]
                                        (.add entries k)
                                        (.add entries v))
                                      (emit-list-as-object objx entries gen)
                                      (.invokeStatic gen rt-type (Method/getMethod "clojure.lang.IPersistentMap map(Object[])")))
        clojure.lang.IPersistentVector (do
                                         (emit-list-as-object objx value gen)
                                         (.invokeStatic gen rt-type (Method/getMethod "clojure.lang.IPersistentVector vector(Object[])")))
        clojure.lang.PersistentHashSet ))))

(defn compile [objx super-name interface-names one-time-use?]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_MAXS)
        source (:source objx)
        line-before (.deref Compiler/LINE_BEFORE)
        line-after (.deref Compiler/LINE_AFTER)]
    (.visit cw Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER
               Opcodes/ACC_FINAL)
            (:internal-name objx)
            nil
            super-name
            interface-names)
    ))


