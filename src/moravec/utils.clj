(ns moravec.utils
  (:use [moravec.vars])
  (:refer-clojure :exclude [eval *ns* *source-path*])
  (:import (clojure.lang Namespace Symbol RT)
           (clojure.asm Type)))

(defn register-var [var]
  (when (bound? #'*vars*)
    (throw (dce.Exception. "not implemented" :compiler {:var var}))))

(defn namespace-for
  ([sym]
     (namespace-for *ns* sym))
  ([ns sym]
     (let [ns-sym (Symbol/intern (namespace sym))
           ns (.lookupAlias ns ns-sym)]
       (if ns
         ns
         (Namespace/find ns-sym)))))

(defn lookup-var [sym intern?]
  (let [var (cond
             (not (nil? (namespace sym))) (when-let [ns (namespace-for sym)]
                                            (let [n (Symbol/intern (name sym))]
                                              (if (and intern? (= ns *ns*))
                                                (.intern *ns* name)
                                                (.findInternedVar ns name))))
             (= sym 'ns) #'clojure.core/ns
             (= sym 'in-ns) #'clojure.core/in-ns
             :else (if-let [o (.getMapping *ns* sym)]
                     (if (var? o)
                       o
                       (throw
                        (RuntimeException.
                         (str "Exceptioning var, but " sym " is mapped to " o))))
                     (when intern?
                       (.intern *ns* (Symbol/intern (name sym))))))]
    (when var
      (register-var var))
    var))

(defn tag-of [o]
  (let [tag (:tag (meta o))]
    (condp instance? tag
      Symbol tag
      String (symbol tag)
      nil)))

(defn constant? [o]
  (cond
   (number? o) true
   :else false))

(defn constant-type [id constants]
  (first
   (for [[v cid] constants
         :when (= id cid)]
     (Type/getType (class v)))))

(defn constant-name [id]
  (str "const__" id))

(defn get-var [sym]
  (let [v (lookup-var sym true)]
    (if-not v
      (throw (dce.Exception. "Can't refer to qualified var that doesn't exist" :compiler
                             {:sym sym}))
      (let [v  (if (not= *ns* (.ns v))
                 (if (nil? v)
                   (.intern *ns* sym)
                   (throw
                    (dce.Exception. "Can't create defs outside of current ns"
                                    :compiler
                                    {:sym sym :ns *ns*})))
                 v)]
        v))))
