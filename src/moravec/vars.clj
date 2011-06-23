(ns moravec.vars
  (:refer-clojure :exclude [eval *ns* *source-path*]))

(declare *loader* *vars*)

(def ^{:dynamic true} *ns* (create-ns 'clojure.core))

(def ^{:dynamic true} *source* "NO_SOURCE_FILE")

(def ^{:dynamic true} *source-path* "NO_SOURCE_PATH")

(def ^{:dynamic true} *line* 0)

(def ^{:dynamic true} *env* nil)

(def ^{:dynamic true} *loop-locals* [])

(def ^{:dynamic true} *next-local-num* 0)

(def ^{:dynamic true} *method* nil)

(def ^{:dynamic true} *loop-label* nil)

(def ^{:dynamic true} *in-catch-finally* nil)
