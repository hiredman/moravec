(ns moravec.test.core
  (:refer-clojure :exclude [eval])
  (:use [moravec.four])
  (:use [clojure.test]))

(deftest identity-check
  (is (= 1 ((eval '(fn* ([x] x))) 1)))
  (is (= 4 ((eval '(fn* kevin ([x] (let [y 2] (+ x y))))) 2)))
  (eval '(def a 1))
  (is (= (eval 'a) 1))
  (is (try
        (eval 'X)
        false
        (catch dce.Exception e
          (and (= :moravec.four/var-resolution (type (.payload e)))
               (= 'X (:var-name (.payload e))))))))
