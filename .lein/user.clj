(ns user
  (:require [clojure.pprint :as pprint]
            [clojure.test :as test]
            [vinyasa.inject]
            [spyscope.core]))

(defmacro run-test
  "Runs the test, with the namespace fixtures."
  [test-fn]
  `(let [once-fixture-fn# (test/join-fixtures (:clojure.test/once-fixtures (meta ~*ns*)))
         each-fixture-fn# (test/join-fixtures (:clojure.test/each-fixtures (meta ~*ns*)))]
     (once-fixture-fn#
      #(each-fixture-fn# ~test-fn))))

(defn pprint [& args]
  (doseq [arg args]
    (clojure.pprint/pprint arg)))

(defn spy
  ([m]
   (pprint/pprint m)
   m)
  ([m label]
   (pprint label m)
   m))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (if (= input :exit)
      exit-code
      input)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym))
                 symbols)
            symbols)))

(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read readr
    :eval (partial contextual-eval (local-context))))

(defn inject-repl-utils []
  (vinyasa.inject/in clojure.core >
                     [clojure.repl doc source pst]
                     [clojure.pprint pp]
                     [user pprint]
                     [clojure.tools.namespace.repl refresh refresh-all]
                     [user run-test spy readr local-context break]
                     ))
