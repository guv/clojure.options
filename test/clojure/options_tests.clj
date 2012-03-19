; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns clojure.options-tests
  (:use clojure.options midje.sweet)
)

; let's do some non-standard testing (is there support for testing in temporary namespaces?)
; more useful examples will be added to my incanter fork

(defn+opts increase 
  [x | {delta 1}] 
  (+ x delta)) 

(facts "'increase meta"
  (resolve 'increase) => truthy
  (meta #'increase) => (contains {:clojure.options/defn+opts true, :options anything, :mandatory-parameters ['x]})
  (-> #'increase meta :options) => (just {:mine {'delta {:name 'delta, :default 1}}}))

(facts "'increase default value"
  (increase 1) => 2
  (increase 2) => 3)

(tabular
  (fact "'increase optional parameter"
    (increase ?x :delta ?delta) => ?y)
  ?x ?delta ?y
   1   0     1
   2  -1     1
  10   5    15)

(facts "apply call to 'increase"
  (apply increase 1 :delta 2 []) => 3
  (apply increase 5 [:delta 3]) => 8)

(fact "'increase called wrong"
   (increase 1 2) => (throws IllegalArgumentException))



(defn+opts f
  [y | :as options]
  (increase (inc y) options))

(facts "'f meta with data from 'increase"
  (resolve 'f) => truthy
  (meta #'f) => (contains {:clojure.options/defn+opts true, :options anything, :mandatory-parameters ['y]})
  (-> #'f meta :options) => (contains {:mine (just {})})
  (-> #'f meta :options) => (contains {'clojure.options-tests/increase (just {'delta {:name 'delta, :default 1}})})
  (-> #'f meta :doc) => (and #"clojure.options-tests/increase" #":delta"))

(facts "calls to 'f"
   (f 1) => 3
   (f 5) => 7)

(tabular
  (fact "'f optional parameter"
    (f ?x :delta ?delta) => ?y)
  ?x ?delta ?y
   1   0     2
   2  -1     2
  10   5    16)


(defn+opts threading-test
  [x, y | :as options]
  [x (-> x (increase options) (+ y))])

(facts "'threading-test meta with data from 'increase"
  (resolve 'threading-test) => truthy
  (meta #'threading-test) => (contains {:clojure.options/defn+opts true, :options anything, :mandatory-parameters ['x 'y]})
  (-> #'threading-test meta :options) => (contains {:mine (just {})})
  (-> #'threading-test meta :options) => (contains {'clojure.options-tests/increase (just {'delta {:name 'delta, :default 1}})})
  (-> #'threading-test meta :doc) => (and #"clojure.options-tests/increase" #":delta"))

(tabular
  (fact "'threading-test optional parameter"
    (threading-test ?x ?y :delta ?delta) => ?r)
  ?x  ?y ?delta ?r
   1   2    2   [1 5]
   2  -3    2   [2 1]
  10   0   16   [10 26])


(defn+opts g
  [a | [call-f] :as opts]
  (if call-f (f a opts) a))

(facts "'g meta"
  (resolve 'g) => truthy
  (meta #'g) => (contains {:clojure.options/defn+opts true, :options anything, :mandatory-parameters ['a]})
  (-> #'g meta :options) => (contains {:mine (just {'call-f {:name 'call-f}})})
  (-> #'g meta :options) => (contains {'clojure.options-tests/increase (just {'delta {:name 'delta, :default 1}})})
  (-> #'g meta :doc) => (and #"clojure.options-tests/increase" #":delta" #":call-f"))

(facts "call 'g"
  (g 10) => 10     
  (g 10 :delta 5) => 10
  (g 10 :call-f true) => 12
  (g 10 :call-f true :delta 5) => 16)


(defn+opts int->str
  "Converts a given positive integer into a string.
  <base>The base to use for string encoding (<= 10).</base>
  "
  [x | {base 10}]
  (loop [x x, digits (list)]
    (if (pos? x)
      (recur (quot x base) (conj digits (mod x base)))
      (apply str digits))))

(facts "'int->str meta"
  (resolve 'int->str) => truthy
  (meta #'int->str) => (contains {:clojure.options/defn+opts true, :options anything, :mandatory-parameters ['x]})
  (-> #'int->str meta :options) => (contains {:mine (just {'base {:name 'base, :default 10, :doc "The base to use for string encoding (<= 10)."}})})
  (-> #'int->str meta :doc) => (and #"Converts a given positive integer into a string\." #":base" #"The base to use for string encoding \(<= 10\)\.")
  (-> #'int->str meta :doc) =not=> (or #"<base>" #"</base>"))

(facts "'call 'int->str"
  (int->str 13) => "13"
  (int->str 13 :base 2) => "1101"
  (int->str 13 :base 3) => "111"
  (int->str 13 :base 9) => "14")

(require 'clojure.string)

(defn+opts convert-ints
  "Converts all integers in a string to another base."
  [s | :as options]
  (clojure.string/replace s #"\d+" #(-> % Integer/parseInt (int->str options))))

(defn+opts do-sth 
  [x | {mode (choice :equal :dec :inc)}]
  (case mode
    :dec (dec x)
    :equal x
    :inc (inc x)
    nil))

(facts "'do-sth meta"
  (resolve 'do-sth) => truthy
  (meta #'do-sth) => (contains {:clojure.options/defn+opts true, :options anything, :mandatory-parameters ['x]})
  (-> #'do-sth meta :options) => (contains {:mine (just {'mode {:name 'mode, :default :equal, :alternatives [:dec :inc]}})}))

(facts "call 'do-sth"
  (do-sth 9) => 9
  (do-sth 9 :mode :equal) => 9
  (do-sth 9 :mode :dec) => 8
  (do-sth 9 :mode :inc) => 10
  (do-sth 9 :mode "error") => (throws IllegalArgumentException))


(facts "only one body allowed"
   (eval '(defn+opts error ([x] x) ([x y] (+ x y)))) => throws
   (eval '(defn+opts error ([x] x))) => var?)