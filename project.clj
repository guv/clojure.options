(defproject clojure.options "0.2.2"
  :description "Advanced support for optional arguments in functions declared with defn-like syntax."
  :url "http://github.com/guv/clojure.options/"
  :license {:name "Eclipse Public License", :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.2.1"]]
  :dev-dependencies	[[midje "1.3.1"]]
  :profiles
    {:dev {:dependencies [[midje "1.3.1"]]}}
  :autodoc {:copyright "Copyright 2011-2012 by Gunnar Völkel"}
  :test-paths ["test"]
)
