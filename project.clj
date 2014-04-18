(defproject clojure.options "0.2.9"
  :description "Advanced support for optional arguments in functions declared with defn-like syntax."
  :url "http://github.com/guv/clojure.options/"
  :license {:name "Eclipse Public License", :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"

  :dependencies [[org.clojure/clojure "1.6.0"]]

  :profiles
    {:dev {:dependencies [[midje "1.5.1"]]}
     :1.2 {:dependencies [[org.clojure/clojure "1.2.1"]]}
     :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
     :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}}

  :aliases {"all" ["with-profile" "dev,1.2:dev,1.3:dev"]
            "cleanbuild" ["do" "clean," "compile" ":all"]}

  :autodoc {:copyright "Copyright 2011-2014 by Gunnar Völkel"}
  :test-paths ["test"]
)
