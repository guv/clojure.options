; Copyright (c) Gunnar Völkel. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v1.0.txt at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns clojure.options
  "Implementation of improved option support for functions.

  For example the following definition declares a function with the optional parameter 'delta that defaults to 1:
  (defn+opts f [x | {delta 1}]
    (+ x delta))
  
  (f 10)
  ;=> 11
  (f 10 :delta 5)
  ;=> 15
  "
  {:author "Gunnar Völkel"}
  (:require [clojure.string :as string])
  (:use [clojure.set :only (rename-keys)]))


(defn ^{:skip-wiki true} process-defn-decl
  "Processes the function declaration like it is done in clojure.core/defn.
  Returns the map {:meta-map metadata, :body-list func-body} with the meta data of the function
  and the list of function bodies.
  "
  [func-decl]
  (let [
			  ; extract doc string (if present) and create metadata map
			  meta-map (if (string? (first func-decl)) 
			             {:doc (first func-decl)} 
			             {}),
			  ; remove doc string if present
			  func-decl (if (string? (first func-decl))
			              (rest func-decl)
			              func-decl),
			  ; add given metadata (if present) to map 
			  meta-map (if (map? (first func-decl))
			             (merge meta-map (first func-decl))
			             meta-map),
			  ; remove metadata if present
			  func-decl (if (map? (first func-decl))
			              (rest func-decl)
			              func-decl),
			  ; if only a single function body then put it into a list
			  func-decl (if (vector? (first func-decl))            
			                   (list func-decl)          
			                   func-decl)	    
			  ; add given metadata at the end (if present) to map 
			  meta-map (if (map? (last func-decl))
			             (merge meta-map (last func-decl))
			             meta-map),
			  ; remove metadata at the end if present
			  func-decl (if (map? (last func-decl))
			              (butlast func-decl)
			              func-decl)
       ]
     {:meta-map meta-map, :body-list func-decl}))



; borrowed from clojure.core
(defmacro ^{:skip-wiki true} assert-args [fnname & pairs]
 `(do 
    (when-not ~(first pairs)
      (throw (IllegalArgumentException. ~(str fnname " requires " (second pairs)))))
   ~(let [more (nnext pairs)]
      (when more
        (list* `assert-args fnname more)))))



(defn ^{:skip-wiki true} add-option-doc
  "Add option documentation from the doc-string to the option information."
  [doc-string, option-kv-pair]
  (if doc-string
    (if-let [match (re-find (re-pattern (format "<%1$s>(.*)</%1$s>" (first option-kv-pair))) doc-string)]
      (assoc-in option-kv-pair [1 :doc] (second match))
      option-kv-pair)
    option-kv-pair))


(defn ^{:skip-wiki true} default-param-meta
  "Create information about the optional parameter and its default value and the alternative values if any."
  [param-decl]
  (let [default-decl (second param-decl)]
		; if we have a default value declaration, ...
		(if-not (nil? default-decl)
		  ; ... then we create the parameter data with default value information:
		  (cond
		    ; when the default value declaration is a list starting with 'choice ...
		    (and (list? default-decl) (= (first default-decl) 'choice))
		      ; ... then the rest of the list contains the default and the alternatives.
		      (let [ [default & alternatives] (rest default-decl) ]
		        {:name (first param-decl), :default default, :alternatives alternatives})
		    ; when the default value declaration is a collection with the metadata flag ::choice ...
		    (and (coll? default-decl) (-> default-decl meta ::choice))
		      ; ... then the collection contains the default and the alternatives.
		      (let [ [default & alternatives] default-decl ]
		        {:name (first param-decl), :default default, :alternatives alternatives})
		    ; otherwise the default declaration is the default value.
		    :default
		      {:name (first param-decl), :default default-decl})
		  ; ... else we just have the name in the parameter data.
		  {:name (first param-decl)})))
  

(defn ^{:skip-wiki true} create-param-meta
  "Create information for meta data of the given mandatory and optional parameters."
  [doc-string, default-params, params]
  (->> (map default-param-meta default-params)
    (concat (map #(hash-map :name %) params))
    (map #(vector (:name %) %))
    (map (partial add-option-doc doc-string))
    (into {})))

(defn ^{:skip-wiki true} resolve-symbol
  "Resolves the given symbol and returns a full qualified symbol with namespace and name."
  [x]
  (when (symbol? x)
    (when-let [v (resolve x)]
      (symbol (-> ^clojure.lang.Var v .ns ns-name name) (-> ^clojure.lang.Var v .sym name)))))

(defn ^{:skip-wiki true} option-fn?
  "Does the given symbol resolve to a function defined via defn+opts?"
  [x]
  (and 
    (symbol? x)
    (when-let [v (resolve x)]
      (when-let [f (try (var-get v) (catch IllegalStateException e nil))]
        (and (fn? f) (-> v meta ::defn+opts))))))

(defn ^{:skip-wiki true} option-fn-call?
  "Is the given form a call to a function also defined via defn+opts with the given option symbol as last parameter?"
  [opt-name, form] 
  (and (option-fn? (first form)) (= (last form) opt-name)))

(defn ^{:skip-wiki true} option-fn-apply?
  "Is the given form an 'apply call to a function that is also defined via defn+opts and has the given options symbol as last parameter?"
  [opt-name, form] 
  (and (= (first form) 'apply) (option-fn? (second form)) (= (last form) opt-name)))


(defn ^{:skip-wiki true} macroexpand!
  "Return a non-macro form of the given form. 
  In case the form is a macro call it is expanded via macroexpand-1 until it cannot be expanded again."
  [form]
  (let [exp-form (macroexpand-1 form)]
    (if (= form exp-form)
      exp-form
      (recur exp-form))))

(defn ^{:skip-wiki true} child-forms
  "Returns all child forms of the given form. The returned child forms are function calls, i.e. lists or cons."
  [form]
  (loop [form-list (filter coll? (macroexpand! form)), children (list)]
    ; as long as there are remaining forms ...
    (if (seq form-list)
      ; ... make sure that the first form is no macro (by expanding any macro form) ... 
      (let [form (macroexpand! (first form-list))]
        ; if the form is a seq (list or cons), ...
        (if (seq? form)
          ; ... then we add it to the children and continue, ...
          (recur (rest form-list), (conj children form)),
          ; ... else we continue with the remaining forms plus the child forms of the current form provided it is a collection.
          (recur (concat (rest form-list) (when (coll? form) (filter coll? form))) , children)))
      children)))

(defn ^{:skip-wiki true} remove-parameter
  "Removes the parameter information of the given parameter symbol from all function parameter information in the given options map."
  [options, param-symb]
  (reduce
    #(update-in %1 [%2] dissoc param-symb)
    options
    (keys options)))

(defn ^{:skip-wiki true} find-transitive-options
  "Find all options in the given implementation body that are passed on by the options symbol specified in defn+opts 
  to other functions defined via defn+opts."
  [opt-name, body]
  (loop [form-list (child-forms body), found-options {}]
    (if (seq form-list)
      (let [form (first form-list)]
        (recur
          ; determine child forms
          (concat (rest form-list) (child-forms form)),
          ; determine transitive options if present
		      (cond
		        (option-fn-call? opt-name, form)
              (let [fsymb (first form)]
                ; merge options after replacing the :mine key by the full qualified symbol
                (merge found-options                  
                  (-> fsymb resolve meta :options (rename-keys {:mine (resolve-symbol fsymb)}) )))
            ; case (apply f ... options)
		        (option-fn-apply? opt-name, form)
              (let [
                    fsymb (second form),
                    fmeta (-> fsymb resolve meta),
                    ; calculate elements to skip = 2 (apply + fsymb) + #mandatory-parameter
                    drop-count (+ 2 (count (:mandatory-parameters fmeta)))
                    options
                    ; excluded options that are used in the 'apply function call from the documentation
                    (->> form
                      ; exclude 'apply, function name, mandatory parameters ... 
		                  (drop drop-count)
                      ; ... and the option symbol at the end of the form.
		                  butlast
                      ; select keys of given options
		                  (take-nth 2)
                      ; build symbols from the selected keys
                      (map (comp symbol name))
                      ; remove the selected symbols from the option map
                      (reduce remove-parameter (:options fmeta)))]
                ; add the options from the 'apply function call to the found options under the symbol of the called function
                (merge found-options (rename-keys options {:mine (resolve-symbol fsymb)})))
		        :else
		          found-options)))
      found-options)))


(def nl0 "\n  ")
(def nnl0 (str "\n" nl0))
(def nl1 (str nl0 "  "))
(def nnl1 (str "\n" nl1))
(def nl2 (str nl1 "  "))

(defn ^{:skip-wiki true} default-str
  "Converts the given default value to a string.
  String values are wrapped in quotes and \"nil\" is returned for nil.
  "
  [d]
  (cond 
    (nil? d) "nil"
    (string? d) (str "\"" d "\"")
    :else
     d))

(defn ^{:skip-wiki true} spacing-fmt
  "Create a format string for the given attribute key in the given options map 
  that ensures right alignment of the strings generated with that format for the attribute values.
  "
  [options, k]
  (if (seq options)
    (let [max-len (->> options (map #(-> % k str count)) (reduce max))]
      (if (pos? max-len)
        (str "%-" max-len "s")
        "%s"))
    "%s"))

(defn ^{:skip-wiki true} option-str
  "Create a string for the given options map."
  [nl, options]
  (let [name-fmt (spacing-fmt options, :name),
        doc-fmt  (spacing-fmt options, :doc )]
	  (->> options 
	    (map 
        #(str ":" (format name-fmt (:name %)) 
           (when (:doc %) (str "  " (format doc-fmt (:doc %)))) "  " 
           (if (contains? % :alternatives)
             (format "[choices = *%s]" (string/join ", " (map default-str (cons (:default %) (:alternatives %)))) )
             (format "[default = %s]" (default-str (:default %))))))
	    (string/join nl))))

(defn ^{:skip-wiki true} remove-option-doc
  "Remove the XML-style option documentation strings of the given options from the given function documentation."
  [options, fn-doc]
  (if fn-doc
		(string/trimr
			(reduce
			  (fn [fdoc, option]
			    (string/replace fdoc
			      (re-pattern (format "<%1$s>.*</%1$s>" (:name option)))
			      ""))
			  fn-doc
			  options))
    ""))

(defn ^{:skip-wiki true} update-doc
  [fn-doc, options-meta-map]
  (let [my-options (->> options-meta-map :options :mine vals (sort-by :name) seq),
        other-options (sort-by key (-> options-meta-map :options (dissoc :mine)))]
    (if (or my-options (and (seq other-options) (some #(some seq (val %)) other-options)))
      (str 
        (when fn-doc
          (str (remove-option-doc my-options, fn-doc) nnl0))
        "The following options can be specified:"
        (when (seq my-options)
          (str nl1 (option-str nl1, my-options)))
        (when (seq other-options) nnl1)
        (->> other-options
          (filter #(-> % val vals seq))
          (map #(str "Passed to function " (key %) ":" nl2 (option-str nl2, (vals (val %)))) )
          (string/join nnl1)))
      fn-doc)))

(defn ^{:skip-wiki true} create-param-check
  "Returns code that checks if a choice option has a valid value."
  [options-symb, param-data]
  `(when-not 
     (or 
       (not (contains? ~options-symb ~(-> param-data :name keyword))) 
       (contains? #{~(:default param-data) ~@(:alternatives param-data)} (~(-> param-data :name keyword) ~options-symb))
     )
     (throw 
       (IllegalArgumentException. 
         (format 
	         ~(str 
	            "Choice parameter " 
	            (-> param-data :name keyword) 
	            " was specified with wrong value: %s. Possible values are: "
	            (string/join ", " (map default-str (cons (:default param-data) (:alternatives param-data))))
	            ".") 
          (~(-> param-data :name keyword) ~options-symb))))))

(defn ^{:skip-wiki true} check-choice-fn
  "Returns code that checks the given values of all choice parameters if any."
  [options-map]
  (let [options-symb (gensym "options_"),
        choice-params (filter #(contains? % :alternatives) (vals options-map))]
    (if (seq choice-params)
	   `(fn [option-map#]
		    (let [~options-symb (when (seq option-map#) (into {} option-map#))]
		      ~@(map (partial create-param-check options-symb) choice-params)))
     `(fn [option-map#]))))

(defn ^{:skip-wiki true} maybe-set-default-values
  [default-map, option-map]
  
)

(defn ^{:skip-wiki true} create-defn+opts-decl
  "Creates the code that declares a function with optional parameters and meta information about them."
  [fname, meta-map, param-symb-list, opt-decl, body]
  (let [as? (-> opt-decl butlast last (= :as)),
        opt-name (if as? (last opt-decl) (gensym "options_")),
        opt-decl (if as? (drop-last 2 opt-decl) opt-decl),
        opt-decl (when (seq opt-decl) (apply concat opt-decl)),
        {default-params true, params false} (group-by coll? opt-decl),
        option-symbols (vec (concat params (map first default-params))),        
        param-vec (conj (vec param-symb-list) '& opt-name),
        options-meta-map 
        (-> {} 
          (assoc ::defn+opts true) 
          (assoc-in [:options :mine] (create-param-meta (:doc meta-map), default-params, params))
          (assoc :mandatory-parameters (vec param-symb-list))
          (update-in [:options] merge (find-transitive-options opt-name, body))),
        
        meta-map (update-in meta-map [:doc] update-doc, options-meta-map)
        default-map (->> (get-in options-meta-map [:options :mine])
                      vals
                      (remove #(-> % :default nil?))
                      (map (fn [param-info] [(-> param-info :name keyword) (:default param-info)]))
                      (into {})),
        check-choice (check-choice-fn (-> options-meta-map :options :mine)) 
       ]
   `(do
      (defn ~fname ~meta-map ~param-vec        
	      (let [~opt-name 
              (cond
                (nil? ~opt-name)
                  {},
                (= 1 (count ~opt-name))
                  (if (-> ~opt-name first meta ::option-map)
                    (first ~opt-name)
                    (throw 
                      (IllegalArgumentException. 
                        (format "An option list is expected when calling function \"%s\" with only one parameter!" (str ~fname)))))
                :else
                  (apply hash-map ~opt-name))]
          (~check-choice ~opt-name)
          (let [~@(when (seq default-map) [opt-name `(merge ~default-map ~opt-name)]),
                {:keys ~option-symbols} ~opt-name,
		            ~opt-name (vary-meta ~opt-name assoc ::option-map true)]
            ~@body)))
      (alter-meta! #'~fname merge '~options-meta-map)
      (alter-meta! #'~fname assoc :arglists (list '~(conj (vec param-symb-list) '& 'options)))
      #'~fname)))

; Does not support using previous declared parameters as default values.
(defmacro defn+opts
  "Define a function with defn-like syntax and option support."
  [fname & fdecl]
  (let [{:keys [meta-map body-list]} (process-defn-decl fdecl)]
    (assert-args defn+opts 
      (= 1 (count body-list)) "that only one body is specified")
    (let [[params & body] (first body-list),
          [symb-list delimiter opt-decl] 
          (let [parts (partition-by #(= % '|) params)]
            (if (= 2 (count parts))
              [[] (first parts) (second parts)]
              parts))]
      (when (and delimiter (empty? opt-decl)) 
        (println (str "WARNING: No optional parameter specification given after \"|\" in parameter declaration of \"" (str fname) "\"!")))
      (create-defn+opts-decl fname, meta-map, symb-list, opt-decl, body))))

(defmacro defn+opts-
  "Define a private function with defn-like syntax and option support."
  [fname & fdecl]
  (list* `defn+opts (with-meta fname (assoc (meta fname) :private true)) fdecl)  
)