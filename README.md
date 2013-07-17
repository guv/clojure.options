# clojure.options

This library provides improved handling for Clojure functions with optional parameters.
The library provides the macro ```defn+opts``` which defines a function with optional parameters.
The syntax is similar to the one of Clojure's ```defn``` except that only one function body is allowed.

A documentation string for optional parameters is supported.
When a ```defn+opts``` function ```f``` calls another ```defn+opts``` function ```g``` and passes its ```options``` symbol
then the docstring of ```f``` will contain information about the optional parameters that can be passed to ```g```.

## Install

Add the following to your ```project.clj``` to let Leiningen install ```clojure.options``` for you:

```clj
:dependencies [[clojure.options "0.2.7"]]
```

## Usage

The following example shows the basic syntax for a function definition with the optional parameter ```base``` with default value ```10```.

```clj
(defn+opts int->str
  "Converts a given positive integer into a string.
  <base>The base to use for string encoding (<= 10).</base>
  "
  [x | {base 10}]
  (loop [x x, digits (list)]
    (if (pos? x)
      (recur (quot x base) (conj digits (mod x base)))
      (apply str digits))))
```
A call to that function looks like:

```clj
(int->str 23 :base 2)
;=> "10111"
(int->str 23)
;=> "23"
```

The documentation for ```int->str``` looks like

```clj
(doc int->str)
;-------------------------
;clojure.options/int->str
;([x & options])
;  Converts a given positive integer into a string.
;
;  The following options can be specified:
;    :base  The base to use for string encoding (<= 10).  [default = 10]
;=> nil
```

The main goal of this library is illustrated with the following function definition:

```clj
(defn+opts convert-ints
  "Converts all integers in a string to another base."
  [s | :as options]
  (clojure.string/replace s #"\d+" #(-> % Integer/parseInt (int->str options))))
```

The function ```convert-ints``` uses ```int->str``` to convert all integers of a given string.
Therefore, it forwards the specified options in the ```convert-ints``` call to the ```int->str``` function.

```clj
(convert-ints "There are 2 types of people ..." :base 2)
;=> "There are 10 types of people ..."
```

The documentation of ```convert-ints``` looks like:

```clj
(doc convert-ints)
;-------------------------
;clojure.options/convert-ints
;([s & options])
;  Converts all integers in a string to another base.
;
;  The following options can be specified:
;
;    Passed to function clojure.options/int->str:
;      :base  The base to use for string encoding (<= 10).  [default = 10]
;=> nil
```

## License

Copyright © 2012-2013 Gunnar Völkel

Distributed under the Eclipse Public License, the same as Clojure.
