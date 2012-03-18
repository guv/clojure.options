# clojure.options

This library provides improved handling for Clojure functions with optional parameters.
The library provides the macro ```defn+opts``` which defines a function with optional parameters.
The syntax is similar to the one of Clojure's ```defn``` except that only one function body is allowed.

A documentation string for optional parameters is supported.
When a ```defn+opts``` function ```f``` calls another ```defn+opts``` function ```g``` and passes its ```options``` symbol
then the docstring of ```f``` will contain information about the optional parameters that can be passed to ```g```.

## Install

Add the following to your project.clj to let Leiningen install clojure.options for you:

```
:dependencies [[clojure.options 0.2.0]]
```

## Usage

The following example shows the basic syntax for a function with the optional parameter 'base with default value 10.

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

```
(doc int->str)
-------------------------
clojure.options-tests/int->str
([x & options])
  Converts a given positive integer into a string.

  The following options (in 'options) can be specified:
    :base  The base to use for string encoding (<= 10).  [default = 10]
nil
```

FIXME: write

## License

Copyright (C) 2012 Gunnar Völkel

Distributed under the Eclipse Public License, the same as Clojure.
