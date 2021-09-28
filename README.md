# memory-whole

[![Clojars Project](https://img.shields.io/clojars/v/net.clojars.escherize/memory-whole.svg)](https://clojars.org/net.clojars.escherize/memory-whole)

A Clojure library to leverage actual data to build runtime function annotations.

Require like so: `[memory-whole.core :as mem]`

## How it works

1. `mem/trace` or `mem/deftrace` functions

Traced functions will save their inputs/outputs to a local sqlite db by default, or whatever db you point it at.

2. call the functions multiple times in the usual way

TODO: As functions begin putting their values into the memory whole, we can generate specs or malli schema from the raw data.

3. use built-in tools to generate specifications and code hints (todo)

There's got to be some nice ways to use this information to improve our dev experience / correctness.

## Follow Along

Let's start with an example of `boo` which just multiplies its arguments, `a` and `b`.

``` clojure
(ns my.ns
  (:require [memory-whole.core :as mem]))
 
(defn boo [a b]
  (* a b))
```

Let's setup memory-whole tracing for `boo` like so:

``` clojure
(mem/trace-vars boo) ;; 1
;; => #'my.ns/boo
```
  
`boo` Acts normal.
  
``` clojure
(boo 10 20) ;; 2
;; => 200
```
  
But now we can query for runtime information about `boo`:
  
``` clojure
(mem/one 'boo)
{:arguments [10 20],
 :name "boo",
 :file "*snip*/memory-whole/README.md",
 :start_time 1630460847233,
 :source "(defn boo [a b]\n  (* a b))", ;;n.b. source is flaky PRs welcome
 :output 200,
 :column 1,
 :end_time 1630460847243,
 :line 31,
 :id 4,
 :ast_hash 948857165,
 :exception nil,
 :full_name "my.ns/boo",
 ;; fixme
 :arg_lists nil}

```

If an exception occurs in `boo`, we can observe that too:

``` clojure

(boo "ten" "zero")
;; =exception=>

(mem/one 'boo)
{:name "boo",
 :arguments ["ten" "zero"],
 :source "(defn boo [a b]\n  (* a b))",
 :output nil,
 :exception
 "#error {
 :cause \"java.lang.String cannot be cast to java.lang.Number\"
 :via
 [{:type java.lang.ClassCastException
   :message \"java.lang.String cannot be cast to java.lang.Number\"
   :at [clojure.lang.Numbers multiply \"Numbers.java\" 173]}]
 :trace
 [[clojure.lang.Numbers multiply \"Numbers.java\" 173]
  [my.ns$boo invokeStatic \"form-init1212254902669330318.clj\" 32]
  [my.ns$boo invoke \"form-init1212254902669330318.clj\" 31]

   ...

  ]}"
 ;; and the other keys
 }
```

Let's try looking up more things. You have access to `mem/one` and `mem/many`, which take the function name (as a string, symbol or var) to let you see the latest. (`mem/many` also takes an optional limit which defaults to 10)

``` clojure
(count (mem/many 'boo))
;;=> 2

;; call the function a few times:
(dotimes [_ 100] (boo (rand-int 99) (+ 100 (rand-int 99))))

(count (mem/many "boo" 200))
;;=> 102

(count (remove :exception (mem/many "boo" 200)))
;;=> 101

(mapv :arguments
      (mem/select ["select id, arguments from calls where name = ? and exception
                    is null order by start_time limit 10"
                   "boo"]))
;;=> [[10 20] [90 142] [13 129] [63 162] [25 172] [73 180] [5 186] [73 150] [92 187] [77 146]]

```

Okay! we have seen that there are a few interesting things that we can learn with memory-whole...

## Getting use of our data

There are a few nice things we can do now.

### Data-Shape generation

#### Malli Provider
``` clojure
(require '[malli.provider :as mp])

(mp/provide
 (mapv :arguments
       (mem/select ["select id, arguments from calls where name = ? and exception
                    is null order by start_time limit 10"
                    "boo"])))

;;=> [:vector int?]

(mp/provide
 (mapv :output
       (mem/select ["select id, output from calls where name = ? and exception
                    is null order by start_time limit 10"
                    "boo"])))

;;=> int?

```

#### Clojure Spec Provider
``` clojure
(require '[spec-provider.provider :as sp])

(sp/infer-specs
 (mapv :arguments
       (mem/select ["select id, arguments from calls where name = ? and exception
                    is null order by start_time limit 10"
                    "boo"]))
 :boo/arguments)

;;=> ((clojure.spec.alpha/def
;;     :boo/arguments
;;     (clojure.spec.alpha/coll-of clojure.core/integer?)))

(sp/infer-specs
 (mapv :output
       (mem/select ["select id, output from calls where name = ? and exception
                    is null order by start_time limit 10"
                    "boo"]))
 :boo/outputs)

;;=> ((clojure.spec.alpha/def :boo/outputs clojure.core/integer?))

```


## TODO

1. [x] finish tracing
   1. [x] figure out how to get a hold of source from var
   2. [x] proper collisions between sources
   3. [x] copy over sql-lite functions
   4. [x] write tests
   5. [ ] figure out why tests cant read source
2. [x] generate data shapes
   0. [ ] delineate function generations by ast hash in a smart way
   This is
   How many samples would be good? How accurate should the generation be? 
   1. [ ] malli
   2. [ ] spec
3. [ ] how to view these things?
   1. clj-kondo, like:  ?
   2. just call `mem/spec-for` ?

## License

Copyright © 2021 Bryan Maass 

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
