# memory-whole

A Clojure library to leverage actual data to build runtime function annotations.

## Usage

1. `mem-whole!` your function (or ns)
2. call the function multiple times
3. use tools to generate specifications for your function

## TODO

1. [x] finish tracing
   1. [x] figure out how to get a hold of source from var
   2. [x] proper collisions between sources
   3. [x] copy over sql-lite functions
   4. [x] write tests
2. [ ] generate data shapes
   0. [ ] delineate function generations by ast hash
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
