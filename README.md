# Iter - A iteration construct for Clojure

An iteration and looping DSL for Clojure.  Iter provides an extensible
looping language that is an alternate to higher order functions.

For example

    (iter (for-each x [1 2 3 4 5 6 7])
          (when (> x 2)
            (collect (* x x))))

      => (9 16 25 36 49)

Iter is inspired to the Common Lisp Iterate package
https://common-lisp.net/project/iterate/doc/index.html#Top

# Overview

# Operators

# Examples

# Writing iter macros

# Internals

# Testing

`make test`

# Authors

- Chris Dean <ctdean@sokitomi.com>
