# Iter - An iteration construct for Clojure

An iteration and looping DSL (Domain Specific Langauge) for Clojure.
`iter` provides an extensible looping language that is an alternate to
higher order functions.

For example

    (iter (foreach x [1 2 3 4 5 6 7])
          (when (> x 2)
            (collect (* x x))))

      => (9 16 25 36 49)

Iter is inspired to the Common Lisp Iterate package
https://common-lisp.net/project/iterate/doc/index.html#Top and has
some similarities to list comprehension systems.

# Installation

Add the following to your project.clj `:dependencies`

    [ctdean/iter "0.4.0"]

and then require the iter macro:

    (require '[iter.core :refer [iter]])

or

    (ns my.app
      (:require [iter.core :refer [iter]]))

# Overview

`iter` is an alternate way of iterating and looping to using the
traditional Clojure tools such as higher order functions.  It provides
a rich set of operators to walk a sequence, return a lazy sequence,
return a single expression, or reduce a complex sequence.

Let's walk through a simple example:

    (iter (foreach x [1 2 3 4 5])
          (collect (+ x 100)))

      => (101 102 103 104 105)

The `iter` macro contains a mix of iter operators and regular Clojure
expressions.  In this example,

- `foreach` binds the variable `x` in turn to each element in the
  Clojure sequence `[1 2 3 4 5]`.
- The variable `x` is now in scope for the rest of the `iter` form.
- The `collect` operator adds the result of the regular Clojure
  expression `(+ x 100)` to the lazy sequence that `iter` returns.
- The lazy sequence `(101 102 103 104 105)` is returned.

Iter has a rich set of operators so you can perform more complex
processing.  Let's look at some more examples, the first with a simple
`if` branch::

    (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8])
          (if (even? x)
              (collect (* x x))))

      => (16 4 36 64)

and with an if/else operator:

    (iter (foreach x (range 10))
          (if (even? x)
              (collect (+ 10 x))
              (collect (int (Math/pow 2 x)))))

      => (10 2 12 8 14 32 16 128 18 512)

and with multiple collects:

    (iter (foreach num (range 5))
          (collect (* num num))
          (when (even? num)
            (collect (+ 10 num))))

      => (0 10 1 4 12 9 16 14)

It's also easy to build up a hash-map:

    (iter (foreach word ["apple" "peaches" "pumpkin pie"])
          (collect-map word (count word)))

      => {"pumpkin pie" 11, "peaches" 7, "apple" 5}

Or a unique sequence:

    (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8])
          (collect-uniq (+ x 10)))

      => (13 11 14 15 19 12 16 18)

You can also reduce a sequence:

    (iter (foreach x (range 10))
          (when (odd? x)
            (reducing x :by +)))

      => 25

Or bind new variables within the scope of iter:

    (iter (foreach x (range 10))
          (with y (* x x))
          (when (> y 20)
            (collect y)))

      => (25 36 49 64 81)

Or maximize a value:

    (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8])
          (maximizing x))

      => 9

    (iter (foreach s (clojure.string/split
                      "Lisp is a programmable programming language" #" "))
          (maximizing s :using (count s)))

      => "programmable"

In general, the `iter` macro contains a series of forms, where some of
the forms are `iter` operators and the rest of the forms are just
plain Clojure expressions.

# How it works

The way that `iter` works is by parsing the input forms, translating
the iter operators and leaving the remaining Clojure forms alone.  A
recursive Clojure expression is generated by the macro to iterate
through all the inputs.  Think of the iter DSL as a state machine
where each form is an action to move the loop to the next state.

The state machine generated does basically three things:

- It iterates through a sequence, collection, or expression.  The
  `foreach` operator is the most common way to iterate.
- It collects results in a lazy sequence.  The `collect` operator is
  the most common way to collect.
- It accumulates results using the `accum` operator.  All `accum`
  variables are always available to use or modify.

One important point: all the non-iter operators are left completely
alone.  There is no walking of Clojure forms to find the `iter`
operators hidden inside them.

As mentioned, there are builtin iter operators like `collect`,
`foreach`, and `accum` that can be used to create new operators with
the `define-iter-op` clause.

# Nested Loops

When you have more that one iteration operator then `iter` creates
nested loops:

    (iter (foreach row [1 2 3])
          (collect [:row row])
          (foreach col ["a" "b"])
          (collect [row col]))

      => ([:row 1] [1 "a"] [1 "b"]
          [:row 2] [2 "a"] [2 "b"]
          [:row 3] [3 "a"] [3 "b"])

This nesting behavior holds true for all the Iteration Operators (see
below).

If you want to avoid nesting and process the sequences in parallel,
you can use multiple sequences to the `foreach` operator:

    (iter (foreach [row col] [1 2 3 4 5] ["a" "b" "c" "d"])
          (collect [row col]))

    => ([1 "a"] [2 "b"] [3 "c"] [4 "d"])

where the sequences are truncated to the length of the shortest
sequence.

# Operators

## Iteration Operators

### foreach

`(foreach var coll)`

`(foreach var coll & colls)`

`foreach` iterates over each element in `coll`.  `coll` is treated as
a sequence and each element in the sequence is bound to `var` for each
iteration.  We iterate through `coll` by repeatedly calling `rest` on
it unti `empty?` returns true.

If more than one sequence is given we iterate over the sequences
simultaneously in the same way that `map` does.

Note that normal Clojure destructuring applies when binding `var`.

Examples:

    (iter (foreach x (range 10))
          (prn x))

    (with-open [in (clojure.java.io/reader "/etc/passwd")]
      (iter (foreach line (line-seq in))
            (when (re-find #"daemon" line)
              (return line))))

    (iter (foreach [name i] [:a :b :c :d] (range))
          (collect-map name i))

See _Nested Loops_ for examples of nested loops.

### forlist

`(forlist var coll)`

`forlist` is like `foreach`, but instead of iterating over each
element it iterates over successive sub-sequences of `coll`.  For
example,

    (iter (forlist x [:a :b :c :d])
          (prn x))

when evaluated, prints

    [:a :b :c :d]
    (:b :c :d)
    (:c :d)
    (:d)

The `forlist` operator is useful for comparing elements further down
the list.  Here's an example for finding the distance between elements
in a vector:

    (iter (forlist nums [2 7 1 8 2 8 1 8 2])
          (with head (first nums))
          (with tail (rest nums))
          (with dist (.indexOf tail head))
          (when (> dist 0)
            (collect [head dist])))

      => ([2 3] [1 3] [8 1] [2 3] [8 1])


### fornext

`(fornext var next-expr)`

`(fornext var next-expr init)`

`(fornext var next-expr init done?)`

This is the iteration building block.  `var` starts out bound to
`init` and is then bound to `next-expr` for every loop iteration.  The
loop terminates when `done?` returns true.

If not given, `init` defaults to `nil` and `done?` defaults to `false`.

You usually don't need to call `fornext` directly, but all the
Iteration Operators are implemented uing `fornext`.

To create an infinite loop:

    (iter (fornext tmp nil nil false)
          (collect :a))

To count from 1 to 10:

    (iter (fornext i (inc i) 1 (> i 10))
          (collect i))

      => (1 2 3 4 5 6 7 8 9 10)

These will work, but calling times and range is probably clearer:

    (iter (times)
          (collect :a))

    (iter (foreach i (range 1 11))
          (collect i))

### times

`(times n)`
`(times)`

Iterate `n` times, or infinitely if `n` is omitted.

    (iter (times 8)
          (collect (rand-int 100)))

      => (82 77 85 25 10 13 5 0)

## Collection operators

### collect

`(collect expr)`

Produce a lazy sequence of `expr` each time `collect` is called.  This
is the basic way to produce results.

    (iter (foreach x (range 5))
          (collect (* x x x)))

      => (0 1 8 27 64)

The `collect` operator can be called as many or as few times as needed:

    (iter (foreach x (range 5))
          (when (even? x)
            (collect "even!"))
          (collect x)
          (collect (* x x)))

      => ("even!" 0 0 1 1 "even!" 2 4 3 9 "even!" 4 16)

### collect-cat

`(collect-cat seq-expr)`

Just like `collect`, but concat `seq-expr` onto the result.

    (iter (foreach x (range 5))
          (collect-cat [x (* x x)]))

      => (0 0 1 1 2 4 3 9 4 16)

The value of `seq-expr` must be a sequence and not a scalar.

###  collect-map

`(collect-map key value)`

Similar to `collect`, but produces a `hash-map` instead of a sequence.
This produces the actual hash-map and is not lazy.

    (iter (foreach x (range 10))
          (collect-map x (* x x)))

      => {0 0, 7 49, 1 1, 4 16, 6 36, 3 9, 2 4, 9 81, 5 25, 8 64}

    (iter (foreach x (range 10))
          (collect-map (clojure.pprint/cl-format nil "~r" x)
                       x))

      => {"six" 6, "three" 3, "two" 2, "seven" 7, "zero" 0,
          "five" 5, "eight" 8, "one" 1, "nine" 9, "four" 4}

### collect-uniq

`(collect-uniq expr)`

Just like `collect`, but returns a unique/distinct lazy sequence of
values:

    (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8])
          (collect-uniq x))

      => (3 1 4 5 9 2 6 8)

### collect-freq

`(collect-freq expr)`

Just like `collect`, but returns a frequency count of the values:

    (iter (foreach x [2 7 1 8 2 8 1 8])
          (when (even? x)
            (collect-freq (cl-format nil "~r" x))))

      => {"two" 2, "eight" 3}

## Control Flow Operators

### if

`(if test then)`

`(if test then else)`

Just like the normal Clojure `if`, but modified to support `iter`.
That is, evaluate `test` and if true evaluate `then`, and if false
evaluate `else`.  If `else` is missing then we just evaluate nil.

    (iter (foreach x [3 1 4 1 5 9 2 6 5 3 5 8])
          (if (odd? x)
              (collect x)
              (collect (- x))))

      => (3 1 -4 1 5 9 -2 -6 5 3 5 -8)

This is not the regular Clojure `if` even though it behaves like one,
it is part of the iter DSL.  As such, the `if` variants like `if-not`
are not valid iter expressions.

### when

`(when test & body)`

This is equivalent to `(if test (do body))`.  It's useful for the
times when there are multiple `collect` or `accum` statements within
`body`,

    (iter (foreach x (range 20))
          (when (odd? x)
            (collect (format "%d" x))
            (collect (format "0x%x" x))))

      => ("1" "0x1" "3" "0x3" "5" "0x5" "7" "0x7" "9" "0x9"
          "11" "0xb" "13" "0xd" "15" "0xf" "17" "0x11" "19" "0x13")

### do

`(do & body)`

Just like the normal Clojure `do`, but modified to support `iter`.
That is, evaluate the `body` expressions in order.

    (iter (foreach x (range 20))
          (if (odd? x)
              (do
                (collect (format "%d" x))
                (collect (format "0x%x" x)))
              (do
                (collect (Integer/toBinaryString x))
                (collect (format "0%o" x)))))

      => ("0" "00" "1" "0x1" "10" "02" "3" "0x3" "100" "04" "5" "0x5"
          "110" "06" "7" "0x7" "1000" "010" "9" "0x9" "1010" "012"
          "11" "0xb" "1100" "014" "13" "0xd" "1110" "016" "15" "0xf"
          "10000" "020" "17" "0x11" "10010" "022" "19" "0x13")


### return

`(return expr)`

Stop execution and return `expr`.

    (iter (foreach x [3 1 4 1 5 9])
          (when (> x 4)
            (return x)))

      => 5

You cannot use both `collect` and `return` in the same `iter` forms.

### stop

`(stop)`

Stop execution of the `iter` forms.

    (iter (foreach x [3 1 4 1 5 9])
          (collect x)
          (when (> x 4)
            (stop)))

      => (3 1 4 1 5)

### break

`(break)`

`(break nlevels)`

Like the C Language `break`, stop the iteration of the current loop.
If used inside a nested loop, the outer loop keeps executing.

If `nlevels` is given in a nested loop, break out of that many loops.

    (iter (foreach x (range 10))
          (if (> x 4)
              (break))
          (collect x))

      => (0 1 2 3 4)

### continue

`(continue)`

`(continue nlevels)`

Like the C Language `continue`, skip the this step in the iteration of
the current loop.

If `nlevels` is given and we are in a nested loop, continue with the
loop that many levels out.

    (iter (foreach x (range 10))
          (if (even? x)
              (continue))
          (collect x))

      => (1 3 5 7 9)

### finding

`(finding expr)`

`(finding expr where)`

Return the the first value of `expr` that is true.  If `where` is
given, return the first value of `expr` that has `where` true.

    (iter (foreach s (clojure.string/split
                       "Lisp is a programmable programming language" #" "))
          (finding (clojure.string/lower-case s) (> (count s) 5)))

      => "programmable"

## Binding Operators

### with

`(with var init)`

The `with` operator binds `var` to `init` for the remainder for the
`iter` form.  This is similar to a Clojure `let` expression.

    (iter (foreach x (range 10))
          (with y (+ 10 x))
          (with z (+ 100 y))
          (collect z))

      => (110 111 112 113 114 115 116 117 118 119)

### with-first

`(with-first var)`

Sets `var` to true in the first cycle of the iteration, and false
every other time.

    (iter (foreach s ["Apple" "Peaches" "Pumpkin Pie"])
          (with-first first?)
          (when (not first?)
            (print ", "))
          (print s))

when evaluated, prints:

    "Apple, Peaches, Pumpkin Pie"

### with-prev

`(with-prev var source-var)`

`(with-prev var source-var init)`

Sets `var` to the previous value of `source-var`.  In this way you can
gain access to the last value of `source-var`.

If not given, `init` defaults to `nil`.

    (iter (foreach x (range 5))
          (with-prev y x :start)
          (collect [x y]))))

      => ([0 :start] [1 0] [2 1] [3 2] [4 3])


### accum

`(accum var expr)`

`(accum var expr init)`

Accumulate `var` by `expr`.  Binds `var` for the remainder for the
`iter` form.  This is similar to `with`, but `var` is persistent
through each cycle of the iteration.

`accum` acts as an alternate to `collect` to gather values, but where
`collect` creates and returns a lazy sequence `accum` persists the
values across iterations.

If not given, `init` defaults to `nil`.

    (iter (foreach x (range 10))
          (accum sum (+ sum x) 0)
          (collect sum))

      => (0 1 3 6 10 15 21 28 36 45)

    (iter (times 10)
          (with-prev m k 0)
          (accum k (+ m k) 1)
          (collect m))

      => (0 1 1 2 3 5 8 13 21 34)

## Aggregate Operators

### counting

`(counting expr)`

Return the count of the number of times `expr` is true.

    (iter (foreach x (range 10))
          (when (odd? x)
            (counting x)))

      => 5

### maximizing

`(maximizing expr)`

`(maximizing expr :using key-expr)`

Return the maximum value of `expr`.  If `key-expr` is given maximize
by that value.

    (iter (foreach [co market-cap] {:aapl 478 :brk 308 :goog 313
                                    :msft 340 :xom 422})
          (maximizing co :using market-cap))

      => :aapl

### minimizing

`(minimizing expr)`

`(minimizing expr :using key-expr)`

Return the minimum value of `expr`.  If `key-expr` is given minimize
by that value.

    (iter (foreach [co market-cap] {:aapl 478 :brk 308 :goog 313
                                    :msft 340 :xom 422})
          (minimizing co :using market-cap))

      => :brk

### summing

`(summing expr)`

Add up `expr` each time `summing` is called and and return the result.
`summing` is initially set to 0 and return 0 if `summing` is never
executed.

        (iter (foreach x (range 1 101))
              (summing x))

          => 5050


        (iter (foreach x (range 10))
              (when (> x 100)
                (summing x)))

          => 0

### multiplying

`(multiplying expr)`

Multiply `expr` each time `multiplying` is called and and return the
result.  `multiplying` is initially set to 1 and return 1 if
`multiplying` is never executed.

    (iter (foreach x (range 6))
          (when (odd? x)
            (multiplying x)))

      => 15

    (iter (foreach x (range 6))
          (when (> x 100)
            (multiplying x)))

      => 1

### reducing

`(reducing expr :by by)`

`(reducing expr :by by :init init)`

`(reducing expr :by by :finally-by finally-by)`

`(reducing expr :by by :init init :finally-by finally-by)`

The `iter` version of the Clojure `reduce` operator.  `expr` is
collected and the `by` function is called using Clojure `reduce`.  If
`init` is given, it is used as the initial value for `reduce`.  If
`finally-by` is given, it is a function of one argument that handles
the result of `reduce`.

    (iter (foreach x (range 10))
          (if (even? x)
              (reducing x :init 100 :by +)))

     => 120

## Pre and Post Processing Operators

### begin

`(begin & body)`

Evaluate the `body` expressions before any iteration starts.  All
`accum` variables are available in the begin block.

    (iter (foreach i (range 5))
                     (begin (println "Start"))
                     (println i))

When evaluated, prints

    Start
    0
    1
    2
    3
    4

### end

`(end & body)`

Evaluate the `body` expressions after iteration ends.  All `accum`
variables are available in the end block.

    (iter (foreach i (range 5))
          (end (println "Done"))
          (println i))

when evaluated, prints

    0
    1
    2
    3
    4
    Done

### finally-by

`(finally-by f)`

Apply the single argument function `f` to the lazy sequence that iter
generates.

    (iter (foreach x (range 10))
          (when (even? x)
            (collect x))
          (finally-by (fn [xs] (reduce + xs))))

      => 20

# iter*

Much in the same way you would use `doseq` for side effects, `iter*`
can be used for looping side effects.

Although you can wrap the normal `iter` expression in `(doall (iter ...)`,
you can use `iter*` for the same purpose.  Be careful returning large
sequences, as `iter*` causes the entire returning sequence to reside
in memory.

    (iter* (times 3)
           (println "Hello world!"))

      => nil

Also prints:

    Hello world!
    Hello world!
    Hello world!

# Writing iter macros

The `define-iter-op` is used to create new `iter` operators by
defining an iter macro.  The only difference between `iter` macros and
regular macros is that `iter` macros are recursively parsed by `iter`.

All the normal caveats to writing regular macros apply when writing
`iter` macros, but `iter` macros have an extra challenge.  Because
`iter` can expand multiple macros at once you should always use
`gensym` to create temporary variables and not the `#` suffix (as in
`var#`).

    (define-iter-op my-counting [x]
      (let [counter (gensym "counter-")]
        `(do
           (when ~x
             (accum ~counter (inc ~counter) 0))
           (end (return ~counter)))))

    (define-iter-op my-maximizing [x]
      `(iter.core/reducing ~x :by max))


# Testing

`make test`

# Authors

- Chris Dean <ctdean@sokitomi.com>
