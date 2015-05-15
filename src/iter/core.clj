(ns iter.core
  "An iteration and looping DSL for Clojure.  Iter provides an
  extensible looping language that is an alternate to higher order
  functions.

  For example

  (iter (for-each x [1 2 3 4 5 6 7])
        (when (> x 2)
          (collect (* x x))))

    => (9 16 25 36 49)

  Iter is inspired to the Common Lisp Iterate package
  https://common-lisp.net/project/iterate/doc/index.html#Top

  @ctdean"
  (:refer-clojure :exclude [when])
  (:require
   [medley.core :refer :all]
   [clojure.core.match :refer [match]]
   [clojure.pprint :refer [pprint]]))

(def registered-macros (atom {}))

(declare compile-ops)
(declare output)
(declare maybe-expand)
(declare build-parse-tree)

;; The state of the compiled operators
(def ^{:dynamic true :private true} *op-state* nil)

;; The currently defined end operators
(def ^{:dynamic true :private true} *ends* nil)

;; A sentinel variable value that shouldn't be returned to the user.
(def ^:private sentinel (Object.))

;;;
;;; Parse iter forms and return our version of an AST.
;;;

(defn- parse
  "Parse a single iter form.  The form is either a builtin iter
  keyword, a special iter macro, or a normal Clojure expression.

  - A keyword is turned into an AST node,
  - An iter macro is expanded and recursively parsed
  - An AST node is created for all Clojure expressions and parsing
  terminates

  Some iter forms are recursively parsed (such as the THEN clause of
  an IF expression), but other forms (such as the COLLECT clause) are
  assumed to be simple Clojure expressions are placed in the AST as
  is."
  [form]
  (match (vec form)
    [:collect expr]                     {:collect expr}
    [:collect-cat expr]                 {:collect-cat expr}
    [:with var expr]                    {:with {:binding var :init [{:expr expr}]}}
    [:accum var expr init]              {:with {:binding var :init [{:expr expr}]}
                                         :prologue {:init-vars var :init-vals init}}
    [:if test then]                     {:if {:test test :then (build-parse-tree [then])
                                              :else nil}}
    [:if test then else]                {:if {:test test :then (build-parse-tree [then])
                                              :else (build-parse-tree [else])}}
    [:begin expr]                       {:begin (build-parse-tree [expr])}
    [:end expr]                         {:end (build-parse-tree [expr])}
    [:stop]                             {:stop sentinel}
    [:stop expr]                        {:stop expr}
    [:return expr]                      {:return expr}
    [:do & forms]                       {:do (build-parse-tree forms)}
    [:finally-by f]                     {:finally-by f}
    :else                               (maybe-expand form)))

(defn- maybe-expand
  "If this is an iter macro, then manually expand the form an
  recursively parse it."
  [form]
  (let [name (first form)]
    (if (keyword? name)
        (throw (IllegalArgumentException.
                (format "Unknown iter keyword %s => %s" name form)))
        (if-let [mname (@registered-macros name)]
          {:do (build-parse-tree [(macroexpand-1 (cons mname (rest form)))])}
          {:expr form}))))

(defn- build-parse-tree
  "Parse the iter clauses into an AST."
  [clauses]
  (->> clauses
       (map parse)
       (mapcat seq)
       (map #(apply hash-map %))))

;;;
;;; Macros
;;;

(defn- reg-macro [src dst]
  (let [fq-src (symbol (str (ns-name *ns*) "/" src))
        fq-dst (symbol (str (ns-name *ns*) "/" dst))]
    (swap! registered-macros assoc src fq-dst)
    (swap! registered-macros assoc fq-src fq-dst)))

(defmacro define-iter-clause
  "Define an iter macro.  The only difference between iter macros and
  regular macros is that iter macros are recursively parsed by iter.

  We register the name of the iter macro so that we know to keep
  parsing.  The macro might be called using the plain or fully
  qualified name, so we register both names."
  [iname & args-body]
  (reg-macro iname iname)
  `(defmacro ~iname ~@args-body))

;; Macro versions of the builtin iter keywords.
(define-iter-clause collect [& args]    `(:collect ~@args))
(define-iter-clause collect-cat [& args] `(:collect-cat ~@args))
(define-iter-clause do [& args]         `(:do ~@args))
(define-iter-clause if [& args]         `(:if ~@args))
(define-iter-clause begin [& args]      `(:begin ~@args))
(define-iter-clause return [& args]     `(:return ~@args))
(define-iter-clause stop [& args]       `(:stop ~@args))
(define-iter-clause end [& args]        `(:end ~@args))
(define-iter-clause accum [& args]      `(:accum ~@args))
(define-iter-clause with [& args]       `(:with ~@args))
(define-iter-clause finally-by [& args] `(:finally-by ~@args))

(reg-macro 'iter 'nested-iter)
(defmacro nested-iter [& body]
  (let [nested (gensym "nested-")]
    `(do
       (with ~nested (iter ~@body))     ; With takes a simple expr so iter
       (collect-cat ~nested))))         ; won't be recursively expanded

;; We need to define this here so we can use use WHEN in our normal
;; clojure code below.
(define-iter-clause when [test & body]
  `(if ~test
       (do ~@body)))

;;;
;;; Compile the AST into operators for the output state machine.
;;;

(defn- build-prologue
  "Build the prologue which contains all the bindings as we call each
  step in the walker.  We might see duplicate `with` bindings as we
  walk down multiple branches of an `if` expression, so delete those
  here for efficiency."
  [prologue]
  (let [merged (->> prologue
                    (map #(map-vals list %))
                    (apply merge-with concat))
        inits     (->> (map #(hash-map :init-var %1 :init-val %2)
                            (:init-vars merged) (:init-vals merged))
                       (distinct-by :init-var))]
    {:init-vars (map :init-var inits)
     :init-vals (map :init-val inits)}))

(defn- compile-if [op after vars]
  (let [x (:if op)]
    [{:if {:test (:test x)
           :then (compile-ops (concat (:then x) after) vars)
           :else (compile-ops (concat (:else x) after) vars)}}]))

(defn- compile-do [op after vars]
  (compile-ops (concat (:do op) after) vars))

(defn- compile-with [op after vars]
  (let [x (:with op)]
    [{:with {:binding (:binding x)
             :expr (compile-ops (:init x) [])
             :ops (compile-ops after vars)}}]))

(defn- compile-collect [op after vars]
  (let [expr (:collect op)
        v (gensym "collect-")]
    (swap! *op-state* update-in [:collect] (fnil inc 0))
    (swap! *op-state* update-in [:walk] (fnil inc 0))
    (compile-with {:with {:binding v :init (compile-ops [{:expr expr}] [])}}
                  after
                  (conj vars {:add 'cons :var v}))))

(defn- compile-collect-cat [op after vars]
  (let [expr (:collect-cat op)
        v (gensym "cat-")]
    (swap! *op-state* update-in [:collect] (fnil inc 0))
    (swap! *op-state* update-in [:walk] (fnil inc 0))
    (compile-with {:with {:binding v :init (compile-ops [{:expr expr}] [])}}
                  after
                  (conj vars {:add 'concat :var v}))))

(defn- compile-begin [op after vars]
  (swap! *op-state* update-in [:begin] concat (compile-ops (:begin op) []))
  (compile-ops after vars))

(defn- compile-end [op after vars]
  (swap! *op-state* update-in [:end] concat (:end op))
  (compile-ops after vars))

(defn- compile-stop [op vars]
  (let [ends *ends*
        forms [{:stop {:vars vars :expr (:stop op)}}]]
    (if (not ends)
        forms
        (binding [*ends* nil]
          (compile-ops (concat ends [op]) vars)))))

(defn- compile-return [op]
  (swap! *op-state* update-in [:return] (fnil inc 0))
  (swap! *op-state* update-in [:walk] (fnil inc 0))
  [op])

(defn- compile-finally-by [op after vars]
  (swap! *op-state* update-in [:finally-by] (fnil conj #{}) (:finally-by op))
  (compile-ops after vars))

(defn compile-gather [vars]
  (if (empty? vars)
      []
      (do
        (swap! *op-state* update-in [:walk] (fnil inc 0))
        [{:gather vars}])))

(defn compile-prologue [op after vars]
  (swap! *op-state* update-in [:prologue] conj (:prologue op))
  (compile-ops after vars))

(defn- compile-ops [parsed vars]
  (if (empty? parsed)
      (compile-gather vars)
      (let [op (first parsed)
            after (rest parsed)]
        (case (first (keys op))
          :with (compile-with op after vars)
          :if (compile-if op after vars)
          :collect (compile-collect op after vars)
          :collect-cat (compile-collect-cat op after vars)
          :stop (compile-stop op vars)
          :return (compile-return op)
          :finally-by (compile-finally-by op after vars)
          :do (compile-do op after vars)
          :begin (compile-begin op after vars)
          :end (compile-end op after vars)
          :prologue (compile-prologue op after vars)
          (cons op (compile-ops after vars))))))

(defn- end-ops [parsed]
  (binding [*op-state* (atom {})]
    (compile-ops parsed [sentinel])
    (:end @*op-state*)))

(defn- compile-ops-tree [parsed]
  (binding [*ends* (end-ops parsed)
            *op-state* (atom {})]
    (let [ops (compile-ops parsed [sentinel])]
      (when (> (count (:finally-by @*op-state*)) 1)
        (throw (IllegalArgumentException.
                "iter: At most one :finally-by allowed")))
      (when (and (:return @*op-state*) (:collect @*op-state*))
        (throw (IllegalArgumentException.
                "iter: can't mix :collect and :return")))
      (if (or true (:walk @*op-state*))
          [@*op-state* ops]
          [@*op-state* (concat ops [{:gather []}])]))))

;;;
;;; Generate the output state machine.
;;;

(defn- ensure-list [x]
  (if (or (nil? x) (seq? x) (list? x) (vector? x))
      x
      (list x)))

(defn- output-if [test then else walk-recur]
  `(if ~test
       (do
         ~@(or (output (ensure-list then) walk-recur)
               (list walk-recur)))
       (do
         ~@(or (output (ensure-list else) walk-recur)
               (list walk-recur)))))

(defn output-do [forms walk-recur]
  `(do ~@(output forms walk-recur)))

(defn output-collects [vars]
  (map (fn [e] `(~(:add e) ~(:var e)))
       (remove #{sentinel} (reverse vars))))

(defn output-gather [exprs walk-recur]
  `(->> ~walk-recur
        ~@(output-collects exprs)))

(defn output-stop [vars expr]
  (let [e (if (= expr sentinel)
              nil
              [expr])]
    `(->> ~e
          ~@(output-collects vars))))

(defn output-return [expr]
  [expr])

(defn- output-with [binding init ops walk-recur]
  `(let [~binding ~@(output init walk-recur)]
     ~@(output ops walk-recur)))

(defn output-begin [forms]
  (when forms
    `(do ~@(map :expr forms))))

(defn output-finally-by [val finally-by]
  (if finally-by
      `(~(first finally-by) ~val)
      val))

(defn- output-op [op walk-recur]
  (match [op]
    [{:expr expr}]                              expr
    [{:gather exprs}]                           (output-gather exprs walk-recur)
    [{:stop {:vars vars :expr expr}}]           (output-stop vars expr)
    [{:return expr}]                            (output-return expr)
    [{:do forms}]                               (output-do forms walk-recur)
    [{:if {:test test :then then :else else}}]  (output-if test then else walk-recur)
    [{:with {:ops ops, :binding binding
             :expr expr}}]                      (output-with binding expr ops walk-recur)
             :else                                       nil))

(defn- output
  "Generate the output for the walker state machine.  The walker
   creates a lazy sequence and the state machine is what we do for each
   iteration of the walker.

   Ops are the compiled operators and walk-recur is the expression
   used to recursively call the walker."
  [ops walk-recur]
  (seq (map #(output-op % walk-recur) ops)))

;;;
;;; Main entry point
;;;

(defmacro iter
  "An iteration and looping DSL for Clojure.  Iter provides an
  extensible looping language that is an alternate to higher order
  functions.

  For example

      (iter (for-each x [1 2 3 4 5 6 7])
            (when (> x 2)
              (collect (* x x))))

      => (9 16 25 36 49)"
  [& clauses]
  (let [parsed (build-parse-tree clauses)
        [op-state ops] (compile-ops-tree parsed)
        prologue (build-prologue (:prologue op-state))
        res (gensym "res-")
        walk (gensym "walk-")
        walk-recur `(~walk ~@(:init-vars prologue))]
    `(let [~walk (fn ~walk [~@(:init-vars prologue)] ; The lazy-seq walker
                   (lazy-seq
                    ~@(output ops walk-recur)))]
       ~(output-begin (:begin op-state))             ; Any initial steps
       (let [~res (~walk ~@(:seq prologue)           ; Call the walker
                         ~@(:init-vals prologue))]
         ~(output-finally-by (if (:return op-state)  ; Post process the result
                                 `(first ~res)
                                 res)
                             (:finally-by op-state))))))

(defn- dump-iter
  "Print the generated Clojure"
  [form]
  (binding [clojure.pprint/*print-suppress-namespaces* true]
    (-> form
        (macroexpand-1)
        (pprint))))

;;;
;;; Higher order macros.  Now that the compiler is defined we can
;;; extend the langauge with these macros.  We need to be careful
;;; about using Clojure gensym variables - since the macros can be
;;; expanded together, we always use the (gensym) function for vars
;;; rather than var# to avoid any variable collisions.

(def ^:private reduce-var (gensym "r-"))

(define-iter-clause reducing
  [expr & {:keys [by init]}]
  (if init
      `(do
         (collect ~expr)
         (finally-by (fn [~reduce-var] (reduce ~by ~init ~reduce-var))))
      `(do
         (collect ~expr)
         (finally-by (fn [~reduce-var] (reduce ~by ~reduce-var))))))

(define-iter-clause finding
  ([expr]
   (let [finding (gensym "finding-")]
     `(do
        (with ~finding ~expr)
        (when ~finding
          (return ~finding)))))
  ([expr where]
   `(when ~where
      (return ~expr))))

(define-iter-clause collect-map [key value]
  `(reducing [~key ~value] :init {} :by (fn [acc# [k# v#]]
                                          (assoc acc# k# v#))))

(def ^:private seen?-var (gensym "seen?-"))

(define-iter-clause collect-uniq [val]
  (let [x (gensym "x-")]
    `(do
       (with ~x ~val)
       (if (not (~seen?-var ~x))
           (collect ~x))
       (accum ~seen?-var (conj ~seen?-var ~x) #{}))))

(define-iter-clause maximize [x]
  `(reducing ~x :by max))

(define-iter-clause minimize [x]
  `(reducing ~x :by min))

(define-iter-clause sum [x]
  `(reducing ~x :by +))

(define-iter-clause multiply [x]
  `(reducing ~x :by *))

(def ^:private counter-var (gensym "counter-"))

(define-iter-clause counting [x]
  `(do
     (accum ~counter-var (inc ~counter-var) 0)
     (end (return ~counter-var))))

(define-iter-clause for-each [var coll]
  (let [xs (gensym "xs-")]
    `(do
       (if (not ~xs)
           (stop))
       (with ~var (first ~xs))
       (accum ~xs (seq (rest ~xs)) (seq ~coll)))))

(define-iter-clause for-list [var coll]
  (let [xs (gensym "xs-")]
    `(do
       (if (empty? ~xs)
           (stop))
       (with ~var ~xs)
       (accum ~xs (rest ~xs) ~coll))))

(define-iter-clause for-prev
  ([var ovar]
   `(for-prev ~var ~ovar nil))
  ([var ovar initially]
   (let [prev (gensym "prev-")]
     `(do
        (with ~var ~prev)
        (accum ~prev ~ovar ~initially)))))

(define-iter-clause for-next
  ([var next-expr]
   `(for-next ~var ~next-expr nil))
  ([var next-expr init]
   (let [x (gensym "x-")]
     `(do
        (with ~var ~x)
        (accum ~x ~next-expr ~init)))))
