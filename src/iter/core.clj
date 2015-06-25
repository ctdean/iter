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
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]))

(def registered-macros (atom {}))

(declare compile-ops)
(declare output)
(declare maybe-expand)
(declare build-parse-tree)

;; The state of the compiled operators
(def ^{:dynamic true :private true} *op-state* nil)

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
    [:fornext var incr init done?]      {:fornext {:var var :incr incr :init init
                                                   :done? done?}}
    [:with var expr]                    {:with {:binding var :init [{:expr expr}]}}
    [:accum var expr init]              {:with {:binding var :init [{:expr expr}]}
                                         :prologue {:accum-var var :accum-init init}}
    [:if test then]                     {:if {:test test :then (build-parse-tree [then])
                                              :else nil}}
    [:if test then else]                {:if {:test test :then (build-parse-tree [then])
                                              :else (build-parse-tree [else])}}
    [:begin & forms]                    {:begin (build-parse-tree forms)}
    [:end & forms]                      {:end (build-parse-tree forms)}
    [:stop]                             {:stop sentinel}
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

(defmacro define-iter-op
  "Define an iter macro.  The only difference between iter macros and
  regular macros is that iter macros are recursively parsed by iter.

  We register the name of the iter macro so that we know to keep
  parsing.  The macro might be called using the plain or fully
  qualified name, so we register both names."
  [iname & args-body]
  (reg-macro iname iname)
  `(defmacro ~iname ~@args-body))

;; Macro versions of the builtin iter keywords.
(define-iter-op collect [expr]          `(:collect ~expr))
(define-iter-op collect-cat [expr]      `(:collect-cat ~expr))
(define-iter-op do [& body]             `(:do ~@body))
(define-iter-op if [test & then-else]   `(:if ~test ~@then-else))
(define-iter-op begin [& body]          `(:begin ~@body))
(define-iter-op return [expr]           `(:return ~expr))
(define-iter-op stop []                 `(:stop))
(define-iter-op end [& body]            `(:end ~@body))
(define-iter-op accum [var expr init]   `(:accum ~var ~expr ~init))
(define-iter-op with [var expr]         `(:with ~var ~expr))
(define-iter-op finally-by [f]          `(:finally-by ~f))

;; We need to define this here so we can use use WHEN in our normal
;; clojure code below.
(define-iter-op when [test & body]
  `(if ~test
       (do ~@body)))

;;;
;;; Compile the AST into operators for the output state machine.
;;;

(def ^:private step-sym (memoize
                         (fn [depth]
                           (when (>= depth 0)
                             (gensym (str "step-" depth "-"))))))

(def ^:private gather-sym-var (gensym (str "gather-")))
(defn- gather-sym [depth]
  (if (< depth 0)
      []
      gather-sym-var))

(defn- compile-if [op after gather]
  (let [x (:if op)]
    [{:if {:test (:test x)
           :then (compile-ops (concat (:then x) after) gather)
           :else (compile-ops (concat (:else x) after) gather)}}]))

(defn- compile-do [op after gather]
  (compile-ops (concat (:do op) after) gather))

(defn- compile-with [op after gather]
  (let [x (:with op)]
    [{:with {:binding (:binding x)
             :expr (compile-ops (:init x) [])
             :ops (compile-ops after gather)}}]))

(defn- compile-collect [op after gather]
  (let [expr (:collect op)
        v (gensym "collect-")
        g (gather-sym (dec (:fornext-depth @*op-state* 0)))]
    (swap! *op-state* update-in [:collect] (fnil inc 0))
    (compile-ops
     (concat [{:with {:binding v :init (compile-ops [{:expr expr}] [])}}]
             [{:with {:binding g :init (compile-ops [{:expr `(conj ~g ~v)}] [])}}]
             after)
     (conj gather g))))

(defn- compile-collect-cat [op after gather]
  (let [expr (:collect-cat op)
        v (gensym "cat-")
        g (gather-sym (dec (:fornext-depth @*op-state* 0)))]
    (swap! *op-state* update-in [:collect] (fnil inc 0))
    (compile-ops
     (concat [{:with {:binding v :init (compile-ops [{:expr expr}] [])}}]
             [{:with {:binding g :init (compile-ops [{:expr `(into ~g ~v)}] [])}}]
             after)
     (conj gather g))))

(defn- compile-fornext [op after gather]
  (let [next (:fornext op)
        pro {:prologue {:fornext-var (:var next)
                        :fornext-incr (:incr next)
                        :fornext-init (:init next)}}
        depth (:fornext-depth @*op-state* 0)]
    (swap! *op-state* update-in [:fornext-depth] (fnil inc 0))
    [{:fornext {:payload next
                :depth depth
                :forms (compile-ops (cons pro after) (conj gather sentinel))}}]))

(defn- compile-begin [op after gather]
  (swap! *op-state* update-in [:begin] concat (:begin op))
  (doall (compile-ops (:begin op) [])) ; force compile for any side effects
  (compile-ops after gather))

(defn- compile-end [op after gather]
  (swap! *op-state* update-in [:end] concat (:end op))
  (doall (compile-ops (:end op) [])) ; force compile for any side effects
  (compile-ops after gather))

(defn- compile-stop [op]
  [{:stop sentinel}])

(defn- compile-return [op]
  (swap! *op-state* update-in [:return] (fnil inc 0))
  [op])

(defn- compile-finally-by [op after gather]
  (swap! *op-state* update-in [:finally-by] (fnil conj #{}) (:finally-by op))
  (compile-ops after gather))

(defn- compile-gather [gather]
  (if (empty? gather)
      []
      [{:gather gather}]))

(defn- compile-prologue [op after gather]
  (swap! *op-state* update-in [:prologue] (fnil conj []) (:prologue op))
  (compile-ops after gather))

(defn- compile-ops [parsed gather]
  (if (empty? parsed)
      (compile-gather gather)
      (let [op (first parsed)
            after (rest parsed)]
        (case (first (keys op))
          :with (compile-with op after gather)
          :if (compile-if op after gather)
          :collect (compile-collect op after gather)
          :collect-cat (compile-collect-cat op after gather)
          :fornext (compile-fornext op after gather)
          :stop (compile-stop op)
          :return (compile-return op)
          :finally-by (compile-finally-by op after gather)
          :do (compile-do op after gather)
          :begin (compile-begin op after gather)
          :end (compile-end op after gather)
          :prologue (compile-prologue op after gather)
          (cons op (compile-ops after gather))))))

(defn- check-validity! [state]
  (when (> (count (:finally-by state)) 1)
    (throw (IllegalArgumentException.
            "iter: At most one :finally-by allowed")))
  (when (and (:return state) (:collect state))
    (throw (IllegalArgumentException.
            "iter: can't mix :collect and :return")))
  (when-let [dups (->> (:prologue state)
                       (filter :accum-init)
                       (distinct)
                       (group-by :accum-var)
                       (filter (fn [[k vs]] (> (count vs) 1)))
                       (map first)
                       (seq))]
    (throw (IllegalArgumentException.
            (format "iter: An accum var must be initialzed to the same value: %s"
                    (string/join ", " dups))))))

(defn- compile-ops-tree [parsed]
  (binding [*op-state* (atom {})]
    (let [ops (compile-ops parsed #{sentinel})
          begin (when-let [p (:begin @*op-state*)]
                  (compile-ops p #{sentinel}))
          end (when-let [p (:end @*op-state*)]
                (compile-ops p #{sentinel}))]
      (check-validity! @*op-state*)
      [(assoc @*op-state* :begin begin :end end)
       ops])))

(defn- build-header
  "Build the header which contains all the bindings as we call each
  step in the stepper.  We might see duplicate `accum` bindings as we
  walk down multiple branches of an `if` expression, so delete those
  here for efficiency."
  [prologue]
  (let [merged (->> prologue
                    (map #(map-vals list %))
                    (apply merge-with concat))
        accums (->> (map #(hash-map :accum-var %1 :accum-init %2)
                         (:accum-var merged) (:accum-init merged))
                    (distinct-by :accum-var))]
    (assoc merged
           :depth 0
           :accum-var (map :accum-var accums)
           :accum-init (map :accum-init accums))))

;;;
;;; Generate the output state machine.
;;;

(defn- ensure-list [x]
  (if (or (nil? x) (seq? x) (list? x) (vector? x))
      x
      (list x)))

(defn- output-if [test then else header]
  `(if ~test
       (do
         ~@(or (output (ensure-list then) header)
               (list header)))
       (do
         ~@(or (output (ensure-list else) header)
               (list header)))))

(defn- output-do [forms header]
  (when (seq forms)
    `(do ~@(output forms header))))

(defn- output-step-args [header]
  (concat (:fornext-var header)
          (:accum-var header)
          [(gather-sym 0)]))

(defn- output-step-vals [header]
  (let [depth (:depth header)]
    (concat (take depth (:fornext-var header))
            [(nth (:fornext-init header) depth nil)]
            (repeat (count (drop (inc depth) (:fornext-var header))) nil)
            (:accum-var header)
            [(gather-sym (dec depth))])))

(defn- output-wrapper-args [header]
  (concat (:accum-var header) [(gather-sym 0)]))

(defn- output-wrapper-vals [header]
  (concat (:accum-var header) [(gather-sym 0)]))

(defn- output-wrapper-vals-init [header]
  (concat (:accum-init header) [(gather-sym -1)]))

(defn- output-parent-recur [header]
  (if (zero? (:depth header))
      (if-let [fn-name (:exit-fn header)]
        `(~fn-name ~@(output-wrapper-vals header))
        (gather-sym 0))
      (let [depth (:depth header)
            parent (dec depth)
            gparent (dec parent)
            parent-step (step-sym parent)]
        `(~parent-step ~@(concat (take parent (:fornext-var header))
                                 [(nth (:fornext-incr header) parent nil)]
                                 (repeat (count (drop (inc parent)
                                                      (:fornext-var header))) nil)
                                 (:accum-var header)
                                 [(gather-sym parent)])))))

(defn- output-fornext [next depth forms header]
  (let [step (step-sym depth)
        header (assoc header :depth depth)]
    `(let [~step (fn ~step [~@(output-step-args header)]
                   (lazy-seq
                    (if ~(:done? next)
                        ~(output-parent-recur header)
                        (do
                          ~@(output forms header)))))]
       (~step ~@(output-step-vals header)))))

(defn- output-gather [vars header]
  (let [fn-name (:gather-fn header)
        step (step-sym (dec (count (:fornext-var header))))
        no-vars? (empty? (remove #{sentinel} vars))
        oform `(~step ~@(concat (drop-last (:fornext-var header))
                                [(last (:fornext-incr header))]
                                (:accum-var header)
                                [(gather-sym -1)]))]
    (cond
      (= fn-name :done) (gather-sym 0)
      fn-name `(~fn-name ~@(output-wrapper-vals header))
      :else (when step
              (if no-vars?
                  oform
                  `(concat ~(gather-sym (:depth header)) ~oform))))))

(defn- output-stop [header]
  (if-let [fn-name (:exit-fn header)]
    `(~fn-name ~@(output-wrapper-vals header))
    (gather-sym 0)))

(defn- output-return [expr header]
  `(let [~(gather-sym 0) [~expr]]
     ~(if-let [fn-name (:exit-fn header)]
        `(~fn-name ~@(output-wrapper-vals header))
        (gather-sym 0))))

(defn- output-with [binding init ops header]
  `(let [~binding ~@(output init header)]
     ~@(output ops header)))

(defn- output-begin [forms]
  (when forms
    `(do ~@(map :expr forms))))

(defn- output-finally-by [val finally-by]
  (if finally-by
      `(~(first finally-by) ~val)
      val))

(defn- output-wrapper [fn-name ops header]
  (when fn-name
    `(~fn-name (fn [~@(output-wrapper-args header)]
                 ~@(output ops header)))))

(defn- output-op [op header]
  (match [op]
    [{:expr expr}]                              expr
    [{:gather vars}]                            (output-gather vars header)
    [{:stop sentinel}]                          (output-stop header)
    [{:return expr}]                            (output-return expr header)
    [{:do forms}]                               (output-do forms header)
    [{:if {:test test :then then :else else}}]  (output-if test then else header)
    [{:fornext
      {:payload next :depth d :forms f}}]       (output-fornext next d f header)
    [{:with {:ops ops :binding binding
             :expr expr}}]                      (output-with binding expr ops header)
    :else                                       nil))

(defn- output
  "Generate the output for the stepper state machine.  The stepper
   creates a lazy sequence and the state machine is what we do for each
   iteration of the stepper.

   Ops are the compiled operators and header is needed to
   recursively call the stepper."
  [ops header]
  (seq (map #(output-op % header) ops)))


;;;
;;; Main entry point
;;;

(defmacro iter
  "An iteration and looping DSL for Clojure.  Iter provides an
  extensible looping language that is an alternate to higher order
  functions.

  For example

      (iter (foreach x [1 2 3 4 5 6 7])
            (when (> x 2)
              (collect (* x x))))

      => (9 16 25 36 49)"
  [& clauses]
  (let [parsed (build-parse-tree clauses)
        [op-state ops] (compile-ops-tree parsed)
        header (build-header (:prologue op-state))
        [res master-fn] (map gensym ["res-" "master-"])
        begin-fn (when (:begin op-state) (gensym "begin-"))
        end-fn (when (:end op-state) (gensym "end-"))]
    `(let [~@(output-wrapper end-fn (:end op-state) (assoc header :gather-fn :done))
           ~@(output-wrapper master-fn ops (assoc header :exit-fn end-fn))
           ~@(output-wrapper begin-fn (:begin op-state)
                             (assoc header :exit-fn master-fn :gather-fn master-fn))
           ~res (~(or begin-fn master-fn) ~@(output-wrapper-vals-init header))]
       ~(output-finally-by (if (:return op-state) ; Post process the result
                               `(first ~res)
                               `(seq ~res))
                           (:finally-by op-state)))))

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
;;; expanded together, we use the (gensym) function for vars rather
;;; than var# to avoid any variable collisions.

(def ^:private reduce-var (gensym "r-"))

(define-iter-op foreach
  ([var coll & colls]
   `(foreach ~var (map vector ~coll ~@colls)))
  ([var coll]
    (let [xs (gensym (str var "-s-"))]
      `(do
         (:fornext ~xs (rest ~xs) ~coll (empty? ~xs))
         (with ~var (first ~xs))))))

(define-iter-op forlist [var coll]
  (let [xs (gensym (str var "-s-"))]
    `(do
       (:fornext ~xs (rest ~xs) ~coll (empty? ~xs))
       (with ~var ~xs))))

(define-iter-op with-prev
  ([var old-var]
   `(with-prev ~var ~old-var nil))
  ([var old-var init]
   (let [prev (gensym "prev-")]
     `(do
        (with ~var ~prev)
        (accum ~prev ~old-var ~init)))))

(define-iter-op with-first [var]
  `(with-prev ~var false true))

(define-iter-op fornext
  ([var next-expr]
   `(fornext ~var ~next-expr nil))
  ([var next-expr init]
   `(fornext ~var ~next-expr ~init false))
  ([var next-expr init done?]
   `(:fornext ~var ~next-expr ~init ~done?)))

(define-iter-op reducing [expr & {:keys [by init finally-by]}]
  (assert by "reducing: missing :by keyword")
  (if init
      `(do
         (collect ~expr)
         (:finally-by (fn [~reduce-var]
                        ~(if finally-by
                             `(~finally-by (reduce ~by ~init ~reduce-var))
                             `(reduce ~by ~init ~reduce-var)))))
      `(do
         (collect ~expr)
         (:finally-by (fn [~reduce-var]
                        ~(if finally-by
                             `(~finally-by (reduce ~by ~reduce-var))
                             `(reduce ~by ~reduce-var)))))))

(define-iter-op finding
  ([expr]
   (let [finding (gensym "finding-")]
     `(do
        (with ~finding ~expr)
        (when ~finding
          (return ~finding)))))
  ([expr where]
   `(when ~where
      (return ~expr))))

(define-iter-op collect-map [key value]
  `(reducing [~key ~value] :init {} :by (fn [acc# [k# v#]]
                                          (assoc acc# k# v#))))

(def ^:private seen?-var (gensym "seen?-"))

(define-iter-op collect-uniq [val]
  (let [x (gensym "x-")]
    `(do
       (with ~x ~val)
       (if (not (~seen?-var ~x))
           (collect ~x))
       (accum ~seen?-var (conj ~seen?-var ~x) #{}))))

(define-iter-op maximizing [x & {:keys [using]}]
  (if using
      `(reducing [~using ~x] :by (partial max-key first) :finally-by second)
      `(reducing ~x :by max)))

(define-iter-op minimizing [x & {:keys [using]}]
  (if using
      `(reducing [~using ~x] :by (partial min-key first) :finally-by second)
      `(reducing ~x :by min)))

(define-iter-op summing [x]
  `(reducing ~x :by +))

(define-iter-op multiplying [x]
  `(reducing ~x :by *))

(def ^:private counter-var (gensym "counter-"))

(define-iter-op counting [x]
  `(do
     (when ~x
       (accum ~counter-var (inc ~counter-var) 0))
     (end (return ~counter-var))))

(define-iter-op times
  ([how-many]
   (let [n (gensym "n-")]
     `(:fornext ~n (dec ~n) ~how-many (<= ~n 0))))
  ([]
   (let [n (gensym "n-")]
     `(:fornext ~n nil nil false))))
