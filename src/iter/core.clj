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
  (:require
   [medley.core :refer [map-vals distinct-by]]
   [clojure.core.match :refer [match]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   iter.macros))

(declare compile-ops)
(declare output)
(declare maybe-expand)
(declare build-parse-tree)

;; Automatically lookup iter macros in these namespaces
(defonce macro-namespaces (atom #{"iter.macros"}))

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
    [:return expr]                      {:return expr}
    [:stop]                             {:stop Integer/MAX_VALUE}
    [:break]                            {:stop 1}
    [:break nlevels]                    {:stop nlevels}
    [:continue]                         {:stop -1}
    [:continue nlevels]                 {:stop (- nlevels)}
    [:do & forms]                       {:do (build-parse-tree forms)}
    [:finally-by f]                     {:finally-by f}
    :else                               (maybe-expand form)))

(defn- safe-find-var [x]
  (try
    (find-var (symbol x))
    (catch java.lang.IllegalArgumentException e
      nil)))

(defn- iter-macros [name]
  (filter #(:iter-op (meta %))
          (let [[pkg base] (rest (re-find #"^(.+)/(.+)$" (str name)))]
            (if pkg
                [(safe-find-var name)]
                (map #(safe-find-var (str % "/" name))
                     (cons (ns-name *ns*) @macro-namespaces))))))

(defn- maybe-expand
  "If this is an iter macro, then manually expand the form and
   recursively parse it."
  [form]
  (let [name (first form)]
    (if (keyword? name)
        (throw (IllegalArgumentException.
                (format "Unknown iter keyword %s => %s" name form)))
        (let [mnames (iter-macros name)]
          (if (> (count mnames) 1)
              (throw (IllegalArgumentException.
                      (format "Multiple definitions for iter keyword %s => %s"
                              name mnames)))
              (if (seq mnames)
                  {:do (build-parse-tree [(macroexpand-1 (cons (first mnames)
                                                               (rest form)))])}
                  {:expr form}))))))

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
  [op])

(defn- compile-return [op]
  (swap! *op-state* update-in [:return] (fnil inc 0))
  [op])

(defn- compile-finally-by [op after gather]
  (swap! *op-state* update-in [:finally-by] (fnil conj #{}) (:finally-by op))
  (compile-ops after gather))

(defn- compile-gather [gather-vars]
  (if (empty? gather-vars)
      []
      [{:gather gather-vars}]))

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

(defn- output-stop-all [header]
  (if-let [fn-name (:exit-fn header)]
    `(~fn-name ~@(output-wrapper-vals header))
    (gather-sym 0)))

(defn- output-stop [nlevels header]
  (if (neg? nlevels)
      (output-stop (dec (- nlevels)) header)
      (let [depth (:depth header)
            target (- depth nlevels)]
        (if (neg? target)
            (output-stop-all header)
            (let [step (step-sym target)]
              `(~step ~@(concat (take target (:fornext-var header))
                                [(nth (:fornext-incr header) target nil)]
                                (repeat (count (drop (inc target)
                                                     (:fornext-var header))) nil)
                                (:accum-var header)
                                [(gather-sym target)])))))))

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
    [{:stop nlevels}]                           (output-stop nlevels header)
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

(defn- iter-expand [clauses use-dorun?]
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
                               (if use-dorun?
                                   `(dorun ~res)
                                   `(seq ~res)))
                           (:finally-by op-state)))))

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
  (iter-expand clauses false))

(defmacro iter!
  "Just like iter, but used for side effects.  Evaluates as if `iter`
   was surrounding in a doall.  This causes the entire return
   expression to be contained in memory."
  [& clauses]
  (iter-expand clauses true))

(defmacro iter*
  "Just like iter!  DEPRECATED"
  [& clauses]
  (iter-expand clauses true))

(defn- dump-iter
  "Print the generated Clojure of an iter form."
  [form]
  (binding [clojure.pprint/*print-suppress-namespaces* true]
    (-> form
        (macroexpand-1)
        (pprint))))

(defmacro define-iter-op
  "Define an iter macro.  The only difference between iter macros and
  regular macros is that iter macros are recursively parsed by iter."
  [iname & args-body]
  `(iter.macros/define-iter-op ~iname ~@args-body))
