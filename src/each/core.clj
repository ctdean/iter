(ns each.core
  "@ctdean"
  (:require
   [medley.core :refer :all]
   [clojure.core.match :refer [match]]
   [clojure.pprint :refer [pprint]]))

;;; Clause Axioms
;;;   :for
;;;   :collect
;;;   :if
;;;   :initially
;;;   :if-first?
;;;   :let
;;;   :previous
;;;   :stop
;;;   :return / leave
;;;   :finding
;;;   :reducing

;;;   :max
;;;   :finally


;;;
;;; High Level clause
;;;   repeat

(declare build-ops)
(declare output)

(def  ^:dynamic *op-state* nil)
(def first-time? (gensym "first-time-"))

(defn- filter-map [f coll & colls]
  (filter identity
          (apply map f coll colls)))

(defn- single? [xs]
  (when-let [s (seq xs)]
    (empty? (rest s))))

(defn to-pretty-str [x]
  (clojure.string/trim
   (with-out-str
     (pprint x))))

(defn ensure-list
  [x]
  (if (or (nil? x) (seq? x) (list? x) (vector? x))
      x
      (list x)))

(defn check-empty? [x]
  (or (nil? x)
      (and (or (seq? x) (coll? x))
           (empty? x))))

;;;
;;; Parse
;;;

(defn- parse [form]
  (match (vec form)
    [:for var coll]             {:prologue {:seq coll :binding var}}
    [:for var :previous ovar]   {:prev {:binding var :old ovar :init nil}}
    [:for var :previous ovar
              :initially i]     {:prev {:binding var :old ovar :init i}}
    [:collect expr]             {:collect expr}
    [:if test then]             {:if {:test test :then (ensure-list (parse then))
                                      :else nil}}
    [:if test then else]        {:if {:test test :then (ensure-list (parse then))
                                      :else (ensure-list (parse else))}}
    [:if-first? then]           {:if {:test first-time? :then (ensure-list (parse then))
                                      :else nil}}
    [:if-first? then else]      {:if {:test first-time? :then (ensure-list (parse then))
                                      :else (ensure-list (parse else))}}
    [:initially expr]           {:initially expr}
    [:let binding init]         {:let {:binding binding :init (parse init)}}
    [:stop]                     {:stop :now}
    [:return expr]              {:return expr}
    [:finding expr]             {:finding {:expr expr}}
    [:finding expr :where test] {:finding {:expr expr :test test} }
    [:reducing expr :by f]      {:reducing {:expr expr :by f}}
    [:reducing expr :by f
                    :init init] {:reducing {:expr expr :by f :init init}}
    :else                       (if (keyword? (first form))
                                    (throw (IllegalArgumentException.
                                            (format "Unknown each keyword %s => %s"
                                                    (first form) form)))
                                    {:expr form})))

(defn- has-op? [op-name parsed]
  (->> parsed
       (tree-seq coll? #(if (map? %) (vals %) %))
       (filter map?)
       (filter-map op-name)
       (remove #(and (coll? %) (empty? %)))
       (seq)))

(defn- build-prologue [parsed]
  (let [prologue (->> parsed
                      (filter-map :prologue)
                      (map #(map-vals list %))
                      (apply merge-with concat))
        nbindings (count (:binding prologue))]
    (assoc prologue
      :xs-vars (repeatedly nbindings #(gensym "coll-"))
      :seq-vars (repeatedly nbindings #(gensym "s-")))))

(defn- push-if [op after vars]
  (let [x (:if op)]
    [{:if {:test (:test x)
           :then (build-ops (concat (:then x) after) vars)
           :else (build-ops (concat (:else x) after) vars)}}]))

(defn- push-let [op after vars]
  (let [x (:let op)]
    [{:let {:binding (:binding x)
            :init (build-ops (ensure-list (:init x)) [])
            :ops (build-ops after vars) }}]))

(defn- push-collect [op after vars]
  (let [expr (:collect op)
        v (gensym "collect-")]
    (swap! *op-state* update-in [:collect] (fnil inc 0))
    (push-let {:let {:binding v :init {:expr expr}}}
              after
              (conj vars v))))

(defn- push-finding [op after vars]
  (let [f (:finding op)
        v (gensym "finding-")
        forms (if (:test f)
                  [{:if {:test (:test f) :then [{:return (:expr f)}]}}]
                  [{:let {:binding v :init {:expr (:expr f)}}}
                   {:if {:test v :then [{:return v}]}}])]
    (build-ops (concat forms after) vars)))

(defn- push-reducing [op after vars]
  (let [r (:reducing op)
        v (gensym "finding-")
        forms [{:collect (:expr r)}]]
    (swap! *op-state* update-in [:reducing] conj r)
    (build-ops (concat forms after) vars)))

(defn- push-return [op]
  (swap! *op-state* update-in [:return] (fnil inc 0))
  [op])

(defn- build-ops [parsed vars]
  (if (empty? parsed)
      [{:accum vars}]
      (let [op (first parsed)
            after (rest parsed)]
        (case (first (keys op))
          :let (push-let op after vars)
          :if (push-if op after vars)
          :collect (push-collect op after vars)
          :finding (push-finding op after vars)
          :stop [{:stop vars}]
          :return (push-return op)
          :reducing (push-reducing op after vars)
          :prologue (build-ops after vars)
          (cons op (build-ops after vars))))))

(defn- build-ops-tree [parsed]
  (binding [*op-state* (atom {})]
   (let [ops (build-ops parsed [])]
     ;; (printf "--- ops %s\n" (to-pretty-str ops))
     (when (and (:reducing @*op-state*) (> (:collect @*op-state*) 1))
       (throw (IllegalArgumentException.
               (format "each: can't mix :collect and :reducing"))))
     (when (and (:return @*op-state*) (:collect @*op-state*))
       (throw (IllegalArgumentException.
               (format "each: can't mix :collect and :return"))))
     [@*op-state* ops])))

;;;
;;; output
;;;

(defn- output-if [test then else data]
  `(if ~test
       (do
         ~@(or (output (ensure-list then) data)
               (list data)))
       (do
         ~@(or (output (ensure-list else) data)
               (list data)))))

(defn output-accum [exprs data]
  (when (seq exprs)
    `(->> ~data
          ~@(map (fn [e] `(cons ~e))
                 (reverse exprs)))))

(defn output-stop [exprs]
  `(->> nil
        ~@(map (fn [e] `(cons ~e))
               (reverse exprs))))

(defn output-return [expr]
  [expr])

(defn- output-let [binding init ops data]
  `(let [~binding ~@(output init data)]
     ~@(output ops data)))

(defn output-reducing [coll reducing]
  (let [output-one (fn [r]
                     (if (:init r)
                         `(reduce ~(:by r) ~(:init r))
                         `(reduce ~(:by r))))]
    `(->> ~coll
          ~@(map (fn [r]
                   (if (:init r)
                       `(reduce ~(:by r) ~(:init r))
                       `(reduce ~(:by r))))
                 reducing))))

(defn- output-op [op data]
  (match [op]
    [{:expr expr}]                              expr
    [{:accum exprs}]                            (output-accum exprs data)
    [{:stop exprs}]                             (output-stop exprs)
    [{:return expr}]                            (output-return expr)
    [{:if {:test test :then then :else else}}]  (output-if test then else data)
    [{:let {:ops ops, :binding binding,
            :init init}}]                       (output-let binding init ops data)
    :else                                       nil))

(defn- output [ops data]
  (seq (filter-map #(output-op % data) ops)))

;;;
;;; Main entry point
;;;

(defmacro each [& clauses]
  (let [parsed (map parse clauses)
        prologue (build-prologue parsed)
        [op-state ops] (build-ops-tree parsed)
        prevs (filter-map :prev parsed)
        data (gensym "data-")
        res (gensym "res-")]
    `(let [walk# (fn walk# [~first-time?
                            ~@(:xs-vars prologue)
                            ~@(map :binding prevs)]
                   (lazy-seq
                    (let [~@(mapcat (fn [s c] `(~s (seq ~c)))
                                    (:seq-vars prologue)
                                    (:xs-vars prologue))]
                      (when (and ~@(:seq-vars prologue))
                        (let [~@(mapcat (fn [b s] `(~b (first ~s)))
                                        (:binding prologue) (:seq-vars prologue))
                              ~data (walk# false
                                           ~@(map (fn [s] `(rest ~s))
                                                  (:seq-vars prologue))
                                           ~@(map :old prevs))]
                          ~@(output ops data))))))]
       ~@(filter-map :initially parsed)
       (let [~res (walk# true
                         ~@(:seq prologue)
                         ~@(map :init prevs))]
         ~(cond
           (:return op-state) `(first ~res)
           (:reducing op-state) (output-reducing res (:reducing op-state))
           :else res)))))

(defn foo []
  (binding [clojure.pprint/*print-suppress-namespaces* true]
    (-> '(each (:for x (range 5))
               (:reducing x :by + :init 3))
        (macroexpand-1)
        (pprint))))

(defn foo2 []
  (binding [clojure.pprint/*print-suppress-namespaces* true]
    (-> '(each (:for x (range 10))
               (:finding x))
        (macroexpand-1)
        (pprint))))

(defn foo3 []
  (binding [clojure.pprint/*print-suppress-namespaces* true]
    (-> '(each (:for x (range 10))
               (:let y (:if (> x 4)
                            (* x x x)))
               (:finding y))

        (macroexpand-1)
        (pprint))))

(defn xfoo []
  (let [walk (fn walk [coll]
               (lazy-seq
                (let [s (seq coll)]
                  (when (and s)
                    (let [x (first s)
                          data (walk (rest s))]
                      (throw (Exception. "77"))
                      (let [y (* x x)]
                        (cons y data)))))))]
    (try
      (walk (range 10))
      (catch Exception e
        (str "got " e)
        )
      )))
