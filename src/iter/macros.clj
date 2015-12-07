(ns iter.macros
  "Iter macros.
  @ctdean"
  (:refer-clojure :exclude [when while let]))

;;;
;;; Shadow clojure builtins
;;;

(defmacro xlet [& forms] `(clojure.core/let ~@forms))
(defmacro xwhen [& forms] `(clojure.core/when ~@forms))

;;;
;;; Define the iter macros
;;;

(defmacro define-iter-op
  "Define an iter macro.  The only difference between iter macros and
  regular macros is that iter macros are recursively parsed by iter.

  We tag the iter macro so that we know to recursively parse the
  form."
  [iname & args-body] `(defmacro ~iname
  {:iter-op true} ~@args-body))

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
(define-iter-op do [& body]             `(:do ~@body))

(define-iter-op break
  ([] `(:break))
  ([nlevels] `(:break ~nlevels)))

(define-iter-op continue
  ([] `(:continue))
  ([nlevels] `(:continue ~nlevels)))

;;;
;;; Higher order macros.  We need to be careful about using Clojure
;;; gensym variables - since the macros can be expanded together, we
;;; use the (gensym) function for vars rather than var# to avoid any
;;; variable collisions.

(def ^:private reduce-var (gensym "r-"))

(define-iter-op foreach
  ([var coll & colls]
   `(foreach ~var (map vector ~coll ~@colls)))
  ([var coll]
    (xlet [xs (gensym (str var "-s-"))]
      `(do
         (:fornext ~xs (rest ~xs) ~coll (empty? ~xs))
         (with ~var (first ~xs))))))

(define-iter-op forlist [var coll]
  (xlet [xs (gensym (str var "-s-"))]
    `(do
       (:fornext ~xs (rest ~xs) ~coll (empty? ~xs))
       (with ~var ~xs))))

(define-iter-op with-prev
  ([var old-var]
   `(with-prev ~var ~old-var nil))
  ([var old-var init]
   (xlet [prev (gensym "prev-")]
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

(define-iter-op forever []
  (xlet [n (gensym "n-")]
    `(:fornext ~n nil nil false)))

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
   (xlet [finding (gensym "finding-")]
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
  (xlet [x (gensym "x-")]
    `(do
       (with ~x ~val)
       (if (not (~seen?-var ~x))
           (collect ~x))
       (accum ~seen?-var (conj ~seen?-var ~x) #{}))))

(define-iter-op collect-freq [val]
  `(do
     (collect ~val)
     (finally-by frequencies)))

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
   (xlet [n (gensym "n-")]
     `(:fornext ~n (dec ~n) ~how-many (<= ~n 0))))
  ([]
   (xlet [n (gensym "n-")]
     `(:fornext ~n nil nil false))))

(define-iter-op while [test]
  `(when (not ~test)
     (break)))

(define-iter-op when [test & body]
  `(if ~test
       (do ~@body)))

(define-iter-op let [bindings & body]
  (assert (vector? bindings) "iter: let requires a vector for its binding")
  (assert (even? (count bindings))
          "iter: let requires an even number of forms in binding vector")
  (if (= (count bindings) 0)
      `(do ~@body)
      `(do
         (with ~@(subvec bindings 0 2))
         (let ~(subvec bindings 2)
           ~@body))))
