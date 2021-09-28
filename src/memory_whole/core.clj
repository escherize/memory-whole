(ns memory-whole.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.repl :as repl]
            [memory-whole.memory :as mem]
            [clojure.java.io :as io]))

(def one mem/one)
(def many mem/many)
(def select mem/select)

(def ignored-form? '#{def quote var try monitor-enter monitor-exit assert})

(defn balanced-to-idx
  [s]
  ;; todo ignore \{ \} sort of characters
  (let [openings #{ \{ \( \[ \" } closes #{\" \] \) \}}
        close->open {\] \[ \) \( \} \{ \" \"}]
    (:idx (reduce
           (fn [{:keys [stack idx] :as _} [char lookahead]]
             (cond
               (and (= char \")
                    (not= lookahead \\)) (if (= \" (peek stack))
                             {:stack (pop stack) :idx (inc idx)}
                             {:stack (conj stack char) :idx (inc idx)})
               (openings char) {:stack (conj stack char) :idx (inc idx)}
               (closes char) (if (= (peek stack) (close->open char))
                               (let [new-stack (pop stack)]
                                 (if (= [] new-stack)
                                   (reduced {:stack [] :idx (inc idx)})
                                   {:stack new-stack :idx (inc idx)}))
                               (throw (Exception. "unbalanced? idk")))
               :else {:stack stack :idx (inc idx)}))
           {:stack [] :idx 0}
           s))))

(defn trim-source [s]
  (when s
    (str/trim (with-out-str (pprint s)))))

(defn read-source-by-hand [text line column]
  (let [rest-of-file (->> text
                          str/split-lines
                          (drop (dec line)) ;; seek lines
                          (str/join "\n")
                          (drop (dec column)))]
    (trim-source
     (or
      (when-let [form
                 (try (read-string (apply str rest-of-file))
                      (catch Throwable _ nil))]
        form)

      (let [idx-to-read-to (balanced-to-idx rest-of-file)]
        (read-string (apply str (take idx-to-read-to rest-of-file))))))))

(defn find-source [fn-symbol]
  (try
    (if-let [o (repl/source-fn fn-symbol)]
      (trim-source (read-string o))
      (when-let [var (resolve fn-symbol)]
        (let [{:keys [line column file fn-body]} (meta var)]
          (cond
            fn-body
            (trim-source fn-body)

            ;; careful this freaking thing acts weird depending on who calls it
            (slurp file)
            (read-source-by-hand (slurp file) line column)

            ;; careful this freaking thing acts weird depending on who calls it
            (io/resource file)
            (read-source-by-hand (io/resource file) line column)

            :else
            nil))))
    (catch Exception e
      (pprint {::notice "Please report issues with source code lookups."
               ::sym fn-symbol
               ::var (resolve fn-symbol)
               ::exception (str (ex-message e) " : " (ex-cause e))
               ::meta (meta (resolve fn-symbol))})
      (throw e))))

(defn var->ns-symbol [v]
  (when v
    (apply str (drop 2 (pr-str v)))))

(def fmt (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss"))

(defn start-trace!
  "Returns id"
  [name _f args]
  (let [var (resolve name)
        {:keys [fn-body fn-name fn-form] :as var-meta} (meta var)
        source (try (find-source name)
                    (catch Throwable _ nil))]

    ;; (println "start-trace!")
    ;; (println "name" (pr-str name))
    ;; (println "var"  (pr-str var))
    ;; (println "\nvar-meta")
    ;; (pprint var-meta)
    ;; (println "\ninsert-start!")

    ;; (pprint {:name (str name)
    ;;          :full_name (var->ns-symbol var)
    ;;          :start_time (System/currentTimeMillis)
    ;;          :arguments (vec args)
    ;;          :arg_lists (:arg_lists var-meta)
    ;;          :file (:file var-meta)
    ;;          :line (:line var-meta)
    ;;          :column (:column var-meta)
    ;;          :source source
    ;;          :ast_hash (hash [fn-name fn-form])})

    (mem/insert-start!
     {:name (str name)
      :full_name (var->ns-symbol var)
      :start_time (System/currentTimeMillis)
      :arguments (vec args)
      :arg_lists (:arg-lists var-meta)
      :file (:file var-meta)
      :line (:line var-meta)
      :column (:column var-meta)
      :source source
      :ast_hash (hash [fn-name fn-form])})))

(defn end-trace!
  [id output]
  (mem/insert-end! id
                   {:end_time (System/currentTimeMillis)
                    :output output}))

(defn throw-trace! [id exception]
  (mem/insert-thrown! id
                      {:end_time (System/currentTimeMillis)
                       :exception (pr-str exception)}))

(defn trace-fn-call
  "Traces a single call to a function f with args. 'name' is the symbol name of the function."
  [name f args]
  (let [id (start-trace! name f args)]
    (try (let [output (apply f args)]
           (end-trace! id output)
           output)
      (catch Throwable thrown
        (do (throw-trace! id thrown)
            ;; rethrow
            (throw thrown))))))
(defmacro deftrace
  "Use in place of defn; traces each call/return of this fn, including
   arguments. Nested calls to deftrace'd functions will print a
   tree-like structure.
   The first argument of the form definition can be a doc string"
  [name & definition]
  (let [doc-string (if (string? (first definition)) (first definition) "")
        fn-form (if (string? (first definition)) (rest definition) definition)]
;;    (println fn-form)
    `(do
       (declare ~name)
       (let [f# (fn ~@fn-form)
             v# (defn ~name ~doc-string [& args#]
                  (trace-fn-call '~name f# args#))]
         ;; (alter-meta! v# assoc ::fn-body (trim-source (list*
         ;;                                              (symbol "defn")
         ;;                                              '~name
         ;;                                              '~fn-form)))
         ;; (alter-meta! v# assoc ::fn-name (trim-source '~name))
         (alter-meta! v# assoc ::fn-form (trim-source '~fn-form))
         v#))))

(comment
  (deftrace f [x] x)
  (f 1)
  (deftrace g ([] 2) ([x] x))
  (g))

(defmacro dotrace
  "Given a sequence of function identifiers, evaluate the body
expressions in an environment in which the identifiers are bound to
the traced functions. Does not work on inlined functions,
such as clojure.core/+"
  [fnames & exprs]
  `(binding [~@(interleave fnames
                           (for [fname fnames]
                             `(let [f# @(var ~fname)]
                                (fn [& args#]
                                  (trace-fn-call '~fname f# args#)))))]
     ~@exprs))

(declare trace-form)
(defmulti trace-special-form (fn [form] (first form)))

(defn ^{:private true} trace-bindings
  "Trace the forms in the given binding list."
  [bindings]
  (vec (apply concat
              (map (fn [[sym value]]
                     `[~sym (trace-forms ~value)]) (partition 2 bindings)))))

 ;; Trace the let form, its bindings then the forms in its body.
(defmethod trace-special-form
  'let* [[_ bindings & body]]
  `(let* ~(trace-bindings bindings)
     (trace-forms ~@body)))

;; Trace the loop form, its bindings then the forms in its body.
(defmethod trace-special-form
  'loop* [[_ bindings & body]]
  `(loop* ~(trace-bindings bindings)
     (trace-forms ~@body)))

;; Trace the new form, mainly its arguments.
(defmethod trace-special-form
  'new [[_ name & args]]
  `(new ~name ~@(map trace-form args)))

(defn ^{:private true} trace-fn-body
  "Trace the forms in a function body."
  [body]
  `(~(first body) ~@(map trace-form (rest body))))

;; Trace the fn form.
(defmethod trace-special-form 'fn* [[_ & args]]
  (if (symbol? (first args))
    (if (vector? (second args))
      `(fn* ~(first args) ~@(trace-fn-body (rest args)))
      `(fn* ~(first args) ~@(map trace-fn-body (rest args))))
    (if (vector? (first args))
      `(fn* ~@(trace-fn-body args))
      `(fn* ~@(map trace-fn-body args)))))

(defmethod trace-special-form :default [form] :default)

(defn ^{:private true} trace-value
  "Trace the given data structure by tracing individual values."
  [v]
  (cond
    (vector? v) `(vector ~@(map trace-form v))
    (map? v) `(into {} ~(vec (map trace-value v)))
    (set? v) `(into #{} ~(vec (map trace-form v)))
    :else v))

(defn ^{:private true} recurs?
  "Test if the given form contains a recur call."
  [form]
  (if (and (or (list? form)
               (seq? form))
           (> (count form) 0))
    (condp = (first form)
      'recur true
      'quote false
      (some identity (map recurs? (rest form))))
    false))

(defn ^{:private true} trace-form*
  "Trace the given form body except if it is to be ignored."
  [form]
  (if (and (or (list? form)
               (seq? form))
           (> (count form) 0))
    (if (ignored-form? (first form))
      form
      (let [sform (trace-special-form form)]
        (if (= sform :default)
          (let [mform (macroexpand-1 form)]
            (if (= form mform)
              (cons (first mform) (map trace-form (rest mform)))
              (trace-form mform)))
          sform)))
    (trace-value form)))

(defprotocol ThrowableRecompose
  "Protocol to isolate trace-form from convoluted throwables that
   do not have a constructor with a single string argument.
   clone-throwable attempts to clone a throwable with a human readable stack trace
   and message :)
   It must return a throwable of the same class. If not clonable, the original
   throwable should be returned. At least this will preserve the original
   throwable information.
   Cloning should be non-obtrusive hence internal exceptions should be silently
   swallowed and return the original throwable."
  (clone-throwable [this stack-trace args]))

(extend-type java.lang.AssertionError
  ThrowableRecompose
  (clone-throwable [this stack-trace args]
    (try
      (let [ctor (.getConstructor java.lang.AssertionError (into-array [java.lang.Object]))
            arg (first args)]
        (doto ^AssertionError (.newInstance ctor (into-array [arg])) (.setStackTrace stack-trace)))
      (catch Exception e# this))))

(extend-type java.nio.charset.CoderMalfunctionError
  ThrowableRecompose
  (clone-throwable [this stack-trace args]
    (try
      (let [ctor (.getConstructor java.nio.charset.CoderMalfunctionError (into-array [java.lang.Exception]))
            arg (first args)]
        (cond
          (instance? java.lang.Exception arg)
          (doto ^java.nio.charset.CoderMalfunctionError (.newInstance ctor (into-array [arg])) (.setStackTrace stack-trace))
          (string? arg)
          (doto ^java.nio.charset.CoderMalfunctionError (.newInstance ctor (into-array [(Exception. ^String arg)])) (.setStackTrace stack-trace))
          :else this))
      (catch Exception e# this))))

(extend-type java.io.IOError
  ThrowableRecompose
  (clone-throwable [this stack-trace args]
    (try
      (let [ctor (.getConstructor java.io.IOError (into-array [java.lang.Throwable]))
            arg (first args)]
        (cond
          (instance? java.lang.Throwable (first arg))
          (doto ^java.io.IOError (.newInstance ctor (into-array [arg])) (.setStackTrace stack-trace))

          (string? arg)
          (doto ^java.io.IOError (.newInstance ctor (into-array [(Throwable. ^String arg)])) (.setStackTrace stack-trace))
          :else this))
      (catch Exception e# this))))

(extend-type java.lang.ThreadDeath
  ThrowableRecompose
  (clone-throwable [this _ _] this)) ;; No way we can add more info here, this one has no args to its constructor

(extend-type java.lang.Throwable
  ThrowableRecompose
  (clone-throwable [this stack-trace args]
    (try
      (let [ctor (.getConstructor (class this) (into-array [java.lang.String]))
            arg (first args)]
        (cond
          (string? arg)
          (doto ^java.lang.Throwable (.newInstance ctor (into-array [arg])) (.setStackTrace stack-trace))
          :else (doto ^java.lang.Throwable (.newInstance ctor (into-array [(str arg)])) (.setStackTrace stack-trace))))
      (catch Exception e# this))))

(extend-type java.lang.Object
  ThrowableRecompose
  (ctor-select [this _ _] this)) ;; Obviously something is wrong but the trace should not alter processing

(extend-type nil
  ThrowableRecompose
  (ctor-select [this _ _] this)) ;; Obviously something is wrong but the trace should not alter processing

(defn trace-compose-throwable
  "Re-create a new throwable with a composed message from the given throwable
   and the message to be added. The exception stack trace is kept at a minimum."
  [^Throwable throwable ^String message]
  (let [previous-msg (or (.getMessage throwable) (format ": No message attached to throwable %s" throwable))
        composed-msg (str previous-msg (if-not (.endsWith previous-msg "\n") "\n") message (if-not (.endsWith message "\n") "\n"))
        new-stack-trace (into-array java.lang.StackTraceElement [(aget (.getStackTrace throwable) 0)])
        new-throwable (clone-throwable throwable new-stack-trace [composed-msg])]
    new-throwable))

(defn trace-form
  "Trace the given form avoiding try catch when recur is present in the form."
  [form]
  (if (recurs? form)
    (trace-form* form)
    `(try
       ~(trace-form* form)
       (catch Throwable e#
         (throw (trace-compose-throwable e# (format "  Form failed: %s" (with-out-str (pprint '~form)))))))))

(defmacro trace-forms
  "Trace all the forms in the given body. Returns any underlying uncaught exceptions that may make the forms fail."
  [& body]
  `(do
     ~@(map trace-form body)))

(defn trace-var*
  "If the specified Var holds an IFn and is not marked as a macro, its
  contents is replaced with a version wrapped in a tracing call;
  otherwise nothing happens. Can be undone with untrace-var.
  In the unary case, v should be a Var object or a symbol to be
  resolved in the current namespace.
  In the binary case, ns should be a namespace object or a symbol
  naming a namespace and s a symbol to be resolved in that namespace."
  ([ns s]
     (trace-var* (ns-resolve ns s)))
  ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)]
       (if (and (ifn? @v) (-> v meta :macro not) (-> v meta ::traced not))
         (let [f @v]
           ;; (println "s:" (pr-str s))
           ;; (println (find-source s))
           (doto v
             (alter-var-root #(fn tracing-wrapper [& args]
                                (trace-fn-call s % args)))
             (alter-meta! assoc ::traced f))
           (if-let [source (try (find-source s)
                                (catch Throwable _ nil))]
             (alter-meta! v assoc ::fn-body source)
             v))))))

(defn untrace-var*
  "Reverses the effect of trace-var / trace-vars / trace-ns for the
  given Var, replacing the traced function with the original, untraced
  version. No-op for non-traced Vars.
  Argument types are the same as those for trace-var."
  ([ns s]
     (untrace-var* (ns-resolve ns s)))
  ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)
           f  ((meta v) ::traced)]
       (when f
         (doto v
           (alter-var-root (constantly ((meta v) ::traced)))
           (alter-meta! dissoc ::traced))))))

(defmacro trace-vars
  "Trace each of the specified Vars.
  The arguments may be Var objects or symbols to be resolved in the current
  namespace."
  [& vs]
  `(do ~@(for [x vs]
           `(if (var? ~x)
              (trace-var* ~x)
              (trace-var* (quote ~x))))))

(defmacro untrace-vars
  "Untrace each of the specified Vars.
  Reverses the effect of trace-var / trace-vars / trace-ns for each
  of the arguments, replacing the traced functions with the original,
  untraced versions."
  [& vs]
  `(do ~@(for [x vs]
           `(if (var? ~x)
              (untrace-var* ~x)
              (untrace-var* (quote ~x))))))

(defn  trace-ns*
  "Replaces each function from the given namespace with a version wrapped
  in a tracing call. Can be undone with untrace-ns. ns should be a namespace
  object or a symbol.
  No-op for clojure.core and clojure.tools.trace."
  [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core clojure.tools.trace} (.name ns))
      (let [ns-fns (->> ns ns-interns vals (filter (comp fn? var-get)))]
        (doseq [f ns-fns]
          (trace-var* f))))))

(defn ^{:private true} resolves-as-var?
  "Try to resolve the symbol in several ways to find out if it's a var or not."
  [n]
  (cond
    (coll? n) nil
    (try (find-ns n) (catch Exception _)) nil
    :else
    (if-let [v (try (ns-resolve *ns* n) (catch Exception _))] (var? v))))

(defmacro trace-ns
  "Trace all fns in the given name space. The given name space can be quoted, unquoted or stored in a var.
   We must try to resolve the expression passed to us partially to find out if it needs to be quoted or not
   when passed to trace-ns*"
  [n]
  (let [quote? (not (or (resolves-as-var? n) (and (coll? n) (= (first n) (quote quote)))))
        n (if quote? (list 'quote n) n)]
    `(trace-ns* ~n)))

(defn  untrace-ns*
  "Reverses the effect of trace-var / trace-vars / trace-ns for the
  Vars in the given namespace, replacing each traced function from the
  given namespace with the original, untraced version."
  [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
          (untrace-var* f))))

(defmacro untrace-ns
  "Untrace all fns in the given name space. The given name space can be quoted, unquoted or stored in a var.
   We must try to resolve the expression passed to us partially to find out if it needs to be quoted or not
   when passed to untrace-ns*"
  [n]
  (let [quote? (not (or (resolves-as-var? n) (and (coll? n) (= (first n) (quote quote)))))
        n (if quote? (list 'quote n) n)]
     `(untrace-ns* ~n)))

(defn traced?
  "Returns true if the given var is currently traced, false otherwise"
  [v]
  (let [^clojure.lang.Var v (if (var? v) v (resolve v))]
    (-> v meta ::traced nil? not)))

(defn traceable?
  "Returns true if the given var can be traced, false otherwise"
  [v]
  (let [^clojure.lang.Var v (if (var? v) v (resolve v))]
    (and (ifn? @v) (-> v meta :macro not))))
