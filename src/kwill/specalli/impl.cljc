(ns kwill.specalli.impl
  (:require
    [clojure.core.match :as match]
    [clojure.spec.alpha :as s]
    [malli.core :as m]
    [malli.registry :as mr]))

(declare mallize*)

(defn convert-set
  [set]
  (into [:enum] set))

(defmulti convert-expression* (fn [form opts] (first form)))
(defmulti convert-symbol* (fn [sym opts] sym))

(defn convert-expression
  [form opts]
  (convert-expression* form opts))

(defn convert-symbol
  [sym opts]
  (convert-symbol* sym opts))

(defn- coerce-number
  "Coerces `x` into a numeric literal given a symbol or list form, else throws."
  [x]
  (cond
    #?@(:clj [(symbol? x) @(resolve x)])
    #?@(:clj [(list? x) (eval x)])
    (number? x) x
    :else
    (throw (ex-info (str "Cannot coerce value to numeric literal " (pr-str x)) {:x x}))))

(defmethod convert-symbol* `double? [_ opts] (convert-expression `(s/double-in) opts))
(defmethod convert-expression* `s/double-in
  [[_ & {:keys [min max NaN? infinite?]}] _]
  (let [opts (cond-> {}
               min (assoc :min (coerce-number min))
               max (assoc :max (coerce-number max))
               (some? NaN?) (assoc :NaN? NaN?)
               (some? infinite?) (assoc :infinite? infinite?))]
    (cond-> [:double]
      (seq opts) (conj opts))))

(defmethod convert-symbol* `int? [_ opts] (convert-expression `(s/int-in nil nil) opts))
(defmethod convert-symbol* `integer? [_ opts] (convert-expression `(s/int-in nil nil) opts))
(defmethod convert-symbol* `pos-int? [_ opts] (convert-expression `(s/int-in 1 nil) opts))
(defmethod convert-symbol* `neg-int? [_ opts] (convert-expression `(s/int-in nil -1) opts))
(defmethod convert-symbol* `nat-int? [_ opts] (convert-expression `(s/int-in 0 nil) opts))
(defmethod convert-expression* `s/int-in
  [[_ min max] _]
  (let [opts (cond-> {}
               min (assoc :min (coerce-number min))
               max (assoc :max (coerce-number max)))]
    (cond-> [:int]
      (seq opts) (conj opts))))

(defmethod convert-symbol* `string? [_ _] :string)
(defmethod convert-symbol* `uuid? [_ _] :uuid)
(defmethod convert-symbol* `uri? [_ _] ['uri?])
(defmethod convert-symbol* `boolean? [_ _] :boolean)
(defmethod convert-symbol* `inst? [_ _] ['inst?])
(defmethod convert-symbol* `map? [_ _] :map)
(defmethod convert-symbol* `keyword? [_ _] :keyword)
(defmethod convert-symbol* `any? [_ _] :any)
(defmethod convert-symbol* `nil? [_ _] :nil)
(defmethod convert-symbol* `qualified-keyword? [_ _] :qualified-keyword)
(defmethod convert-symbol* `qualified-symbol? [_ _] :qualified-symbol)

(defmethod convert-expression* `s/merge
  [[_ & specs] opts]
  (into [:merge]
    (map #(mallize* % opts))
    specs))

(defmethod convert-expression* `s/keys
  [[_ & {:keys [req req-un opt opt-un]}] {:keys [registry] :as opts}]
  (let [unqualify (fn [ks] (map #(keyword (name %)) ks))
        kfn (fn [k]
              (if (and registry (mr/schema registry k))
                k
                (mallize* k opts)))
        req' (map (fn [k] [k (kfn k)]) req)
        req-un' (map (fn [k] [(unqualify k) (kfn k)]) req-un)
        opt' (map (fn [k] [k {:optional true} (kfn k)]) opt)
        opt-un' (map (fn [k] [(unqualify k) {:optional true} (kfn k)]) opt-un)
        ks (concat req' req-un' opt' opt-un')]
    (into [:map] ks)))

(defmethod convert-expression* `s/map-of
  [[_ kpred vpred] opts]
  [:map-of (mallize* kpred opts) (mallize* vpred opts)])

#?(:clj
   ;; resolve is not supported in cljs so this is clj only for now
   (defmethod convert-expression* `s/multi-spec
     [[_ multi-method retag] opts]
     (let [multi-fn @(resolve multi-method)
           methods (methods multi-fn)]
       (into [:multi {:dispatch retag}]
         (map (fn [[dispatch-val f]]
                [dispatch-val (mallize* (f nil) opts)]))
         methods))))

(defn mallize-collection-form
  [[_ pred & {:keys [into kind count max-count min-count distinct gen-max gen]} :as form] opts]
  (let [;; TODO: allow end user extension
        op (condp = kind
             `vector? :vector
             `set? :set
             nil :sequential
             (throw (ex-info (str "Unsupported collection :kind " (pr-str kind) ".")
                      {:kind kind
                       :form form})))
        params (cond-> {}
                 min-count (assoc :min min-count)
                 max-count (assoc :max max-count)
                 count (assoc :min count :max count)
                 gen-max (assoc :gen/max gen-max))]
    (cond-> [op]
      (seq params) (conj params)
      true (conj (mallize* pred opts)))))

(defmethod convert-expression* `s/every [form opts] (mallize-collection-form form opts))

(defmethod convert-expression* `s/every-kv
  [[_ kpred vpred & s-opts] opts]
  ;; Reused s/every-kv impl form
  ;; https://github.com/clojure/spec.alpha/blob/13bf36628eb02904155d0bf0d140f591783c51af/src/main/clojure/clojure/spec/alpha.clj#L579
  (mallize* `(s/every (s/tuple ~kpred ~vpred) ~@s-opts) opts))

(defmethod convert-expression* `s/coll-of [form opts] (mallize-collection-form form opts))

(defmethod convert-expression* `s/cat
  [[_ & key-pred-forms] opts]
  (into [:catn]
    (comp
      (partition-all 2)
      (map (fn [[k pred]]
             [k (mallize* pred opts)])))
    key-pred-forms))

(defmethod convert-expression* `s/alt
  [[_ & key-pred-forms] opts]
  (into [:altn]
    (comp
      (partition-all 2)
      (map (fn [[k pred]]
             [k (mallize* pred opts)])))
    key-pred-forms))

;(defmethod convert-expression* `s/& [[_ pred-form] opts] [:* (mallize* pred-form opts)])

;; https://clojure.github.io/spec.alpha/clojure.spec.alpha-api.html#clojure.spec.alpha/*
(defmethod convert-expression* `s/* [[_ pred-form] opts] [:* (mallize* pred-form opts)])

;; https://clojure.github.io/spec.alpha/clojure.spec.alpha-api.html#clojure.spec.alpha/+
(defmethod convert-expression* `s/+ [[_ pred-form] opts] [:+ (mallize* pred-form opts)])

;; https://clojure.github.io/spec.alpha/clojure.spec.alpha-api.html#clojure.spec.alpha/?
(defmethod convert-expression* `s/? [[_ pred-form] opts] [:? (mallize* pred-form opts)])

;; Unclear Malli equivalent: s/keys* (useful in Malli?)

(defmethod convert-expression* `s/inst-in
  [[_ start end] opts]
  ;; 2022-09-22: Malli doesn't have support for dates, so we do a best effort convert
  ;; https://github.com/metosin/malli/issues/49
  [:and 'inst? [:fn #(s/inst-in-range? start end %)]])

(defmethod convert-expression* `s/nonconforming [[_ form] opts] (mallize* form opts))

(defmethod convert-expression* `s/nilable [[_ form] opts] [:maybe (mallize* form opts)])

(defmethod convert-expression* `s/tuple
  [[_ & preds] opts]
  (into [:tuple]
    (map #(mallize* % opts))
    preds))

(defn match-spec-preds
  "Returns Malli schema opts under ::schema-opts if the `pred-forms` included
  match predicate forms Spec generates by default. Forms not matched are returned
  under `::unmatched`."
  [pred-forms]
  (reduce
    (fn [acc form]
      (match/match [form]
        [((`fn _ ((`<= '% max) :seq)) :seq)]
        (update acc ::schema-opts assoc :max max)
        [((`fn _ ((`<= min '%) :seq)) :seq)]
        (update acc ::schema-opts assoc :min min)
        [((`fn _ ((`s/int-in-range? min max _) :seq)) :seq)]
        (update acc ::schema-opts assoc :min min :max max)
        [((`fn _ ((`not (('Double/isInfinite '%) :seq)) :seq)) :seq)]
        (update acc ::schema-opts assoc
          :gen/infinite? false
          :infinite? false)
        [((`fn _ ((`not (('Double/isNaN '%) :seq)) :seq)) :seq)]
        (update acc ::schema-opts assoc
          :gen/NaN? false
          :NaN? false)
        :else (update acc ::unmatched conj form)))
    {::schema-opts {}
     ::unmatched   []} pred-forms))

(comment
  (s/def ::double-w-pred (s/double-in :NaN? false :infinite? false :min 1 :max 2))
  (match-spec-preds (drop 2 (s/form ::double-w-pred))))

(defmethod convert-expression* `s/and
  [[_ & pred-forms] opts]
  (let [schemas (loop [pred-forms pred-forms
                       schemas []]
                  (if-let [pred-form (first pred-forms)]
                    (if (symbol? pred-form)
                      (let [{::keys [schema-opts unmatched]} (match-spec-preds (rest pred-forms))]
                        (recur
                          unmatched
                          (conj schemas
                            (condp = pred-form
                              `double? (mallize* (list* `s/double-in (mapcat identity schema-opts)) opts)
                              `int? (mallize* (list `s/int-in (:min schema-opts) (:max schema-opts)) opts)
                              (mallize* pred-form opts)))))
                      (recur (rest pred-forms) (conj schemas (mallize* pred-form opts))))
                    schemas))]
    (if (= 1 (count schemas))
      (first schemas)
      (into [:and] schemas))))

(comment
  (match/match [(vec pred-forms)]
    [[`double?
      ((`fn _ ((`<= '% max) :seq)) :seq)
      ((`fn _ ((`<= min '%) :seq)) :seq)]]
    (mallize* (list `s/double-in :min min :max max) opts)
    [[`int?
      ((`fn _ ((`s/int-in-range? min max _) :seq)) :seq)]]
    (mallize* (list `s/int-in min max) opts)
    :else (into [:and] (map #(mallize* % opts)) pred-forms))
  )

(defmethod convert-expression* `s/or
  [[_ & key-pred-forms] opts]
  (into [:orn]
    (map (fn [[k pred-form]]
           [k (mallize* pred-form opts)]))
    (partition 2 key-pred-forms)))

;; Resolves at runtime, so Clojure only
;; Handles case like: (s/def ::set-enum (set set-enum-vs))
#?(:clj
   (defmethod convert-expression* `set
     [[_ set-sym] opts]
     (mallize* (set (eval set-sym)) opts)))

(defmethod convert-expression* 'spec-tools.core/spec
  [[_ spec-form] opts]
  (or
    (:kwill.specalli/schema spec-form)
    (mallize* (:spec spec-form) opts)))

(defn find-registered-sym
  [sym {:keys [registry]}]
  (let [try-syms (cond-> [sym]
                   (and (qualified-symbol? sym) (= "clojure.core" (namespace sym)))
                   (conj (symbol (name sym))))]
    (some (fn [sym]
            (when (mr/schema (or registry m/default-registry) sym)
              sym))
      try-syms)))

(defmethod convert-symbol* :default
  [sym opts]
  ;; If schema exists in registry, use the registered schema, else catch rest with :fn
  (if-let [sym (find-registered-sym sym opts)]
    sym
    #?(:clj [:fn @(requiring-resolve sym)])))

#?(:clj
   (defmethod convert-expression* :default
     [form opts]
     [:fn (eval form)]))

(defn simplify-schema
  [schema]
  (if (and (counted? schema) (= 1 (count schema)))
    (first schema)
    schema))

(defn mallize*
  [spec opts]
  ;(prn "A)" spec)
  (let [form (if (or (seq? spec) (symbol? spec)) spec (s/form spec))
        schema (cond
                 ;; spec pointer ref. e.g., (s/form (s/def ::parent ::child)) => ::child
                 (keyword? form)
                 (mallize* (s/form form) opts)
                 ;; simple predicate or set enumeration
                 ;; set enumeration is not supported in CLJS (is it possible to support it?)
                 (symbol? form)
                 (let [v #?(:clj @(resolve form) :cljs nil)]
                   (if (set? v)
                     (mallize* v opts)
                     (convert-symbol form opts)))
                 ;; set literal
                 (set? form)
                 (convert-set form)
                 ;; spec helper fn
                 (seq? form)
                 (convert-expression form opts))]
    ;(prn "B)" {:form form :sym? (symbol? form) :kw? (keyword? form)})
    (simplify-schema schema)))


