(ns kwill.specalli-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test :refer :all]
    [malli.registry :as mr]
    [matcher-combinators.clj-test]
    [spec-tools.core :as st]
    [malli.core :as m]
    [kwill.specalli :as specalli]))

(defn =-fn
  [x]
  (fn [y] (= y x)))

;; Multi-spec
(defmulti example-spec ::type)
(defmethod example-spec ::a [_] map?)
(s/def ::multi (s/multi-spec example-spec ::type))

(s/def ::every (s/every number? :min 1))

(s/def ::double-in (s/double-in :min 1 :max 2))
(s/def ::and-double-in-pos (s/and (s/double-in :min 1 :max 2) pos?))
(s/def ::int-in (s/int-in 1 2))

(s/def ::int-in-list-max (s/int-in 1 (* 1 2)))

(s/def ::and-with-fn-call (s/and double? (=-fn 42)))

(def set-enum-vs ["a"])
(s/def ::set-enum (set set-enum-vs))

;; Spec tools
(s/def ::spec-tools-schema-data
  (st/spec {:spec                  `string?
            :form                  'string?
            :kwill.specalli/schema :string}))

(def example-max-count 10)

(comment (s/form ::set-enum)


  )

(def example-registry
  (mr/fast-registry
    (merge
      (m/default-schemas)
      {::int-in [:int {:min 1 :max 2}]})))

(comment
  (require 'sc.api)
  (malli.core/validate [:double {:max `example-max-count}] 11.0)
  (malli.core/validate :map [])
  (malli.core/validate [:map
                        ::int-in
                        ]
    {::int-in 1}
    {:registry example-registry})
  (neg-int? 0)

  (m/form
    (m/schema [:map
               ::int-in]
      {:registry example-registry})
    {:registry example-registry})

  (malli.registry/schema example-registry ::int-in)
  (specalli/mallize (s/keys :req [::int-in])
    {:registry example-registry})

  (let [my-pred #(pos? %)]
    (specalli/mallize (s/and int? my-pred)))

  m/validator
  )

(deftest mallize-test
  (testing "double"
    (is (= :double (specalli/mallize `double?)) "double?")
    (testing "s/double-in"
      (is (= :double (specalli/mallize (s/double-in))))
      (is (= [:double {:min 1 :max 10}]
            (specalli/mallize (s/double-in :min 1 :max example-max-count))))
      (is (= [:and [:double {:min 1, :max 2}] 'pos?]
            (specalli/mallize ::and-double-in-pos)))))

  (testing "int"
    (is (= :int (specalli/mallize int?)))
    (is (= :int (specalli/mallize integer?)))
    (is (= [:int {:min 1}] (specalli/mallize pos-int?)))
    (is (= [:int {:max -1}] (specalli/mallize neg-int?)))
    (is (= [:int {:min 0}] (specalli/mallize nat-int?)))
    (is (= [:int {:min 0 :max 2}] (specalli/mallize (s/int-in 0 2))))
    (is (= [:int {:min 1 :max 2}]
          (specalli/mallize ::int-in-list-max))))

  (is (= :string (specalli/mallize string?)))
  (is (= :uuid (specalli/mallize uuid?)))
  (is (= :map (specalli/mallize map?)))
  (is (= :keyword (specalli/mallize keyword?)))

  (testing "s/keys"
    (is (= [:map
            [::int-in [:int {:min 1 :max 2}]]]
          (specalli/mallize (s/keys :req [::int-in]))))
    (is (= [:map
            [::int-in ::int-in]]
          (specalli/mallize (s/keys :req [::int-in])
            {:registry example-registry}))))

  (is (= [:multi {:dispatch ::type}
          [::a :map]]
        (specalli/mallize ::multi))
    "s/multispec")

  (is (= [:map-of :keyword :string]
        (specalli/mallize (s/map-of keyword? string?)))
    "s/map-of")

  (is (= [:sequential :int] (specalli/mallize (s/every int?))))
  (is (= [:sequential [:tuple :string :int]] (specalli/mallize (s/every-kv string? int?))))
  (is (= [:sequential :string] (specalli/mallize (s/coll-of string?))))
  (is (= :string (specalli/mallize (s/nonconforming string?))))
  (is (= [:tuple :int :int] (specalli/mallize (s/tuple int? int?))))
  (is (= [:maybe :string] (specalli/mallize (s/nilable string?))))

  (is (= [:catn [:int :int]] (specalli/mallize (s/cat :int int?))))
  (is (= [:catn
          [:int [:+ :int]]
          [:str [:? :string]]
          [:m [:* :map]]]
        (specalli/mallize
          (s/cat
            :int (s/+ int?)
            :str (s/? string?)
            :m (s/* map?)))))
  (is (= [:altn [:int :int]] (specalli/mallize (s/alt :int int?))))

  (testing "s/and"
    (is (= [:and :int 'pos?] (specalli/mallize (s/and int? pos?))))
    (is (match? [:and :int [:fn fn?]]
          (specalli/mallize (s/and int? (=-fn 1))))))

  (testing "s/or"
    (is (= [:orn [:int :int] [:double :double]]
          (specalli/mallize (s/or :int int? :double double?)))))

  (is (match? [:and 'inst? [:fn fn?]]
        (specalli/mallize (s/inst-in #inst"2020" #inst"2021"))))

  (testing "enum"
    (is (= [:enum "a"] (specalli/mallize ::set-enum)))
    (is (= [:enum "a"] (specalli/mallize #{"a"})))))
