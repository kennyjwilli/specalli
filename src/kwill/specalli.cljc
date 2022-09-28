(ns kwill.specalli
  "Clojure Spec to Malli schema conversion."
  (:require
    [clojure.spec.alpha :as s]
    [kwill.specalli.impl :as impl]))

(defn mallize
  "Returns a Malli Schema for the given `spec`. Optionally takes an `opts` map
  that gets passed to all Malli API calls."
  ([spec] (mallize spec nil))
  ([spec opts]
   (impl/mallize* spec opts)))

(comment
  (defmulti my-spec ::type)
  (defmethod my-spec ::a [_] map?)
  (s/def ::multi (s/multi-spec my-spec ::type))
  (s/form ::multi)
  (methods @(resolve `my-spec))

  (s/def ::size-gib
    (s/int-in 1 (* 64 1024)))
  (s/form ::size-gib)

  (mallize ::size-gib)

  (mallize ::multi {})
  (mallize (s/and int? pos?) {})

  (s/def ::string-in (compute.data-model.spec-helpers/string-in :min 1))
  (s/form ::string-in)
  (s/get-spec ::string-in)

  (mallize ::double-in {})
  (mallize ::double-in-and {})
  (s/form ::double-in-and)

  (s/def ::every (s/every number? :min 1))
  (s/form ::every)

  (s/def ::double-in (s/double-in :min 1 :max 2))
  (s/form ::double-in)
  (s/def ::double-in-and (s/and (s/double-in :min 1 :max 2) pos?))
  (s/form ::double-in-and)
  (s/def ::int-in (s/int-in 1 2))
  (s/form ::int-in))
