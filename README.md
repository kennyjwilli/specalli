# specalli

Convert Clojure Specs to Malli schemas at runtime.

## Installation

## Usage

The library exposes a single API namespace `kwill.specalli` with the `mallize` function. The function takes a Clojure
Spec form and returns a Malli schema. A handful of examples are presented below. A more comprehensive set of examples
can be found in the test namespace `kwill.specalli-test`.

```clojure
(require '[kwill.specalli :as specalli])

(specalli/mallize `double?)
=> :double

(specalli/mallize (s/double-in :min 1 :max 10))
=> [:double {:min 1, :max 10}]

(specalli/mallize
  (s/cat
    :int (s/+ int?)
    :str (s/? string?)
    :m (s/* map?)))
=> [:catn [:int [:+ :int]] [:str [:? :string]] [:m [:* :map]]]
```

## Caveats

### Missing Coverage

- `s/keys*`
- `s/&`

### Merging Maps

Malli does not support merging `:map` and `:multi` yet ([see #493](https://github.com/metosin/malli/issues/493)). If you
pass a Spec that tries to merge those, the library will return a schema that appears to support it, but it will not
work.

### Runtime Conversion and Usage in ClojureScript

Certain Spec definitions are difficult to impossible to convert to a Schema at runtime without resolving a symbol or
evaluating a form. As such, dynamic conversions of those Specs are not possible in ClojureScript. A comprehensive list
of forms requiring symbol resolution or `eval` capabilities are listed below.

#### Symbol or form in Spec option value

In some places, Spec will accept a symbol as the input to a Spec's option value. For example, you can use a top-level
var as the `:max` value with `s/double-in`.

```clojure
(def the-max 10)
(s/double-in :max the-max)
```

Or you can even use a form.

```clojure
(s/double-in :max (* 1024 8)) 
```

Unfortunately, we need to pass the actual value to the Malli schema option value. This requires us to resolve or eval
the option value. Specs defined with a var or form will not work in ClojureScript.

### Enumeration via symbol

Spec supports specifying an enumeration by defining a spec as a var pointing to a set literal. To convert a Spec at
runtime, we must know if the symbol returned by the Spec's form is a set. We cannot do this dynamic resolution in
ClojureScript, so we fall back to a `:fn` schema.

```clojure
(def enum-set #{"a"})
=> #'user/enum-set

(s/def ::enum my-set)
=> :user/enum

(s/form ::enum)
=> user/my-set

;; In CLJS only:
(specalli/mallize ::enum)
=> [:fn #{"a"}]
```

#### `s/multi-spec`

Multispecs are defined using multimethods which are defined at runtime. We need to resolve the multimethod symbol to a
callable function, enumerate its methods, and execute the multimethod for each dispatch to create the `:multi` schema.
