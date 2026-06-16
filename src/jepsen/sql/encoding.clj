(ns jepsen.sql.encoding
  "Maps values to and from SQL types in various ways."
  (:refer-clojure :exclude [name type])
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [string :as str]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen.sql [checker :refer [assert-instance-or-nil]]]))

(defprotocol Encoding
  "This protocol describes a way to coerce longs to and from SQL
  columns."
  (name [this] "The arbitrary object name of this encoding.")
  (type [this] "The SQL string we use for the column type.")
  (encode [this k x] "Serializes a long value to the appropriate SQL type.")
  (decode [this k x] "Deencodes x back to a long."))

(defrecord IntegerEncoding []
  Encoding
  (name [_] :integer)
  (type [_] "integer")
  (encode [_ k x] x)
  (decode [_ k x]
    (when-not (or (nil? x)
                  (integer? x))
      (throw+ {:type :jepsen.sql/wrong-type
               :expected :integer
               :actual   (class x)
               :value    x}))
    (when x (long x))))

(defrecord DoubleEncoding []
  Encoding
  (name [_] :double-precision)
  (type [_] "double precision")
  (encode [_ k x]
    ; Gotta be within 2^24, otherwise floats will lose integer precision
    ; Double should be able to do 2^53, but let's be conservative
    (assert (< -16777216 x 16777216))
    x)
  (decode [_ k x]
    (assert-instance-or-nil Double x)
    (when x (long x))))

(defrecord DecimalEncoding []
  Encoding
  (name [_] :decimal)
  (type [_] "decimal(32, 16)")
  (encode [_ k x]
    ; We encode either before or after the decimal
    (if (even? (hash k))
      x
      ; 1230 -> 0.0321
      (bigdec (str "0." (str/join (reverse (str x)))))))
  (decode [_ k x]
    (assert-instance-or-nil BigDecimal x)
    (when x
      (if (even? (hash k))
        (long x)
        (if-let [[m digits] (re-find #"^0.(\d+)$" (str x))]
          (parse-long (str/join (reverse digits)))
          (throw+ {:type :jepsen.sql/unexpected-value
                   :expected "0.123..."
                   :actual x}))))))

; char(n) fixed-width padded strings
(defrecord CharacterEncoding []
  Encoding
  (name [_] :character)
  (type [_] "character(32)")
  (encode [_ k x] (str x))
  (decode [_ k x]
    (assert-instance-or-nil String x)
    (when x
      (parse-long (str/trim x)))))

(defrecord IdentityEncoding []
  Encoding
  (name [_] :identity)
  (type [_] "integer")
  (encode [_ k x] x)
  (decode [_ k x] x))

(def all-encodings
  "All available encodings"
  [(DoubleEncoding.)
   (CharacterEncoding.)
   (DecimalEncoding.)
   (IntegerEncoding.)])

(defn encodings
  "All allowed encodings for a test."
  [test]
  (let [names (set (:encodings test (mapv name all-encodings)))]
    (assert (seq names) "No encodings provided")
    (let [encodings (filterv (comp names name) all-encodings)]
      (when (not= (count names)
                  (count encodings))
        (throw (IllegalArgumentException. (str "Unrecognized encoding names"
                                               (pr-str names)))))
      encodings)))

(defn encoding
  "Turns an integer key into an encoding. Rotates between encodings using mod.
  Helpful for picking an encoding for each table."
  [^long k]
  (nth encodings (mod k (count encodings))))
