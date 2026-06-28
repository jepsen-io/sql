(ns jepsen.sql.gen-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [clojure.test.check.generators :as g]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [jepsen [random :as rand]]
            [jepsen.sql [ast :refer :all]
                        [gen :refer :all]]))

(deftest gen-schema-test
  (is (= (schema
           [(table "t"
                   [(column "c" text-type {:primary-key? true})
                    (column "b" boolean-type)
                    (column "a" integer-type)])
            (table "u"
                   [(column "b" integer-type {:primary-key? true})])]
           {:longs [1 -1]
            :doubles [-3.0 -2.0]
            :strings ["Xy"]})
         (g/generate (gen-schema {:max-table-count  3
                                  :max-column-count 3})
                     2 123))))

(deftest gen-case-test
  (rand/with-seed 0
    (let [case (generate {:max-statement-count 1024})]
      (is (= [["CREATE TABLE t (b TEXT PRIMARY KEY, a TEXT, c TEXT, d BOOLEAN)"]
              ["CREATE TABLE v (a BOOLEAN, c INTEGER PRIMARY KEY, b INTEGER, d BOOLEAN)"]
              ["CREATE TABLE u (d INTEGER)"]]
             (mapv sql (setup case))))

      (is (= [["DROP TABLE t"] ["DROP TABLE v"] ["DROP TABLE u"]]
             (mapv sql (teardown case))))
      ; You're meant to replace this when the generators change; the idea is to
      ; look through the history by hand and make sure it seems reasonable;
      ; you're getting a nice blend of statements and predicates, etc.
      ;(mapv (comp prn sql) (:statements case))
      (is (= [
["SELECT * FROM v"]
["SELECT * FROM v"]
["DELETE FROM v WHERE ?" true]
["SELECT * FROM v WHERE ((d = ?) AND (d OR (? = ?)) AND ((d OR ? OR ? OR a) OR (c = c)) AND a)" false nil true true true]
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" nil 125 -3545 false]
["DELETE FROM t"]
["SELECT * FROM v"]
["SELECT * FROM u WHERE (d = ?)" -27283]
["INSERT INTO u (d) VALUES (?)" -163662]
["INSERT INTO u (d) VALUES (?)" -24]
["UPDATE v SET a = ?, b = ?, d = ?, c = ? WHERE (? = b)" true 3334 nil 287693 125]
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" nil -50026 -1831 true]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" true -922880 nil -25]
["SELECT * FROM v WHERE ?" nil]
["UPDATE v SET a = ?" nil]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" true 31832 -852274519 false]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" true 6171078 true -6467835]
["UPDATE u SET d = ?" 1231]
["DELETE FROM u WHERE ((d = d) AND (NOT ?) AND (NOT (? = ?)))" nil 806056 -1200097335]
["SELECT * FROM t"]
["INSERT INTO t (c, d, b, a) VALUES (?, ?, ?, ?)" "OQE" false "ziq" "x-HjP6b5Zn7GUGLUD8z"]
["SELECT * FROM v WHERE a"]
["INSERT INTO u (d) VALUES (?)" 1231]
["SELECT * FROM u"]
["UPDATE v SET a = ?, b = ? WHERE (? = ?)" false -27283 8334377 62957454]
["INSERT INTO t (c, a, b, d) VALUES (?, ?, ?, ?)" "0ljecM D7Ld" "lx6aqpuo-4mUWz003--t3A" "3" nil]
["UPDATE t SET b = ?, d = ? WHERE (NOT ?)" "OQE" true false]
["SELECT * FROM v"]
["UPDATE v SET a = ?, b = ?, d = ? WHERE ?" false -2 true true]
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" true -27283 15396517 true]
["SELECT * FROM t WHERE (? = ?)" "ziq" "LeKRPrfbQjRcrdd4r-fG"]
["SELECT * FROM t"]
["SELECT * FROM u WHERE ?" nil]
["INSERT INTO u (d) VALUES (?)" -108]
["UPDATE t SET b = ?, a = ?, c = ?, d = ? WHERE (d = (NOT d))" "0" "OQE" "g-tf" nil]
["SELECT * FROM u"]
["INSERT INTO u (d) VALUES (?)" 125]
["SELECT * FROM v WHERE (? = ?)" false true]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "0" nil "KyDtZ0FnJ4iSe6xm_35kJcH7qsNv188" "w0w7wG_Og2D"]
["INSERT INTO u (d) VALUES (?)" -45]
["SELECT * FROM t"]
["INSERT INTO t (b, a, d, c) VALUES (?, ?, ?, ?)" "FitI8wNUB7TXVAlkWu" "aVXid1JN" true "jb0 gLy5oQACXX9vJEdl"]
["SELECT * FROM v WHERE a"]
["UPDATE v SET c = ?" 0]
["SELECT * FROM u WHERE (NOT ((? = d) AND ? AND (d = ?) AND (d = d)))" 1231 true -27283]
["INSERT INTO t (d, b, a, c) VALUES (?, ?, ?, ?)" nil "ziq" "ziq" "6NkaV"]
["UPDATE v SET b = ? WHERE a" 125]
["INSERT INTO t (c, d, b, a) VALUES (?, ?, ?, ?)" "OQE" nil "0" "xCEHyT-NfMz8jk8cJPWzwc47TrpZn"]
["SELECT * FROM v WHERE ?" false]
["INSERT INTO v (b, d, a, c) VALUES (?, ?, ?, ?)" 125 true false 132260549]
["UPDATE t SET c = ?, d = ? WHERE ?" "PMv7" false false]
["INSERT INTO t (a, b, c, d) VALUES (?, ?, ?, ?)" "ziq" "fby3qM81pr76pookgSZOZv0Oevbu" "3m8BfazFMQ2VBsVHs6k9aDgnh1Y" false]
["SELECT * FROM t"]
["SELECT * FROM u WHERE (? = d)" -4566]
["INSERT INTO u (d) VALUES (?)" 1231]
["UPDATE u SET d = ? WHERE ((? = d) OR (d = ?) OR (d = ?) OR ?)" -27283 442529 125 3779 true]
["UPDATE u SET d = ? WHERE ?" 125 nil]
["SELECT * FROM t WHERE d"]
["UPDATE u SET d = ? WHERE ?" 1231 false]
["UPDATE u SET d = ? WHERE ?" 817216526 nil]
["SELECT * FROM t WHERE (b = a)"]
["SELECT * FROM u WHERE ((d = ?) OR ((? AND ?) OR ?))" 1231 true nil false]
["SELECT * FROM t"]
["UPDATE u SET d = ?" 369873]
["SELECT * FROM t WHERE ?" nil]
["UPDATE v SET a = ?, b = ?, c = ?, d = ? WHERE (((? = ?) AND ? AND (b = ?) AND (c = ?)) OR ? OR ? OR ?)" nil -27283 -27283 nil false nil nil 5101842 5854 nil nil true]
["DELETE FROM t WHERE ((? = ?) = (? OR d OR ? OR d))" "M4lsiqVF7hd7f-vU12ovNMNpR2x" "SKerW3mPgaHf5lc-9P3b1q4Q1" false true]
["INSERT INTO u (d) VALUES (?)" -1]
["UPDATE t SET c = ?, b = ?, d = ?, a = ? WHERE (NOT d)" "moYDrhfZxUKju8Lqmq" "0" true "zoXItDkf75 ytSADq"]
["SELECT * FROM t WHERE d"]
["SELECT * FROM t WHERE (b = ?)" "lw5doFKX-oqOsCSc17m"]
["SELECT * FROM u"]
["SELECT * FROM t WHERE (d = (d AND ? AND ?))" nil true]
["SELECT * FROM v WHERE (? OR ((c = ?) OR (? = d) OR (? OR ?) OR (b = c)) OR ? OR (? = ?))" false -2 nil true false true true nil]
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" false -27283 -1617527243 false]
["INSERT INTO u (d) VALUES (?)" -27283]
["SELECT * FROM v WHERE (((a OR d OR d) AND (NOT a) AND (? = d) AND (NOT ?)) OR ((d = ?) AND ?))" false nil false false]
["SELECT * FROM u WHERE (d = d)"]
["INSERT INTO t (a, d, c, b) VALUES (?, ?, ?, ?)" "OQE" nil "OQE" "ziq"]
["INSERT INTO u (d) VALUES (?)" 1022]
["UPDATE t SET a = ?, c = ? WHERE d" " tz-Rj_AIwA" "0"]
["SELECT * FROM u WHERE ((NOT (d = d)) AND ?)" nil]
["INSERT INTO v (c, a, b, d) VALUES (?, ?, ?, ?)" 125 false -27283 true]
["SELECT * FROM v WHERE ?" false]
["SELECT * FROM u WHERE ?" true]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "ziq" true "ziq" "0"]
["INSERT INTO t (d, c, b, a) VALUES (?, ?, ?, ?)" true "F5gJnOyLMjoSnPdVEVumkWQsMjkmT" "uKj-rabJ8a55h8DVFSIPjgCx1K0JcJKdF" "ziq"]
["UPDATE t SET a = ?, d = ?, b = ?, c = ? WHERE (NOT d)" "5ZCThTmF" nil "ziq" "S6LdxbhrX-EreuTV4YEBMnfAwyTrS_0QE"]
["UPDATE u SET d = ? WHERE (d = d)" -23]
["SELECT * FROM v"]
["SELECT * FROM u WHERE (d = d)"]
["INSERT INTO v (b, d, c, a) VALUES (?, ?, ?, ?)" -132082363 nil -7595 true]
["SELECT * FROM v WHERE ((a = a) = ?)" false]
["SELECT * FROM t WHERE d"]
["UPDATE u SET d = ?" 1231]
["INSERT INTO v (c, a, d, b) VALUES (?, ?, ?, ?)" -27283 false false 1231]
["SELECT * FROM v WHERE (b = b)"]
["INSERT INTO u (d) VALUES (?)" 1231]
["SELECT * FROM u WHERE ?" nil]
["SELECT * FROM u WHERE (? OR (d = d) OR (? = ?))" true 1231 -423903]
["INSERT INTO u (d) VALUES (?)" 449461560]
["SELECT * FROM u"]
["SELECT * FROM u"]
["UPDATE u SET d = ? WHERE (? = ?)" -6395 -875984 -293557]
["SELECT * FROM t WHERE ?" nil]
["UPDATE t SET c = ?, a = ? WHERE ((? = ?) = (d = d))" "0" "qpe_jvZ3" "OQE" "TEJ"]
["SELECT * FROM u WHERE (? = d)" -27283]
["INSERT INTO t (c, a, b, d) VALUES (?, ?, ?, ?)" "OQE" "0" "2syx3jvONLBcbgoknDytPPf irlfre" nil]
["INSERT INTO v (c, a, d, b) VALUES (?, ?, ?, ?)" -39254068 false false 1231]
["SELECT * FROM t WHERE ((? AND d AND ?) = d)" true false]
["INSERT INTO v (b, a, c, d) VALUES (?, ?, ?, ?)" -27283 false -29 nil]
["UPDATE v SET b = ?, d = ?, a = ? WHERE (NOT d)" 125 true false]
["SELECT * FROM t WHERE ?" nil]
["SELECT * FROM u WHERE ?" nil]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" false -27283 nil 125]
["UPDATE u SET d = ? WHERE (? = d)" 125 4660756]
["SELECT * FROM t WHERE ((c = ?) AND d AND (NOT d) AND (d AND (d OR d)))" "wUZ"]
["INSERT INTO v (b, c, a, d) VALUES (?, ?, ?, ?)" 1231 125 true nil]
["INSERT INTO t (c, d, b, a) VALUES (?, ?, ?, ?)" "ziq" false "q4FRrvff62-rwZsl-T7FvtW" "ziq"]
["INSERT INTO u (d) VALUES (?)" 1231]
["SELECT * FROM v WHERE (b = b)"]
["UPDATE t SET b = ?" "V8rodsGIiZp"]
["INSERT INTO v (b, a, c, d) VALUES (?, ?, ?, ?)" -7 nil 1231 false]
["SELECT * FROM u WHERE (? = d)" -27283]
["SELECT * FROM u WHERE ((? = ?) OR ((NOT ?) AND (NOT ?) AND (NOT ?) AND (NOT ?)) OR ((d = d) AND (? OR ? OR ? OR ?) AND (d = ?) AND ?) OR (? = d))" -12 96963207 false true true true nil false false false -27283 false 58483]
["SELECT * FROM u WHERE (? = ?)" -27283 1231]
["INSERT INTO t (a, d, c, b) VALUES (?, ?, ?, ?)" "LHFiTTym9mY82" false "COEbtWIkLN5wWLoQIIV46GO" "OQE"]
["SELECT * FROM v WHERE (? AND (? = ?))" false nil false]
["UPDATE t SET c = ?, b = ?, a = ?, d = ?" "iEjZulD53RkTrz0M5uP" "0" "ziq" false]
["UPDATE t SET c = ?, d = ? WHERE ?" "ziq" false false]
["INSERT INTO v (b, d, c, a) VALUES (?, ?, ?, ?)" -936068120 true 1231 false]
["UPDATE t SET b = ?, a = ?, d = ?, c = ? WHERE (d = ?)" "OQE" "OQE" nil "xuxW4uN6DT9Lx8K9Wn" nil]
["UPDATE t SET b = ?, a = ? WHERE ((? OR d OR d) = (? OR ? OR ? OR d))" "OQE" "LDmrDHcTJjLNkZEK-kxbwU" nil true false nil]
["UPDATE v SET a = ?, d = ? WHERE (? = (NOT ?))" nil false nil true]
["INSERT INTO v (d, a, b, c) VALUES (?, ?, ?, ?)" true true -27283 -162072688]
["INSERT INTO u (d) VALUES (?)" 1231]
["INSERT INTO t (c, a, b, d) VALUES (?, ?, ?, ?)" "mQ9FI" "ziq" "fyYCStEbHWPABUyNx1BJ" true]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" false 1231 125 nil]
["UPDATE u SET d = ? WHERE (? = d)" -27283 -729216978]
["SELECT * FROM v WHERE ?" nil]
["SELECT * FROM v WHERE (? = (? OR d OR d))" nil nil]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" nil -27283 true 4802]
["SELECT * FROM v"]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "0" false "qGst4hzceNnoH3-o" "UZqC"]
["INSERT INTO u (d) VALUES (?)" 0]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" false 3618 6 true]
["SELECT * FROM t"]
["UPDATE t SET c = ?, b = ? WHERE ((? OR (NOT d) OR (? AND ?) OR (d = d)) OR (d = d) OR d)" "U-IVgzMZxvF7X6p" "OIg7_2vjc33J0tXI0hN6-78RWeDUDK1JF" true false nil]
["SELECT * FROM u WHERE (NOT ?)" false]
["SELECT * FROM u WHERE (NOT (d = ?))" 125]
["SELECT * FROM v"]
["SELECT * FROM v WHERE (? OR (? = ?))" true 125 -27283]
["SELECT * FROM v WHERE (c = b)"]
["INSERT INTO t (b, a, d, c) VALUES (?, ?, ?, ?)" "YG7u0XK99YwXoBv3M3S2VPg2G" "ziq" nil "ziq"]
["SELECT * FROM t WHERE ((? = ?) = d)" "REAE" "AO5Aswl0LT2ta75afiNWffe_lBa7ec6P"]
["UPDATE u SET d = ? WHERE ?" 6480 true]
["UPDATE u SET d = ? WHERE ?" 125 false]
["UPDATE u SET d = ? WHERE ?" 125 false]
["SELECT * FROM u"]
["INSERT INTO v (b, c, d, a) VALUES (?, ?, ?, ?)" 125 1231 false nil]
["INSERT INTO v (d, a, c, b) VALUES (?, ?, ?, ?)" nil true 1231 -891]
["DELETE FROM u"]
["SELECT * FROM t WHERE (? AND (b = b) AND d AND (d = ?))" false true]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "0" nil "N" "g_h2oYzQ00Bec-yOt_dXjLSFcDg"]
["SELECT * FROM v WHERE (c = c)"]
["SELECT * FROM t"]
["INSERT INTO t (b, c, a, d) VALUES (?, ?, ?, ?)" "3OeY5Tj9GuUQAAshI1c" "OQE" "ziq" true]
["INSERT INTO t (b, a, c, d) VALUES (?, ?, ?, ?)" "XIu" "NkQI3jLNQ" "OQE" true]
["INSERT INTO t (d, a, b, c) VALUES (?, ?, ?, ?)" nil "L-etNAwe1FKxS1FlvLCz0Bv" "NRzRpjf" "0"]
["SELECT * FROM t WHERE ?" true]
["SELECT * FROM t"]
["INSERT INTO u (d) VALUES (?)" 1231]
["UPDATE t SET b = ?, a = ?, d = ?, c = ? WHERE ((? = a) = (NOT d))" "F63GxNki6AiPmNZI6w4cDyvd" "Yb0 9Z8nVs" true "ziq" "0"]
["SELECT * FROM u WHERE ((NOT ?) AND (d = ?))" false 1197]
["DELETE FROM u"]
["SELECT * FROM t WHERE d"]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "Y" nil "3Ljw5TMPnBW6wmwm" "YvWQPkvwa4 "]
["DELETE FROM v WHERE ?" true]
["INSERT INTO u (d) VALUES (?)" 16533]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" nil 1231 1231 false]
["UPDATE u SET d = ?" -27283]
["SELECT * FROM t WHERE d"]
["UPDATE v SET d = ?, a = ? WHERE ((? AND a AND a) = ?)" nil false nil false]
["UPDATE u SET d = ?" 1]
["UPDATE v SET d = ?, c = ?, a = ?" true 296920407 false]
["SELECT * FROM v WHERE ((? = ?) OR (a = a))" -86240 -102295390]
["INSERT INTO v (c, b, a, d) VALUES (?, ?, ?, ?)" 1231 125 false nil]
["INSERT INTO t (a, b, c, d) VALUES (?, ?, ?, ?)" "0" "5q4Q-6 1Zj3Qqejy8" "X HfWWU" true]
["INSERT INTO t (b, a, c, d) VALUES (?, ?, ?, ?)" "IQ3 xFue1N84rOesWw" "248abIU6ysZWOZ A2 P" "ziq" nil]
["SELECT * FROM u WHERE ?" true]
["SELECT * FROM t"]
["INSERT INTO u (d) VALUES (?)" 1231]
["INSERT INTO t (a, c, b, d) VALUES (?, ?, ?, ?)" "UZvrdu6Vazv" "ziq" "OQE" false]
["INSERT INTO t (d, a, b, c) VALUES (?, ?, ?, ?)" true "0" "8" "OQE"]
["SELECT * FROM u"]
["INSERT INTO v (c, d, a, b) VALUES (?, ?, ?, ?)" -103738865 nil nil -36596]
["UPDATE u SET d = ? WHERE (d = ?)" 125 125]
["INSERT INTO v (b, a, d, c) VALUES (?, ?, ?, ?)" 125 nil nil 145230]
["SELECT * FROM u WHERE (? = ?)" -27283 -27283]
["SELECT * FROM t WHERE ?" false]
["SELECT * FROM t WHERE (? AND (? AND d AND d AND ?) AND ((? = ?) OR ? OR (? = ?) OR (NOT d)))" false nil nil false false false false true]
["UPDATE t SET d = ?, b = ? WHERE ?" nil "bMMXhQ_AxvKOU 3UcT_68ITEBhRgxY9" nil]
["SELECT * FROM t WHERE (d AND (a = c))"]
["SELECT * FROM v WHERE d"]
["UPDATE t SET d = ?, a = ? WHERE (((? = d) AND (d OR ? OR d) AND (? = ?) AND (? = ?)) OR (NOT (d = ?)) OR (b = ?))" false "OQE" true true nil false "pTwR3nj" "G3r1C64ga9 8YTHWHeFpEnGYaD q" true " P9u"]
["UPDATE u SET d = ? WHERE (? = ?)" 1231 125 -28043602]
              ]
             (mapv sql (:statements case)))))))

(defn samples
  "Takes 10 deterministic samples from a generator of SQL expressions. Returns
  a vector of SQL vectors."
  [g]
  (->> (range) ; Seeds
       (map (partial g/generate g 30))
       (take 10)
       (mapv sql)))

(deftest gen-expr-leaf-test
  (let [t1 (table "t1"
                  [(column "int1" integer-type)
                   (column "int2" integer-type)
                   (column "text1" text-type)])
        schema (schema [t1] (g/generate (gen-vpool {}) 10 0))]
    (testing "ints"
      (is (= [["?" -1]
              "int2"
              "int2"
              "int2"
              "int2"
              ["?" -20]
              ["?" -25105412]
              "int2"
              "int1"
              ["?" 0]]
             (samples (gen-expr-leaf {} schema t1 integer-type)))))
    (testing "text"
      (is (= [["?" "V43__l5PWbnXKaQL31Zc5a"]
              "text1"
              "text1"
              "text1"
              "text1"
              ["?" "lMBgHlM7Tiy"]
              ["?" "v8eetQAnmdo-PjyAckDi"]
              "text1"
              "text1"
              ["?" "t2-Ma8p0pw"]]
             (samples (gen-expr-leaf {} schema t1 text-type)))))
    (testing "booleans"
      (is (= [["?" nil]
              ["?" false]
              ["?" false]
              ["?" true]
              ["?" false]
              ["?" false]
              ["?" nil]
              ["?" false]
              ["?" false]
              ["?" nil]]
             (samples (gen-expr-leaf {} schema t1 boolean-type)))))))

(deftest gen-expr-boolean-test
  (let [t1 (table "t1"
                  [(column "int1" integer-type)
                   (column "int2" integer-type)
                   (column "text1" text-type)])
        schema (schema [t1] (g/generate (gen-vpool {}) 10 0))
        g (gen-expr-boolean {} schema t1)]
    (is (= [
            ["?" true]
            ["(((text1 = ?) OR (? = int2)) OR ?)" "Gf" 7 false]
            ["(? = ?)" 1 1]
            ["(? = int2)" 7]
            ["((text1 = ?) OR (? AND ?) OR ?)"
             "emWLIIiI6WLXbOkHD1pGuA3appu"
             true
             true
             nil]
            ["?" nil]
            ["?" true]
            ["((? OR ? OR (NOT ?)) OR ?)" true false false false]
            ["(NOT ?)" false]
            ["?" false]
            ]
           (samples g)))))
