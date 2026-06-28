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
                   [(column "b" integer-type {:primary-key? true})])])
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
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" nil -12 176 false]
["DELETE FROM t"]
["SELECT * FROM v"]
["SELECT * FROM u WHERE (d = ?)" -66360484]
["INSERT INTO u (d) VALUES (?)" -1]
["INSERT INTO u (d) VALUES (?)" 959823]
["UPDATE v SET a = ?, b = ?, d = ?, c = ? WHERE (? = b)" true -3 nil 1072218635 -1495720]
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" nil -300 -15583 true]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" true -2 nil 229]
["SELECT * FROM v WHERE ?" nil]
["UPDATE v SET a = ?" nil]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" true 3870 -4395 false]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" true 7032 true -812134460]
["UPDATE u SET d = ?" -18549318]
["DELETE FROM u WHERE ((d = d) AND (NOT ?) AND (NOT (? = ?)))" nil 4416307 1949]
["SELECT * FROM t"]
["INSERT INTO t (c, d, b, a) VALUES (?, ?, ?, ?)" " ne1aClAXIxCEeocMg-DFLpX" false "FyfSYCe8" "XwykEcTkxTGi7mTvP-Cs-ZjzQTuL4Nb _"]
["SELECT * FROM v WHERE a"]
["INSERT INTO u (d) VALUES (?)" 8]
["SELECT * FROM u"]
["UPDATE v SET a = ?, b = ? WHERE (? = ?)" false -31221704 -22525 -1884]
["INSERT INTO t (c, a, b, d) VALUES (?, ?, ?, ?)" "ig" "fAKqKtfbtCm1hzYKjSaNzB1Q_3LCrsQ" "I-PH qhp1EszQE3FH2CnMXQbq44wPXV" nil]
["UPDATE t SET b = ?, d = ? WHERE (NOT ?)" "fA7E8orQI4A0Wa" true false]
["SELECT * FROM v"]
["UPDATE v SET a = ?, b = ?, d = ? WHERE ?" false 0 true true]
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" true -635928 0 true]
["SELECT * FROM t WHERE (? = ?)" "CRWp-mcR4aN0NV4_f9fJ" "356gfan0e_"]
["SELECT * FROM t"]
["SELECT * FROM u WHERE ?" nil]
["INSERT INTO u (d) VALUES (?)" -5887]
["UPDATE t SET b = ?, a = ?, c = ?, d = ? WHERE (d = (NOT d))" "c-XRTXQlYF tS6AR81H0u4j0dcnZVHU" "ESbp" "SeHBcwRK-3hog_c wC98qMOKGUiEk" nil]
["SELECT * FROM u"]
["INSERT INTO u (d) VALUES (?)" -25]
["SELECT * FROM v WHERE (? = ?)" false true]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "VnXhwCxqZbXJOXQeGDeG" nil "N M199CTt5jiHfACB" "UkhdPvLrgU3F_ XVBFn1SI"]
["INSERT INTO u (d) VALUES (?)" 9]
["SELECT * FROM t"]
["INSERT INTO t (b, a, d, c) VALUES (?, ?, ?, ?)" "z4Tl -DU1_1Wy" "juqE5K_YqLiaq-" true "QFQw"]
["SELECT * FROM v WHERE a"]
["UPDATE v SET c = ?" 1]
["SELECT * FROM u WHERE (NOT ((? = d) AND ? AND (d = ?) AND (d = d)))" 8 true 5156]
["INSERT INTO t (d, b, a, c) VALUES (?, ?, ?, ?)" nil "g-QFLFfvmPOSAhydBQa0q" "UP3GSf2gmS5H8zlReV-ebow7ip5e" "Xi18gKMXYnMbe-UN6WEZfx"]
["UPDATE v SET b = ? WHERE a" 139]
["INSERT INTO t (c, d, b, a) VALUES (?, ?, ?, ?)" "aCx-b P4YaAm" nil "mt0-59P RRoTp2BFrEG-HsJ7 -" "aA"]
["SELECT * FROM v WHERE ?" false]
["INSERT INTO v (b, d, a, c) VALUES (?, ?, ?, ?)" 62858 true false -1]
["UPDATE t SET c = ?, d = ? WHERE ?" "0W-" false false]
["INSERT INTO t (a, b, c, d) VALUES (?, ?, ?, ?)" "77YwkTJw7TTwfXcs2" "ppM7rHLBfwNfor6h6uxUZZhAoTUo1J7" "75nc077IVn8Y33cxerCw-" false]
["SELECT * FROM t"]
["SELECT * FROM u WHERE (? = d)" 6]
["INSERT INTO u (d) VALUES (?)" 5761598]
["UPDATE u SET d = ? WHERE ((? = d) OR (d = ?) OR (d = ?) OR ?)" 2155 40 0 1605731740 true]
["UPDATE u SET d = ? WHERE ?" -10467 nil]
["SELECT * FROM t WHERE d"]
["UPDATE u SET d = ? WHERE ?" 49 false]
["UPDATE u SET d = ? WHERE ?" -21167060 nil]
["SELECT * FROM t WHERE (b = a)"]
["SELECT * FROM u WHERE ((d = ?) OR ((? AND ?) OR ?))" -326 true nil false]
["SELECT * FROM t"]
["UPDATE u SET d = ?" -91773]
["SELECT * FROM t WHERE ?" nil]
["UPDATE v SET a = ?, b = ?, c = ?, d = ? WHERE (((? = ?) AND ? AND (b = ?) AND (c = ?)) OR ? OR ? OR ?)" nil 3550 -9 nil false nil nil -1680 -26 nil nil true]
["DELETE FROM t WHERE ((? = ?) = (? OR d OR ? OR d))" "xnoekHFDjabKyUFsQK-A2T7hO2PX" "PQBvA8OhNMH-EzPmrhGAq4ZJ_wrko5" false true]
["INSERT INTO u (d) VALUES (?)" -28388138]
["UPDATE t SET c = ?, b = ?, d = ?, a = ? WHERE (NOT d)" "0lfuwa8truhkDWhVBpL6JFnMvGi8-NQS" "Pj4ATSoBPdin" true "eiBcItk"]
["SELECT * FROM t WHERE d"]
["SELECT * FROM t WHERE (b = ?)" "v5_P"]
["SELECT * FROM u"]
["SELECT * FROM t WHERE (d = (d AND ? AND ?))" nil true]
["SELECT * FROM v WHERE (? OR ((c = ?) OR (? = d) OR (? OR ?) OR (b = c)) OR ? OR (? = ?))" false 565583 nil true false true true nil]
["INSERT INTO v (a, c, b, d) VALUES (?, ?, ?, ?)" false 93196 -4 false]
["INSERT INTO u (d) VALUES (?)" 108177207]
["SELECT * FROM v WHERE (((a OR d OR d) AND (NOT a) AND (? = d) AND (NOT ?)) OR ((d = ?) AND ?))" false nil false false]
["SELECT * FROM u WHERE (d = d)"]
["INSERT INTO t (a, d, c, b) VALUES (?, ?, ?, ?)" "Y7AAytwuuwVOyxz8J0H ZRGGvdBfBa" nil "Zcg" "uTjbF EZQVuFaFP"]
["INSERT INTO u (d) VALUES (?)" 5198]
["UPDATE t SET a = ?, c = ? WHERE d" "V 3ZNQnw6WiQL5UNnc8UWlEh2MFr" "rNo0WfAS8ZAXza3NdNvAUgqN7w"]
["SELECT * FROM u WHERE ((NOT (d = d)) AND ?)" nil]
["INSERT INTO v (c, a, b, d) VALUES (?, ?, ?, ?)" -210892849 false 7 true]
["SELECT * FROM v WHERE ?" false]
["SELECT * FROM u WHERE ?" true]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "Q0lYEUV6xjqOg" true "lKj7eTQJyo6kkhdF pSST-QF7BcjTq7x" "1E6d1tlhPldtf9uUyAPQ5FJVLz3VI4ALz"]
["INSERT INTO t (d, c, b, a) VALUES (?, ?, ?, ?)" true "tZaV5MthUu9qZJfVh6rrKiIsyNmj3Y" "QLKI _22jdf0MPAUpBUQNp- " "Ogwb63_bRRiye"]
["UPDATE t SET a = ?, d = ?, b = ?, c = ? WHERE (NOT d)" "gWSqr I3SVVwdCJnOUC2rr" nil "3sEYSRlezZ4Q-ez7FiE_tH1_gmLITJ0Gt" "jdrxVF2m59Re4n-kPkEyX0X"]
["UPDATE u SET d = ? WHERE (d = d)" -120103855]
["SELECT * FROM v"]
["SELECT * FROM u WHERE (d = d)"]
["INSERT INTO v (b, d, c, a) VALUES (?, ?, ?, ?)" -22738072 nil 2426 true]
["SELECT * FROM v WHERE ((a = a) = ?)" false]
["SELECT * FROM t WHERE d"]
["UPDATE u SET d = ?" -38372]
["INSERT INTO v (c, a, d, b) VALUES (?, ?, ?, ?)" -8 false false -323]
["SELECT * FROM v WHERE (b = b)"]
["INSERT INTO u (d) VALUES (?)" 19527837]
["SELECT * FROM u WHERE ?" nil]
["SELECT * FROM u WHERE (? OR (d = d) OR (? = ?))" true -195244 -882373]
["INSERT INTO u (d) VALUES (?)" 46]
["SELECT * FROM u"]
["SELECT * FROM u"]
["UPDATE u SET d = ? WHERE (? = ?)" 4 -2205448 -3695]
["SELECT * FROM t WHERE ?" nil]
["UPDATE t SET c = ?, a = ? WHERE ((? = ?) = (d = d))" "JTvQiiAKeL PodKNX3 QTJE-YEYjvYtt" "2y0dlLHU7NB_iQi-TZuI" "d2Uv757rM7Z7UCq9nyere" "bkB"]
["SELECT * FROM u WHERE (? = d)" -6369]
["INSERT INTO t (c, a, b, d) VALUES (?, ?, ?, ?)" "DM5X3ZiNToOUuAKEYSTrc2 1mHGhS" "cfrpNXL0_lnaG9kza8xJFhbJQuXEQlC A" "0zeOAkQOLYkQjxQGMwIn7TL7nS82D4G" nil]
["INSERT INTO v (c, a, d, b) VALUES (?, ?, ?, ?)" -524255010 false false 231846]
["SELECT * FROM t WHERE ((? AND d AND ?) = d)" true false]
["INSERT INTO v (b, a, c, d) VALUES (?, ?, ?, ?)" 126194123 false 116 nil]
["UPDATE v SET b = ?, d = ?, a = ? WHERE (NOT d)" 36262938 true false]
["SELECT * FROM t WHERE ?" nil]
["SELECT * FROM u WHERE ?" nil]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" false -508648 nil -152]
["UPDATE u SET d = ? WHERE (? = d)" -12959 6]
["SELECT * FROM t WHERE ((c = ?) AND d AND (NOT d) AND (d AND (d OR d)))" "L6wtUjq81KCKxepP6QjqOHFpg2"]
["INSERT INTO v (b, c, a, d) VALUES (?, ?, ?, ?)" -1184768135 3471 true nil]
["INSERT INTO t (c, d, b, a) VALUES (?, ?, ?, ?)" "_Z WmgfY-EAC" false "ib2zrJb" "P4fjK0dOrB_xbdZHUA"]
["INSERT INTO u (d) VALUES (?)" 1]
["SELECT * FROM v WHERE (b = b)"]
["UPDATE t SET b = ?" "o "]
["INSERT INTO v (b, a, c, d) VALUES (?, ?, ?, ?)" 50029 nil -920534 false]
["SELECT * FROM u WHERE (? = d)" 55650]
["SELECT * FROM u WHERE ((? = ?) OR ((NOT ?) AND (NOT ?) AND (NOT ?) AND (NOT ?)) OR ((d = d) AND (? OR ? OR ? OR ?) AND (d = ?) AND ?) OR (? = d))" -58 5803 false true true true nil false false false 2442 false 64100756]
["SELECT * FROM u WHERE (? = ?)" -3161928 9836762]
["INSERT INTO t (a, d, c, b) VALUES (?, ?, ?, ?)" "khY40FCNdTsKuy_Ds_jMc0lJUbHe " false "e9FEe" "keRp9U7jXZTPYrHOo"]
["SELECT * FROM v WHERE (? AND (? = ?))" false nil false]
["UPDATE t SET c = ?, b = ?, a = ?, d = ?" "27rERS_kDdyEejAzhNScSkuxpJ9" "xghYxtz9wMmu93sz0qGiVhGd" "mLmINbNFzGfFFQ1 NkyB g5U7Y50o 2sK" false]
["UPDATE t SET c = ?, d = ? WHERE ?" "kKnBHh" false false]
["INSERT INTO v (b, d, c, a) VALUES (?, ?, ?, ?)" 849762225 true -4654276 false]
["UPDATE t SET b = ?, a = ?, d = ?, c = ? WHERE (d = ?)" "OQ8Tmy89dF8DuQ5F5RWKtCc" "n_pwCrDe9iO24M-DMI9Kf-06EXVktYr" nil "ZZEI6M3ifw30bwDsxeS 8d5Jv" nil]
["UPDATE t SET b = ?, a = ? WHERE ((? OR d OR d) = (? OR ? OR ? OR d))" "JrSOXkaW7nnO4AaRT" "prhzJBwk8" nil true false nil]
["UPDATE v SET a = ?, d = ? WHERE (? = (NOT ?))" nil false nil true]
["INSERT INTO v (d, a, b, c) VALUES (?, ?, ?, ?)" true true -115 70]
["INSERT INTO u (d) VALUES (?)" -874]
["INSERT INTO t (c, a, b, d) VALUES (?, ?, ?, ?)" "A3mh0-W" "aQdUbMoZqwjSGArW3uHAjGkbY" "7JvrPFoHdV40uka8gOAeD" true]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" false 33 -2685 nil]
["UPDATE u SET d = ? WHERE (? = d)" -5811374 -32051639]
["SELECT * FROM v WHERE ?" nil]
["SELECT * FROM v WHERE (? = (? OR d OR d))" nil nil]
["INSERT INTO v (d, c, a, b) VALUES (?, ?, ?, ?)" nil -506073 true 24992]
["SELECT * FROM v"]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "MghSzVV1yWpeni5XLzRj0Sp IteU" false "dBN4cNo" "Z8NMXoWQPgdMtv"]
["INSERT INTO u (d) VALUES (?)" 378]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" false -486627900 430 true]
["SELECT * FROM t"]
["UPDATE t SET c = ?, b = ? WHERE ((? OR (NOT d) OR (? AND ?) OR (d = d)) OR (d = d) OR d)" "SaIHbsmmnIyy" "eJadfP jeeu86Cf4Xe" true false nil]
["SELECT * FROM u WHERE (NOT ?)" false]
["SELECT * FROM u WHERE (NOT (d = ?))" 304210]
["SELECT * FROM v"]
["SELECT * FROM v WHERE (? OR (? = ?))" true -367345 -358686048]
["SELECT * FROM v WHERE (c = b)"]
["INSERT INTO t (b, a, d, c) VALUES (?, ?, ?, ?)" "BR009Kf0Y6TY" "5" nil "a_It1"]
["SELECT * FROM t WHERE ((? = ?) = d)" " " "5afvranhxp5W3"]
["UPDATE u SET d = ? WHERE ?" 32714733 true]
["UPDATE u SET d = ? WHERE ?" 15 false]
["UPDATE u SET d = ? WHERE ?" -1 false]
["SELECT * FROM u"]
["INSERT INTO v (b, c, d, a) VALUES (?, ?, ?, ?)" -7 16169 false nil]
["INSERT INTO v (d, a, c, b) VALUES (?, ?, ?, ?)" nil true -678 -2898726]
["DELETE FROM u"]
["SELECT * FROM t WHERE (? AND (b = b) AND d AND (d = ?))" false true]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "5Z5oF1yQ4jCuamvd2pSzqZ" nil " o1QKVzz0AZ2h xbf" "mUPPklCcfB6tJ5xuMCG-owfx2wQP"]
["SELECT * FROM v WHERE (c = c)"]
["SELECT * FROM t"]
["INSERT INTO t (b, c, a, d) VALUES (?, ?, ?, ?)" "6Xfu8eHL8PeJvSX83npjJhiQ0" "zkmu3xvdWVmAase-A2tJ6GD_" "wcGub" true]
["INSERT INTO t (b, a, c, d) VALUES (?, ?, ?, ?)" "H0YdgzosksxYCxpUNjCKgYEvtJP2EGN" "wIBlZvxRX7LzLX41p4g" "E2_eD2rj11faaPQBx6sKPeJBd" true]
["INSERT INTO t (d, a, b, c) VALUES (?, ?, ?, ?)" nil "XswlIfaMosHdk-rbi48Hm5jMC ztjVar" "5Wmrld3k" "qnOCsL1W EKyQhvUQxs-fgzNu87d_PfdZ"]
["SELECT * FROM t WHERE ?" true]
["SELECT * FROM t"]
["INSERT INTO u (d) VALUES (?)" -19157]
["UPDATE t SET b = ?, a = ?, d = ?, c = ? WHERE ((? = a) = (NOT d))" "d_Vjulsc08yknWAUIVP_9t_j25CN4q" "vz_tqtjaU3We oVc-V1YOiY7" true "K-VJuU7r-b6drFPrbHVDG-VI7 nOPY" "zeo_-bXFaKAnqhF8-zUqQGSojH_xBj"]
["SELECT * FROM u WHERE ((NOT ?) AND (d = ?))" false -3]
["DELETE FROM u"]
["SELECT * FROM t WHERE d"]
["INSERT INTO t (b, d, c, a) VALUES (?, ?, ?, ?)" "jaAUrdnJ7VjFah" nil "7DbS" "LP0riuD"]
["DELETE FROM v WHERE ?" true]
["INSERT INTO u (d) VALUES (?)" 238683]
["INSERT INTO v (a, b, c, d) VALUES (?, ?, ?, ?)" nil -175 -489151 false]
["UPDATE u SET d = ?" 311065074]
["SELECT * FROM t WHERE d"]
["UPDATE v SET d = ?, a = ? WHERE ((? AND a AND a) = ?)" nil false nil false]
["UPDATE u SET d = ?" 203751]
["UPDATE v SET d = ?, c = ?, a = ?" true -16102 false]
["SELECT * FROM v WHERE ((? = ?) OR (a = a))" 6579 129960783]
["INSERT INTO v (c, b, a, d) VALUES (?, ?, ?, ?)" -376 509107 false nil]
["INSERT INTO t (a, b, c, d) VALUES (?, ?, ?, ?)" "SMtE_99XMKoeQCTEOk_-fLiM5rZZ0Nv" "PvK083-z3AteoOmdv" "0YxAgZWpKD2evP" true]
["INSERT INTO t (b, a, c, d) VALUES (?, ?, ?, ?)" "0k2WH7oqrutUaSoCYMJVsKcATyW5iS" "4O_Gr" "mTA82IoQ" nil]
["SELECT * FROM u WHERE ?" true]
["SELECT * FROM t"]
["INSERT INTO u (d) VALUES (?)" -2]
["INSERT INTO t (a, c, b, d) VALUES (?, ?, ?, ?)" "LiTlvmVc0pC_HuALVTQDYx11n8" "-uIQpNEhiscUEifmQ" " vZeFRS4jIMnom" false]
["INSERT INTO t (d, a, b, c) VALUES (?, ?, ?, ?)" true "SlZSxafZ-YMx" "LzbDkV2x2G71hEY4PgRT4rQ" "HOQg7k_ xNXYx7Goqrk0tnP"]
["SELECT * FROM u"]
["INSERT INTO v (c, d, a, b) VALUES (?, ?, ?, ?)" 3 nil nil -1]
["UPDATE u SET d = ? WHERE (d = ?)" 230 -21]
["INSERT INTO v (b, a, d, c) VALUES (?, ?, ?, ?)" 47 nil nil -815]
["SELECT * FROM u WHERE (? = ?)" 0 -7972922]
["SELECT * FROM t WHERE ?" false]
["SELECT * FROM t WHERE (? AND (? AND d AND d AND ?) AND ((? = ?) OR ? OR (? = ?) OR (NOT d)))" false nil nil false false false false true]
["UPDATE t SET d = ?, b = ? WHERE ?" nil "Ro yY01-ZDFj3wQIeTl" nil]
["SELECT * FROM t WHERE (d AND (a = c))"]
["SELECT * FROM v WHERE d"]
["UPDATE t SET d = ?, a = ? WHERE (((? = d) AND (d OR ? OR d) AND (? = ?) AND (? = ?)) OR (NOT (d = ?)) OR (b = ?))" false "JM05tORqmGtfy" true true nil false "q4MjMdYoJ1aA" "Sf" true "PtSpYMd135WnOB QCgvExTmA_TaFOmi"]
["UPDATE u SET d = ? WHERE (? = ?)" 45219329 -3056978 3013112]
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
        schema (schema [t1])]
    (testing "ints"
      (is (= [["?" -15938654] "int2" "int2" "int2" "int2" ["?" 5] ["?" -96] "int2" "int1" ["?" 121277]]
             (samples (gen-expr-leaf {} schema t1 integer-type)))))
    (testing "text"
      (is (= [["?" "yU5acRx88p3QHl47C8HDHuaWEU_t3My"]
              "text1"
              "text1"
              "text1"
              "text1"
              ["?" "3-xg0f2Bx4tUp2A1ccN2 8o8YnK"]
              ["?" "gbA6ko  BKSp6xTOANpUXtTt9i"]
              "text1"
              "text1"
              ["?" "t8Rg"]]
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
        schema (schema [t1])
        g (gen-expr-boolean {} schema t1)]
    (is (= [
            ["?" true]
            ["(((text1 = ?) OR (? = int2)) OR ?)"
             "xh2"
             20035960
             false]
            ["(? = ?)" 54 37]
            ["(? = int2)" 258006]
            ["((text1 = ?) OR (? AND ?) OR ?)"
             "pE2E5k_ptyZcA1_Avnlww"
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
