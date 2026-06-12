(ns jepsen.sql.base
  "Support functions."
  (:require [clojure.tools.logging :refer [info warn]]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs]
            [jepsen.sql [checker :refer [assert-at-most-one]]]))

(defn indirection
  "Constructs a map representing an indirection table. Indirection tables
  provide a layer of indirection, wrapping another table; they let us force the
  database to do things like joins.

  Takes a test, the name of the indirection table, the name of the target
  table, and the name of the integer id column in the target table."
  [test indirection-table target-table target-id-col]
  {:indirection-table         indirection-table
   ; TODO: would be nice to do secondary key lookups here too, to stress
   ; predicates.
   :indirection-id-col        "id"
   :indirection-target-id-col "target_id"
   :target-table              target-table
   :target-id-col             target-id-col})

(defn create-indirection-table!
  "Creates an indirection table for looking up rows in another table by integer
  primary key. All keys are integers. Takes a test, connection, and the
  indirection table map. All key types are assumed to be integer. Returns a map
  which describes the indirection table; you'll need this later."
  [test conn
   {:keys [indirection-table
           indirection-id-col
           indirection-target-id-col]}]
  (assert (:indirection? test))
  (j/execute!
    conn
    ; We're intentionally going to avoid using foreign key constraints here;
    ; it's more fun if the database breaks them.
    [(str "CREATE TABLE IF NOT EXISTS " indirection-table " ("
          indirection-id-col        " int primary key, "
          indirection-target-id-col " int)")]))

(defn write-indirection!
  "Upserts an indirection relationship. Takes a test, connection, the
  indirection map, the indirection key, and the target key."
  [test conn
   {:keys [indirection-table
           target-table
           indirection-id-col
           indirection-target-id-col
           target-id-col]}
   indirection-id target-id]
  (assert (:indirection? test))
  ; Just to be weird, we'll do our upsert via a DELETE/INSERT.
  (j/execute! conn [(str "DELETE FROM " indirection-table
                         " WHERE " indirection-id-col " = ?")
                    indirection-id])
  (j/execute!
    conn
    [(str "INSERT INTO " indirection-table
          " (" indirection-id-col ", " indirection-target-id-col ")"
          " VALUES (?, ?)")
     indirection-id
     target-id]))

(defn read-indirection-target-id
  "Takes a test, connection, and indirection, and an indirection ID. Looks up
  the corresponding target id."
  [test conn
   {:keys [indirection-table
           indirection-id-col
           indirection-target-id-col]}
   indirection-id]
  (assert (:indirection? test))
  (let [r (assert-at-most-one
            (j/execute!
              conn
              [(str "SELECT " indirection-target-id-col
                    " FROM " indirection-table
                    " WHERE " indirection-id-col " = ?")
               indirection-id]
              {:builder-fn rs/as-unqualified-lower-maps}))]
    (-> r first (get (keyword indirection-target-id-col)))))

(defn read-indirection-join
  "Reads a single row of the target table via an indirection table, using a
  query of the form

  SELECT target.* FROM indirection
  INNER JOIN target ON indirection.target_id = target.id
  WHERE indirection.id = indirection_id

  Takes a test, connection, indirection map, and the indirection id to look up.
  Returns a single row."
  [test conn
   {:keys [indirection-table
           target-table
           indirection-id-col
           indirection-target-id-col
           target-id-col]}
   indirection-id]
  (-> conn
      (j/execute!
        [(str "SELECT " target-table ".* FROM " indirection-table
              " INNER JOIN " target-table " ON "
              indirection-table "." indirection-target-id-col " = "
              target-table "." target-id-col
              " WHERE " indirection-table "." indirection-id-col " = ?")
         indirection-id]
        {:builder-fn rs/as-unqualified-lower-maps})
      assert-at-most-one
      first))

(defn read-indirection
  "Reads a single row of the target table via an indirection table. Takes a
  test, connection, indirection map, and the indirection id to look up."
  [test conn indirection indirection-id]
  (assert (:indirection? test))
  (read-indirection-join test conn indirection indirection-id))
