;;; database.el -*- lexical-binding: t; -*-

;; test db
(sql-add-postgres-db
 postgres
 :port 5432
 :user "postgres"
 :host "localhost"
 :database "postgres"
 :password "1234")

(sql-add-postgres-db
 ranked_choice
 :port 5432
 :user "rc_admin"
 :host "localhost"
 :database "ranked_choice"
 :password "rc1234")