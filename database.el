;;; database.el -*- lexical-binding: t; -*-

; test db
(sql-add-postgres-db
 localhost
 :port 5432
 :user "postgres"
 :host "localhost"
 :database "postgres"
 :password "postgres")
