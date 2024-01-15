;;; database.el -*- lexical-binding: t; -*-

;; test db
(sql-add-postgres-db
 postgres
 :port 5432
 :user "postgres"
 :host "localhost"
 :database "postgres"
 :password "postgres")


(sql-add-postgres-db
 rt_integration
 :port 5432
 :user "root"
 :host "localhost"
 :database "rt_integration"
 :password "root")
