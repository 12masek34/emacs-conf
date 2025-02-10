;;; database.el -*- lexical-binding: t; -*-

; test db
(sql-add-postgres-db
 localhost
 :port 5432
 :user "postgres"
 :host "localhost"
 :database "postgres"
 :password "postgres")

(sql-add-postgres-db
 monita_test
 :port 5432
 :user "martys"
 :host "192.168.0.142"
 :database "prod-kb-monita"
 :password "mTG54QhPg2IzSyzK")

(sql-add-postgres-db
 monita_pricing
 :port 5432
 :user "postgres"
 :host "192.168.0.142"
 :database "pricing"
 :password "Itc4HSRe81oxwdbF")

(sql-add-postgres-db
 game_server
 :port 5432
 :user "game_user"
 :host "localhost"
 :database "game"
 :password "game_pass")
