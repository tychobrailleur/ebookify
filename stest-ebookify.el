(let* ((result
        (mongo-with-open-database
            (db :host 'local)
          (mongo-do-request
           (make-mongo-message-query
            :flags 0
            :number-to-skip 0
            :number-to-return 0
            :full-collection-name "nouvelles_development.nouvelle"
            ;;            :query '("$or" ("num" . "06905") ("num" . "17738")))
            :query '("$or" ("title" . "Marine") ("title" . "Emma")))
;;            :query '("$query" ("$or" ("title" . "Marine") ("title" . "Emma"))))
           :database db)))
       (docres (mongo-message-reply-documents result)))
  (mapc (lambda (d) (message "Resultat = %s (%s)" (assoc-string "title" d) (assoc-string "num" d))) docres))
