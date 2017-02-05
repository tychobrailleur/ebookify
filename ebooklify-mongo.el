;;; ebooklify-mongo.el --- MongoDB backend for ebooklify  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sébastien Le Callonnec

;; Author: Sébastien Le Callonnec <sebastien@weblogism.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; MongoDB backend for ebooklify.

;;; Code:

(require 'ebooklify-doc)
(require 'mongo)

(defgroup ebooklify-mongo nil
  "ebooklify mongodb parameters"
  :group 'ebooklify)

(defcustom ebooklify-mongo-collection
  "nouvelles_development.nouvelle"
  "Name of the MongoDB collection to get documents from."
  :type 'string
  :group 'ebooklify-mongo)

(defcustom ebooklify-mongo-search-field
  "num"
  "Field used to find document in `ebooklify-mongo-collection'."
  :type 'string
  :group 'ebooklify-mongo)

(defcustom ebooklify-mongo-fields
  '(("id" "num") ("title" "title") ("body" "body"))
  "Field used for laying out doc from `ebooklify-mongo-collection'."
  :type '(alist
          :key-type (choice :tag "Field"
                            (const :tag "id" id)
                            (const :tag "title" title)
                            (const :tag "body" body))
          :value-type string)
  :group 'ebooklify-mongo)

(defun ebooklify-mongo--get-property-name (property)
  (cadr (assoc-string property ebooklify-mongo-fields)))

(defun ebooklify-mongo--fetch-document (doc-nums)
  "Retrieve documents that will be part of the ebook."
  (mapcar (lambda (d)
            (let* ((query `((,ebooklify-mongo-search-field . ,d)))
                   (result
                    (mongo-with-open-database
                        (db :host 'local)
                      (mongo-do-request
                       (make-mongo-message-query
                        :flags 0
                        :number-to-skip 0
                        :number-to-return 0
                        :full-collection-name ebooklify-mongo-collection
                        :query query)
                       :database db)))
                   (docres (mongo-message-reply-documents result))
                   (doc (car docres)))
              (make-document :title (cdr (assoc-string (ebooklify-mongo--get-property-name "title") doc))
                             :body (cdr (assoc-string (ebooklify-mongo--get-property-name "body") doc))
                             :num (cdr (assoc-string (ebooklify-mongo--get-property-name "id") doc)))))
          doc-nums))


(provide 'ebooklify-mongo)
;;; ebooklify-mongo.el ends here
