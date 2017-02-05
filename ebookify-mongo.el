;;; ebookify-mongo.el --- MongoDB backend for ebookify  -*- lexical-binding: t; -*-

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

;; MongoDB backend for ebookify.

;;; Code:

(require 'ebookify-doc)
(require 'mongo)

(defcustom ebookify-mongo-collection
  "nouvelles_development.nouvelle"
  "Name of the MongoDB collection to get documents from."
  :type 'string
  :group 'ebookify)

(defcustom ebookify-mongo-search-field
  "num"
  "Field used to find document in `ebookify-mongo-collection'."
  :type 'string
  :group 'ebookify)

;; TODO: support different backends for documents.
;; FIXME: The expected fields are expected to be stored in `title', `body', `num'
(defun ebookify-mongo--fetch-document (doc-nums)
  "Retrieve documents that will be part of the ebook."
  (mapcar (lambda (d)
            (let* ((query `((,ebookify-mongo-search-field . ,d)))
                   (result
                    (mongo-with-open-database
                        (db :host 'local)
                      (mongo-do-request
                       (make-mongo-message-query
                        :flags 0
                        :number-to-skip 0
                        :number-to-return 0
                        :full-collection-name ebookify-mongo-collection
                        :query query)
                       :database db)))
                   (docres (mongo-message-reply-documents result))
                   (doc (car docres)))
              (make-document :title (cdr (assoc-string "title" doc))
                             :body (cdr (assoc-string "body" doc))
                             :num (cdr (assoc-string "num" doc)))))
          doc-nums))


(provide 'ebookify-mongo)
;;; ebookify-mongo.el ends here
