;;; ebookify-mode.el --- Creates ebooks              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Sébastien Le Callonnec

;; Author: Sébastien Le Callonnec <sebastien@weblogism.com>
;; Keywords:
;; Require-Packages: ((mongo-el "0.0.0") (s "1.11.0")

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

;; Create an eBook from a collection of documents in a known format.

;;; Code:

(require 'mongo)
(require 's)

(defgroup ebookify nil
  "ebookify parameters"
  :group 'applications)

(defcustom ebookify-mongo-collection
  "nouvelles_development.nouvelle"
  "Name of the MongoDB collection to get documents from"
  :type 'string
  :group 'ebookify)

(defcustom ebookify-mongo-search-field
  "num"
  "Field used to find document in `ebookify-mongo-collection'"
  :type 'string
  :group 'ebookify)

(defcustom ebookify-output-directory
  "/tmp"
  "Directory where output files are created"
  :type 'string
  :group 'ebookify)

(defcustom ebookify-pandoc-executable
  "/usr/bin/pandoc"
  "Path to Pandoc executable"
  :type '(file :must-match t)
  :group 'ebookify)

(defcustom ebookify-document-format
  "html"
  "Original format of the document"
  :type 'string
  :group 'ebookify)

(defstruct document title body num)

;; query 	BSON document that represents the query. The query will contain one or more elements,
;; all of which must match for a document to be included in the result set. Possible elements include
;; $query, $orderby, $hint, $explain, and $snapshot.

;; (defun ebookify--mongo-build-query (docids)
;;   (cond
;;    ((listp docids) (append (list "$or") (mapcar (lambda (d) `(,ebookify-mongo-search-field . ,d)) docids)))
;;    ((stringp docids) `(,ebookify-mongo-search-field . ,docids))))

(setq debug-on-error t)


;; TODO: support different backends for documents.
(defun ebookify--fetch-document (doc-nums)
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

(defun ebookify--docfile (document extension)
  (concat "/tmp/" (document-num document) extension))

(defun ebookify--store-document (document)
  (write-region (document-body document) nil (ebookify--docfile document ".html") 'append))

(defun ebookify--read-file-to-string (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun ebookify--convert-to-tex (document)
  (let ((docfile (ebookify--docfile document ".html")))
    (shell-command-to-string (format "%s -f %s -t latex -o %s %s" ebookify-pandoc-executable ebookify-document-format
                                     (ebookify--docfile document ".tex") docfile))))

(defun ebookify--read-template (template-name)
  (let* ((current-script (expand-file-name "~/dev/ebookify/ebookify-mode.el"))
         (template-path (concat (file-name-directory current-script) "/templates/" template-name)))
    (ebookify--read-file-to-string template-path)))

(defun ebookify--expand-section-template (document template)
  (s-replace ":body:" (ebookify--read-file-to-string (ebookify--docfile document ".tex"))
             (s-replace ":title:" (document-title document) template)))

(defun ebookify--expand-main-template (title author sections template)
  (s-replace ":sections:" sections
             (s-replace ":author:" author
                        (s-replace ":title:" title template))))


(defun ebookify--build-source (title author documents)
  (let ((main-template (ebookify--read-template "main.txt"))
        (section-template (ebookify--read-template "section.txt"))
        (sections nil))
    (setq sections (mapconcat (lambda (doc)
                                (ebookify--expand-section-template doc section-template)) documents ""))
    (write-region
     (ebookify--expand-main-template title author sections main-template) nil "/tmp/main.tex" nil nil nil t)))

(defun ebookify--run-tex4ebook ()
  (shell-command-to-string (format "cd %s && tex4ebook main.tex -f %s" ebookify-output-directory "mobi")))

(defun ebookify-create-ebook (doc-ids)
  (interactive)
  (let ((docs (ebookify--fetch-document doc-ids)))
    (mapcar #'ebookify--store-document docs)
    (mapcar #'ebookify--convert-to-tex docs)
    (ebookify--build-source "Title" "Moi" docs)
    (ebookify--run-tex4ebook)))

(provide 'ebookify-mode)
;;; ebookify-mode.el ends here
