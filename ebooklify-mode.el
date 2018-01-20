;;; ebooklify-mode.el --- Creates ebooks              -*- lexical-binding: t; -*-

;; Copyright (C) 2017,2018  Sébastien Le Callonnec

;; Author: Sébastien Le Callonnec <sebastien@weblogism.com>
;; Version: 0.1.0
;; Keywords:
;; Require-Packages: ((mongo-el "0.0.0") (s "1.11.0"))

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
(require 's)
(require 'ebooklify-doc)
(require 'ebooklify-mongo)

(defgroup ebooklify nil
  "ebooklify parameters"
  :group 'applications)

(defcustom ebooklify-document-backend
  "mongo"
  "Backend storing the documents to be added to the ebook."
  :type 'string
  :group 'ebooklify)

(defcustom ebooklify-output-directory
  "/tmp"
  "Directory where output files are created."
  :type 'directory
  :group 'ebooklify)

(defcustom ebooklify-output-document-format
  'mobi
  "eBook format."
  :type '(choice (const :tag "epub" epub)
                 (const :tag "epub3" epub3)
                 (const :tag "mobi" mobi))
  :group 'ebooklify)

(defcustom ebooklify-pandoc-executable
  "/usr/bin/pandoc"
  "Path to Pandoc executable."
  :type '(file :must-match t)
  :group 'ebooklify)

(defcustom ebooklify-document-format
  "html"
  "Original format of the document."
  :type 'string
  :group 'ebooklify)

(defvar ebooklify--template-directory nil)
(setq ebooklify--template-directory
      (when load-file-name
        (expand-file-name "templates" (file-name-directory load-file-name))))

(defun ebooklify--docfile (document extension)
  (expand-file-name (concat (document-num document) extension) ebooklify-output-directory))

(defun ebooklify--store-document (document)
  (write-region (document-body document) nil (ebooklify--docfile document ".html") nil nil nil t))

(defun ebooklify--read-file-to-string (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun ebooklify--convert-to-tex (document)
  "Convert the document format to TeX."
  (let ((docfile (ebooklify--docfile document ".html")))
    (shell-command-to-string
     (format "%s -f %s -t latex -o %s %s"
             ebooklify-pandoc-executable ebooklify-document-format
             (ebooklify--docfile document ".tex") docfile))))

(defun ebooklify--read-template (template-name)
  (let ((template-path (expand-file-name template-name ebooklify--template-directory)))
    (ebooklify--read-file-to-string template-path)))

(defun ebooklify--expand-section-template (document template)
  (s-replace ":body:" (ebooklify--read-file-to-string (ebooklify--docfile document ".tex"))
             (s-replace ":title:" (document-title document) template)))

(defun ebooklify--expand-main-template (title author sections template)
  (s-replace ":sections:" sections
             (s-replace ":author:" author
                        (s-replace ":title:" title template))))


(defun ebooklify--build-source (title author documents)
  (let ((main-template (ebooklify--read-template "main.txt"))
        (section-template (ebooklify--read-template "section.txt"))
        (sections nil))
    (setq sections (mapconcat
                    (lambda (doc)
                      (ebooklify--expand-section-template doc section-template)) documents ""))
    (write-region
     (ebooklify--expand-main-template title author sections main-template) nil
     (expand-file-name "main.tex" ebooklify-output-directory) nil nil nil t)))

(defun ebooklify--run-tex4ebook (ebook-format)
  (shell-command-to-string (format "cd %s && tex4ebook main.tex -f %s" ebooklify-output-directory ebook-format)))

(defun ebooklify--get-documents-from-backend (ids)
  ;;  (require (intern (concat "ebooklify-"  ebooklify-document-backend)))
  (funcall (intern (format "ebooklify-%s--fetch-document" ebooklify-document-backend)) ids))

(defun ebooklify-create-ebook (title author doc-ids)
  "Create an eBook entitled TITLE by AUTHOR with docs DOC-IDS.

DOC-IDS is a comma-separated list of unique identifiers
identifying each document to include in the eBook."
  (interactive "sTitle: \nsAuthor: \nsList of IDs: ")
  (let* ((ids (mapcar #'s-trim (split-string doc-ids ",")))
         (docs (ebooklify--get-documents-from-backend ids)))
    (mapc #'ebooklify--store-document docs)
    (mapc #'ebooklify--convert-to-tex docs)
    (ebooklify--build-source title author docs)
    (ebooklify--run-tex4ebook ebooklify-output-document-format)))

(provide 'ebooklify-mode)
;;; ebooklify-mode.el ends here
