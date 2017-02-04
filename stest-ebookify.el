;;; stest-ebookify.el --- Tests for ebookify         -*- lexical-binding: t; -*-

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

;;; Code:



(provide 'stest-ebookify)
;;; stest-ebookify.el ends here


(require 'ert)

(ert-deftest ebookify--docfile/create-absolute-path ()
  (let ((ebookify-output-directory "/tmp")
        (doc (make-document :num "00001")))
    (should (equal (ebookify--docfile doc ".tex") "/tmp/00001.tex"))))

(ert-deftest ebookify--read-file-to-string/read-file ()
  (let ((str (ebookify--read-file-to-string "LICENSE")))
    (should (numberp (string-match " +GNU GENERAL PUBLIC LICENSE" str)))))

;;; stest-ebookify.el ends here
