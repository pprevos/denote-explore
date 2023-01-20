;;; denote-explore-stats.el --- Summary statistics of your Denote collection -*- lexical-binding: t -*-

;; Copyright (C) 2023 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-extra/
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (dash "2.19.1") (f "0.20.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Counting Denote files and keywords and visualise keywords and extensions
;; with plain text bar charts.  Available functions:
;;
;; - `denote-explore-count-notes': Display number of notes in the mini buffer.
;; - `denote-explore-count-keywords': Display number of keywords.
;; - `denote-explore-keywords-barchart': Plain text bar chart of keyword.
;; - `denote-explore-extensions-barchart': Plain text bar chart of file types.

;;; Code:

(require 'chart)
(require 'dash)
(require 'f)

;; Counting functions

(defun denote-explore-count-notes ()
  "Count number of Denote text files and attachments."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
	 (denote-files (length (denote-directory-text-only-files)))
	 (attachments (- all-files denote-files)))
    (message "%s Denote files (%s attachments)"
	     denote-files attachments)))

(defun denote-explore-count-keywords ()
  "Count distinct Denote keywords."
  (interactive)
  (message "%s distinct keywords"
           (length (denote-keywords))))

;; Visualisation functions

(defun denote-explore--barchart (table n var title)
  "Create a barchart from a frequency TABLE with top N entries.
VAR and TITLE used for display."
  (chart-bar-quickie
   'vertical
   title
   (mapcar #'car table) var
   (mapcar #'cdr table) "Frequency" n))

(defun denote-explore--table (list)
  "Generate an ordered frequency table from a LIST."
  (sort (-frequencies list)
        (lambda (a b) (> (cdr a) (cdr b)))))

(defun denote-explore--extract-keywords ()
  "Extract keywords from `denote-directory-files'."
  (mapcan #'denote-extract-keywords-from-path (denote-directory-files)))

(defun denote-explore-keywords-barchart (n)
  "Create a barchart with the top N most used Denote keywords."
  (interactive "nNumber of keywords: ")
  (denote-explore--barchart
   (denote-explore--table (denote-explore--extract-keywords)) n
   "Keywords" "Denote Keywords"))

(defun denote-explore-extensions-barchart ()
  "Visualise the number of Denote files and attachment."
  (interactive)
  (let (extlst)
    (dolist (file (denote-directory-files))
      (push (file-name-extension file) extlst))
    (denote-explore--barchart
     (denote-explore--table extlst)  nil
     "Extensions" "Denote file extensions")))

(provide 'denote-explore-stats)
;;; denote-explore-stats.el ends here
