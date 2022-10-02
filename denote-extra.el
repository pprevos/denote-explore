;;; denote-extra.el --- Convenience functions for Denote -*- lexical-binding: t -*-

;; Copyright (C) 2022  Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.2"))

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
;; This package provides some convenience functions for working with Denote.
;; Counting notes and tags
;; Take a random walk through your notes

(require 'dash)
(require 'chart)
(require 'f)

;; Counting functions

(defun denote-extra-count-notes ()
  "Count number of notes."
  (interactive)
  (message "%s notes" (length (denote-directory-files))))

(defun denote-extra-extract-keywords ()
  "Extract keywords from `denote-directory-files'."
  (mapcan #'denote-extract-keywords-from-path
          (denote-directory-files)))

(defun denote-extra-count-keywords ()
  "Count distinct keywords."
  (interactive)
  (message "%s distinct keywords"
           (length (delete-dups (denote-extra-extract-keywords)))))

;; Visualisation functions

(defun denote-extra--barchart (table n var title)
  "Create a barchart from a frequency TABLE with top-N entries.
VAR and TITLE used for display."
  (chart-bar-quickie
   'vertical
   title
   (mapcar #'car table) var
   (mapcar #'cdr table) "Frequency" n))

  (defun denote-extra--table (list)
  "Generate an ordered frequency table from a list."
  (sort (-frequencies list)
        (lambda (a b) (> (cdr a) (cdr b)))))

(defun denote-extra-keywords-barchart (n)
  "Create a barchart with the top-n most used Denote keywords."
  (interactive "nNumber of keywords: ")
  (denote-extra--barchart
   (denote-extra--table (denote-extra-extract-keywords)) n
   "Keywords" "Denote Keywords"))

(defun denote-extra-extensions-barchart ()
  "Visualise the number of Denote files and attachment "
  (interactive)
  (let ((extlst nil))
    (dolist (f (denote-directory-files))
      (push (file-name-extension f) extlst))
    (denote-extra--barchart
     (denote-extra--table extlst)  nil
     "Extensions" "Denote file extensions")))

;; Random walks

(defun denote-extra-random-walk-note ()
  "Jump to a random note."
  (interactive)
  (let* ((denotes (denote--directory-files))
         (denotes-no-current (delete buffer-file-name denotes)))
    (find-file (nth (random (length denotes-no-current)) denotes-no-current))))

(defun denote-extra-check-metadata ()
  "Synchronise the metadata with the filename for all denote files."
  (let ((org-files
         (-filter (lambda (f) (string-match "\\.txt$\\|\\.md$\\|\\.org$" f))
                  (denote-directory-files))))
    (save-some-buffers)
    (dolist (f org-files)
      (message "Checking %s" f)
      (find-file f)
      (denote-rename-file-using-front-matter f)
      (kill-buffer)))
  (message "Integrity check completed"))

(provide 'denote-extra)
