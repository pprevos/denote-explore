;;; denote-stats.el --- Convenience functions for Denote -*- lexical-binding: t -*-

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
;; The functions in this file provide statistical data about the set of notes.

(require 'dash)
(require 'chart)
(require 'f)

(defun denote-stats-count-notes ()
  "Count number of notes."
  (interactive)
  (message "%s notes" (length (denote-directory-files))))

(defun denote-stats-list-keywords ()
  "Generate list of all available keywords."
  (apply #'append (mapcar
                   #'denote--extract-keywords-from-path
                   (denote-directory-files))))

(defun denote-stats-count-keywords ()
  "Count distinct keywords."
  (interactive)
  (message "%s distinct keywords"
           (length
            (delete-dups
             (denote-stat-list-keywords)))))

(defun denote-stats-barchart (table n var title)
    "Create a barchart from a frequency table with top-`n` entries.
`var` and `title` used for display"
    (chart-bar-quickie
     'vertical
     title
     (mapcar #'car keywords-table) var
     (mapcar #'cdr keywords-table) "Frequency" n))

  (defun denote-stats-keywords-table (keywords)
    "Sort a frequency table with most common on top."
    (sort (-frequencies keywords)
          (lambda (a b) (> (cdr a) (cdr b)))))

  (defun denote-stats-keywords-barchart (n)
    "Create a barchart with the top-n most used Denote keywords."
    (interactive "nNumber of bars: ")
    (let* ((keywords (denote-stats-list-keywords))
           (keywords-table (denote-stats-keywords-table keywords)))
      (denote-stats-barchart keywords-table n
                             "Keywords" "Denote Keyword Frequencies")))
