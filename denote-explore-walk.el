;;; denote-explore-walk.el --- Take random walks in your Denote files -*- lexical-binding: t -*-

;; Copyright (C) 2023  Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Homepage: https://github.com/pprevos/citar-denote
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (denote "1.2"))

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
;; denote-explore-walk provides the following interactive functions:

;; 1. `denote-explore-random-note': Jump to a random note
;; 2. `denote-explore-random-keyword': Jump to random note with selected keyword
;; 2. `denote-explore-random-walk': Jump to a random link in a note
;;
;;; Code:

(require 'f)
(require 'denote)
(require 'denote-explore-stats)

(defun denote-explore--jump (denotes)
  "Jump to a random not in the DENOTES list."
  (find-file (nth (random (length denotes)) denotes)))

(defun denote-explore-random-note ()
  "Jump to a random denote or attachment.
With universal argument the sample includes attachments."
  (interactive)
  (let* ((denotes (if (equal current-prefix-arg nil)
		      (denote-directory-text-only-files)
		    (denote-directory-files)))
	 (denotes-no-current (delete buffer-file-name denotes)))
    (denote-explore--jump denotes-no-current)))

(defun denote-explore--keywords-regexp-dwim ()
  "Select keyword(s) based on context.
Within Denote buffer selects any of the assigned keywords.
If the keyword list is an empty string, then pick any available keyword.
When not in a Denote buffer, pick any available keyword."
  (let* ((file (buffer-file-name))
         (type (denote-filetype-heuristics file))
         (keywords
          (if (denote-file-is-note-p file)
              (if-let* ((kwd (denote-retrieve-keywords-value file type))
			(null kwd))
                  (if (or (listp kwd) (not (string-blank-p kwd)))
                      kwd
                    (denote-keywords))
                (denote-keywords))
            (denote-keywords)))
         (selected (completing-read-multiple
		    "Select keyword: " keywords)))
    (sort (copy-sequence selected) #'string<)))

(defun denote-explore-random-keyword ()
  "Jump to a random note with same selected keyword(s) as the current buffer.

When not in a Denote buffer, or when no keyword is assigned in the
current buffer, select from all available keywords.

Multiple keywords only works when `denote-sort-keywords' is non-nil
or when file keywords are in the same order as selection.  Use the
`denote-explore-order-keywords' function to audit all files.

With universal argument the sample includes attachments."
  (interactive)
  (let* ((keywords-list (denote-explore--keywords-regexp-dwim))
	 (keywords (mapconcat 'identity keywords-list ".*_"))
	 (regexp (if (equal current-prefix-arg nil)
		     (concat "_" keywords)
		   (concat "_" keywords ".*\\.org\\|\\.md|\\.txt"))))
    (if-let (denotes (denote-directory-files-matching-regexp regexp))
	(denote-explore--jump (delete (buffer-file-name) denotes))
      (user-error "Keywords not found"))))

(defun denote-explore--gather-links ()
  "Collect links in the current buffer."
    (let* ((file (buffer-file-name))
            (type (denote-filetype-heuristics file))
            (regexp (denote--link-in-context-regexp type)))
            (denote-link--expand-identifiers regexp)))

(defun denote-explore--gather-backlinks ()
  "Collect backlinks to the current buffer."
  (let* ((file (buffer-file-name))
            (id (denote-retrieve-filename-identifier file)))
            (delete file (denote--retrieve-files-in-xrefs id))))

(defun denote-explore-random-link ()
  "Jump to a random linked note (forward or backward).
With universal argument the sample includes attachments."
  (interactive)
  (let* ((links (denote-explore--gather-links))
         (backlinks (denote-explore--gather-backlinks))
         (all-links (append links backlinks)))
    ;; TODO Add prefix arg
    (if (not (null all-links))
        (denote-explore--jump all-links)
      (user-error "No links in this buffer"))))

(provide 'denote-explore-walk)
;;; denote-explore-walk.el ends here

