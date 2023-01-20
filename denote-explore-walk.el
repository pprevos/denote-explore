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
;; 2. `denote-explore-random-keyword': Jump to random note with a selected keyword
;; 2. `denote-explore-random-walk': Jump to a random link in a note
;;
;;; Code:

(require 'denote)

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

(defun denote-explore-random-keyword ()
  "Jump to random note with keyword selected from current buffer."
  (interactive)
  ;; Reused `denote-keywords-remove code'
  (if-let* ((file (buffer-file-name))
            ((denote-file-is-note-p (buffer-file-name)))
            (file-type (denote-filetype-heuristics file)))
      (if-let* ((cur-keywords (denote-retrieve-keywords-value file file-type))
		(or (listp cur-keywords) (not (string-blank-p cur-keywords))))
	  ((let* ((selected-keyword
		   (completing-read "Select keyword: " cur-keywords))
		  (denotes (denote-directory-files-matching-regexp
			    (concat "_" selected))))
	     (denote-explore--jump (delete (buffer-file-name) denotes))))
	(user-error "No keywords in Denote file"))
    (user-error "Buffer not visiting a Denote file")))


(defun denote-explore-random-keyword ()
  "Jump to random note with keyword selected from current buffer.
With prefix argument, select keyword from all notes."
  (interactive)
  ;; Check if we are in denote
  (if-let* ((file (buffer-file-name))
            ((denote-file-is-note-p (buffer-file-name)))
            (file-type (denote-filetype-heuristics file)))
      (if-let* (
		()


		)

       )
    (user-error "You are not in a Denote file")))
  
(defun denote-explore-random-link ()
  "Jump to a random link in the current buffer."
  (interactive)
  (if-let* ((file (buffer-file-name))
            ((denote-file-is-note-p file)))
      (if-let* (
		;; List all links in current buffer
		;; List all backlinks in current buffer
		(links nil)
		(backlinks nil)
		(buffer-links (append links backlinks))
		)
	  (denote-explore--jump buffer-links)
	    (user-error "No links or backlinks found"))
    (user-error "Buffer not visiting a Denote file")))

(defun denote-link-find-file ()
  "Use minibuffer completion to visit linked file."
  (interactive)
  (if-let* ((current-file (buffer-file-name))
            (file-type (denote-filetype-heuristics current-file))
            (setq regexp (denote--link-in-context-regexp file-type))
            (files (denote-link--expand-identifiers regexp)))
      (find-file
       (denote-get-path-by-id
        (denote-extract-id-from-string
         (denote-link--find-file-prompt files))))
    (user-error "No links found in the current buffer")))

(provide 'denote-explore-walk)
;;; denote-explore-walk.el ends here
