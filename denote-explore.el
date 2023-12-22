;;; denote-explore.el --- Collection of functions to explore Denote files -*- lexical-binding: t -*-

;; Copyright (C) 2023 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-extra/
;; Version: 0.9
;; Package-Requires: ((emacs "29.1") (denote "2.0.0") (dashboard "2.19.1") (f "0.20.0"))

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
;; Collection of functions to explore Denote files:
;;
;; 1. Statistics (count notes and keywords)
;; 2. Random walks (follow linked files)
;; 3. Visualisation

;;; Code:

(require 'denote)
(require 'chart)
(require 'dash)
(require 'f)
(require 'dashboard)
(require 'all-the-icons)

;; Random Walks

(defun denote-explore--jump (denotes)
  "Jump to a random note in the DENOTES list."
  (find-file (nth (random (length denotes)) denotes)))

;;;###autoload
(defun denote-explore-random-note ()
  "Jump to a random denote or attachment.

With universal argument the sample includes attachments."
  (interactive)
  (let* ((denotes (if current-prefix-arg
		      (denote-directory-files)
		    (denote-directory-text-only-files)))
	 (denotes-no-current (delete buffer-file-name denotes)))
    (denote-explore--jump denotes-no-current)))

(defun denote-explore--keywords-regexp-dwim ()
  "Select list of Denote keyword(s) based on context.

Within a Denote buffer selects any of the assigned keywords.

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

;;;###autoload
(defun denote-explore-random-keyword ()
  "Jump to a random note with same selected keyword(s) as the current buffer.

When not in a Denote buffer, or when no keyword is assigned in the
current buffer, select from all available keywords.

Multiple keywords only works when `denote-sort-keywords' is non-nil
or when file keywords are in the same order as selection.

With universal argument the sample includes attachments."
  (interactive)
  (let* ((keywords-list (denote-explore--keywords-regexp-dwim))
	 (keywords (mapconcat 'identity keywords-list ".*_"))
	 (kw-regex (concat "_" keywords)))
    (if-let* ((denotes (denote-directory-files-matching-regexp kw-regex))
	      (sample (if current-prefix-arg
			  denotes
			(seq-filter #'denote-file-is-note-p denotes))))
	(progn
	  (denote-explore--jump sample)
	  (message (car sample)))
      (user-error "No matching Denote file found"))))

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

;;;###autoload
(defun denote-explore-random-link ()
  "Jump to a random linked note (forward or backward).

With universal argument the sample includes attachments."
  (interactive)
  (if (denote-file-is-note-p (buffer-file-name))
      (let* ((flinks (denote-explore--gather-links))
	     (blinks (denote-explore--gather-backlinks))
	     (alinks (append flinks blinks))
	     (links (if current-prefix-arg
			(seq-filter #'denote-file-is-note-p alinks)
		      alinks)))
	(if (not (null links))
	    (denote-explore--jump links)
	  (user-error "No links in or to this buffer")))
  (user-error "Buffer is not a Denote file")))

;; Counting functions

;;;###autoload
(defun denote-explore-count-notes ()
  "Count number of Denote text files and attachments."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
	 (denote-files (length (denote-directory-text-only-files)))
	 (attachments (- all-files denote-files)))
    (message "%s Denote files (%s attachments)"
	     denote-files attachments)))

;;;###autoload
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

(defun denote-explore--count-keywords ()
  "Create association list of Denote keyword frequency."
  (denote-explore--table
   (mapcan
    #'denote-extract-keywords-from-path
    (denote-directory-files))))

;;;###autoload
(defun denote-explore-keywords-barchart (n)
  "Create a barchart with the top N most used Denote keywords."
  (interactive "nNumber of keywords: ")
  (denote-explore--barchart (denote-explore--count-keywords) n
  "Keywords" "Denote Keywords"))

;;;###autoload
(defun denote-explore-extensions-barchart ()
  "Visualise the number of Denote files and attachment."
  (interactive)
  (let (extlst)
    (dolist (file (denote-directory-files))
      (push (file-name-extension file) extlst))
    (denote-explore--barchart
     (denote-explore--table extlst)  nil
     "Extensions" "Denote file extensions")))

;; Dashboard widget for Denote statistics

;;;###autoload
(defun denote-explore--dashboard (list-size)
  "Helper function to display Denote summary on the Emacs Dashboard.

LIST-SIZE is a dummy variable required by the dashboard package."
  (dashboard-insert-heading "Denote:"
                            "d"
                            (all-the-icons-octicon
                             "file-text"
                             :height 1.2
                             :v-adjust 0.0
                             :face 'dashboard-heading))
  (let ((prefix (concat "    "
                        (all-the-icons-octicon
                         "primitive-dot"
                         :height 1.0 :v-adjust 0.01)
                        "   ")))
    (insert "\n")
    (insert (concat prefix (denote-explore-count-notes) "\n"))
    (insert (concat prefix (denote-explore-count-keywords))))
  (dashboard-insert-shortcut 'denote "d" "Denote:"))

;;;###autoload
(defun denote-explore-activate-dashboard-widget ()
  "Add the Denote statistics to the Emacs dashboard."
  (interactive)
  (add-to-list ' dashboard-item-generators
               '(denote . denote-explore--dashboard))
  (add-to-list 'dashboard-items '(denote) t)
  (add-to-list 'dashboard-item-shortcuts '(denote . "d")))

;;;###autoload
(defun denote-explore-deactivate-dashboard-widget ()
  "Remove the Denote statistics to the Emacs dashboard."
  (interactive)
  (setq dashboard-items
      (assq-delete-all 'denote dashboard-items)))

;; Visualise
(defvar denote-explore-network-filename
  (concat (file-name-as-directory (getenv "HOME")) "denote-network.html")
  "Full path of the D3.js network output file.")

(defun denote-explore-network-r ()
  "Generate a D3.js visualisation of the Denote files using R.

Requires the R software to be available.  When using the first time
R will install required packages."
  (interactive)
  (unless (executable-find "Rscript")
    (user-error "R unavailable on this system"))
  (let* ((command (format "Rscript denote-explore-network.R %s %s"
			  denote-directory
			  denote-explore-network-filename))
	 (exit-status (shell-command command)))
    (cond ((eq exit-status 0)
           (if (file-exists-p denote-explore-network-filename)
               (browse-url denote-explore-network-filename)
             (error "Network file does not exist")))
          (t (user-error "Network generation unsuccessful")))))

(provide 'denote-explore)
;;; denote-explore.el ends here
