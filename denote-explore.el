;;; denote-explore.el --- Explore Denote files -*- lexical-binding: t -*-

;; Copyright (C) 2023 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-extra/
;; Version: 1.1
;; Package-Requires: ((emacs "29.1") (denote "2.2.0") (f "0.20.0"))

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
;; Explore Denote functionality:
;;
;; 1. Statistics (count notes and keywords)
;; 2. Random walks through your notes
;; 3. Maintaining notes (janitor)
;; 3. Visualisation

;;; Code:

(require 'denote)
(require 'chart)
(require 'dash)
(require 'f)
(require 'cl-lib)

;; Variables
(defgroup denote-explore ()
  "Variables for network visualisation."
  :group 'files)

(defcustom denote-explore-json-vertices-filename
  (concat (file-name-as-directory user-emacs-directory) "denote-vertices.json")
  "Filename of the JSON file that stores the Denote network vertices (files)."
  :group 'denote-explore
  :type 'string)

(defcustom denote-explore-json-edges-filename
  (concat (file-name-as-directory user-emacs-directory) "denote-edges.json")
  "Filename of the JSON file that stores the Denote network edges (links)."
  :group 'denote-explore
  :type 'string)

(defcustom denote-explore-network-filename
  (expand-file-name "denote-network.html" (getenv "HOME"))
  "Filename and path of the D3.js output files."
  :group 'denote-explore
  :type 'string)

(defvar denote-explore-load-directory
  (file-name-directory load-file-name)
  "Path of the denote-explore package.")

;; STATISTICS

;;;###autoload
(defun denote-explore-count-notes ()
  "Count number of Denote text files and attachments."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
	 (denote-files (length (denote-directory-files nil nil t)))
	 (attachments (- all-files denote-files)))
    (message "%s Denote files (%s attachments)" denote-files attachments)))

;;;###autoload
(defun denote-explore-count-keywords ()
  "Count distinct Denote keywords."
  (interactive)
  (message "%s distinct keywords" (length (denote-keywords))))

;; RANDOM WALKS
(defun denote-explore--jump (denote-sample)
  "Jump to a random note in the DENOTE-SAMPLE file list."
  (find-file (nth (random (length denote-sample)) denote-sample)))

;;;###autoload
(defun denote-explore-random-note ()
  "Jump to a random Denote file.

With universal argument, the sample includes attachments."
  (interactive)
  (let* ((denotes (if current-prefix-arg
                      (denote-directory-files)
                    (denote-directory-files nil nil t)))
         (denotes-no-current (remove buffer-file-name denotes)))
    (if denotes-no-current
        (denote-explore--jump denotes-no-current)
      (user-error "No Denote files found"))))

;;;###autoload
(defun denote-explore-random-link ()
  "Jump to a random linked note (forward or backward).

With universal argument the sample includes links to attachments."
  (interactive)
  (if (denote-file-is-note-p (buffer-file-name))
      (let* ((forward-links (denote-link-return-links))
	     (back-links (denote-link-return-backlinks))
	     (all-links (append forward-links back-links))
	     (links (if current-prefix-arg
			all-links
		      (seq-filter #'denote-file-is-note-p all-links))))
	(if links (denote-explore--jump links)
	  (user-error "No links in or to this buffer")))
    (user-error "Buffer is not a Denote file")))

(defun denote-explore--retrieve-keywords (file)
  "Retrieve keywords from Denote FILE or attachment."
  (let* ((filetype (denote-filetype-heuristics file))
	 (keywords (if (denote-file-is-note-p file)
		       (denote-retrieve-keywords-value file filetype)
		     (string-split (denote-retrieve-filename-keywords file) "_"))))
    (if (equal (car keywords) "") nil keywords)))

(defun denote-explore--select-keywords ()
  "Select Denote keyword(s).

- Use \"*\" to select all listed keywords.
- If no keyword is defined in the current buffer, then choose from all
  available keywords."
  (if-let* ((file (buffer-file-name))
	    (buffer-keywords (denote-explore--retrieve-keywords file))
	    (keywords (if (> (length buffer-keywords) 1)
			  (delete-dups (sort (completing-read-multiple
					      "Select keyword: " buffer-keywords)
					     #'string<))
			buffer-keywords)))
      (if (string= (car keywords) "*") buffer-keywords keywords)))

;;;###autoload
(defun denote-explore-random-keyword ()
  "Jump to a random note with selected keyword(s).

- Manually select one or more keywords from the active Denote buffer.
- Use \"*\" to select all listed keywords.

Using multiple keywords requires `denote-sort-keywords' to be non-nil
or keywords in the same order as the selection. Alternatively, use
`denote-explore-sort-keywords' to correct all existing files.

With universal argument the sample includes attachments."
  (interactive)
  (if-let* ((keyword-list (denote-explore--select-keywords))
	    (keyword-regex (concat "_" (mapconcat 'identity keyword-list ".*_"))))
      (if-let* ((denotes (denote-directory-files keyword-regex))
		(sample (if current-prefix-arg
			    denotes
			  (seq-filter #'denote-file-is-note-p denotes)))
		(clean-sample (cl-remove-if
			       (lambda (string)
				 (string= (buffer-file-name) string)) sample)))
	  (denote-explore--jump clean-sample)
	(user-error "No matching Denote file found"))
    (user-error "No keywords found")))

;; JANITOR

;;;###autoload
(defun denote-explore-identify-duplicate-identifiers ()
  "Provide a list of duplicate identifiers."
  (interactive)
  (if-let* ((notes (denote-directory-files))
	    (ids (mapcar #'denote-retrieve-filename-identifier
			 (denote-directory-files)))
	    (dups (delete-dups
		   (cl-remove-if-not
		    (lambda (id)
		      (member id (cdr (member id ids)))) ids))))
      (message (format "Duplicate identifier(s): %s"
		       (mapconcat (lambda (id) id) dups ", ")))
    (message "No duplicate identifiers found")))

(defun denote-explore--table (list)
  "Generate an ordered frequency table from a LIST."
  (sort (-frequencies list)
        (lambda (a b) (> (cdr a) (cdr b)))))

;;;###autoload
(defun denote-explore-single-keywords ()
  "Select a note or attachment with a keyword that is only used once."
  (interactive)
  (let* ((keywords-count (denote-explore--table (denote--inferred-keywords)))
	 (single-keywords (mapcar #'car (cl-remove-if
					 (lambda (pair) (> (cdr pair) 1))
					 keywords-count)))
	 (selected-keyword (completing-read "Select single keyword: "
					    single-keywords)))
    (find-file (car (denote-directory-files (concat "_" selected-keyword))))))

;;;###autoload
(defun denote-explore-zero-keywords ()
  "Select a note or attachment without any keywords."
  (interactive)
  (let* ((with-keyword-regex "--\\([[:alnum:][:nonascii:]-]*_\\)")
	 (keywords (denote-directory-files with-keyword-regex))
	 (zero-keywords (seq-remove (lambda (note) (member note keywords))
				    (denote-directory-files))))
    (find-file (completing-read "Select file with zero keywords: "
				zero-keywords))))

(defun denote-explore--retrieve-title (file)
  "Retrieve the title from a Denote FILE or an attachment."
  (if (denote-file-is-note-p file)
      (denote-retrieve-title-value file (denote-filetype-heuristics file))
    (denote-retrieve-filename-title file)))

;;;###autoload
(defun denote-explore-sort-keywords ()
  "Order the keywords of all Denote notes and attachments alphabetically."
  (interactive)
  (save-some-buffers)
  (let ((denote-rename-no-confirm nil)
	(notes (denote-directory-files)))
    (dolist (file notes)
      (let ((file-type (denote-filetype-heuristics file))
	    (keywords (denote-explore--retrieve-keywords file)))
	(denote-rename-file file
			    (denote-explore--retrieve-title file)
			    (denote-keywords-sort
			     (if (equal (car keywords) "") nil keywords))
			    (denote-retrieve-filename-signature file)))))
  (message "All keywords ordered alphabetically"))

;;;###autoload
(defun denote-explore-rename-keyword ()
  "Rename or remove a keyword across the whole Denote collection.

The first selected existing keyword is renamed or removed.
Select an empty string as new keyword to remove the selection."
  (interactive)
  (save-some-buffers)
  (let* ((denote-rename-no-confirm nil)
	 (old-keyword (car (denote-keywords-prompt)))
	 (new-keyword (read-from-minibuffer "New keyword: "))
	 (notes (denote-directory-files (concat "_" old-keyword))))
    (dolist (file notes)
      (let* ((current-keywords (denote-explore--retrieve-keywords file))
	     (keywords (if (equal new-keyword "")
			   (remove old-keyword current-keywords)
			 (mapcar (lambda (kwd)
				   (if (equal kwd old-keyword)
				       new-keyword kwd))
				 current-keywords))))
	(denote-rename-file file
			    (denote-explore--retrieve-title file)
			    (if (equal keywords nil) "" keywords)
			    (denote-retrieve-filename-signature file))))))

;;;###autoload
(defun denote-explore-sync-metadata ()
  "Synchronise the filenames with the metadata for all Denote files.

Set `denote-rename-buffer-mode' to ensure synchronised notes."
  (interactive)
  (save-some-buffers)
  (let ((notes (denote-directory-files nil nil t)))
    (dolist (file notes)
      (message file)
      (denote-rename-file-using-front-matter file)))
  (message "Integrity check completed"))

;; VISUALISATION

(defun denote-explore--barchart (table n var title)
  "Create a barchart from a frequency TABLE with top N entries.
VAR and TITLE used for display."
  (chart-bar-quickie
   'vertical
   title
   (mapcar #'car table) var
   (mapcar #'cdr table) "Frequency" n))

;;;###autoload
(defun denote-explore-keywords-barchart (n)
  "Create a barchart with the top N most used Denote keywords."
  (interactive "nNumber of keywords: ")
  (denote-explore--barchart (denote-explore--table
			     (denote--inferred-keywords)) n
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

(defun denote-explore--extract-vertices (regex)
  "Extract Denote network vertices metadata from files matching REGEX."
  (if-let* ((files (denote-directory-files regex))
            (vertices (lambda (file)
			(cons (denote-retrieve-filename-identifier file)
			      (denote-explore--retrieve-title file)))))
      ;; TODO: Add all metadata
      ;; (denote-retrieve-filename-signature file)
      ;; (denote-explore--retrieve-keywords file)
      ;; (denote-get-file-extension file)
      (mapcar vertices files)
    (user-error (format "No Denote files found matching '%s'" regex))))

(defun denote-explore--extract-edges (regex)
  "Extract Denote network edges (Denote links) from files matching REGEX."
  (if-let* ((files (denote-directory-files regex))
	    (links-xref (xref-matches-in-files
			 "\\[\\[denote:[0-9]\\{8\\}" files))
	    (from (mapcar #'denote-retrieve-filename-identifier
			  (mapcar
			   #'xref-location-group
			   (mapcar #'xref-match-item-location
				   links-xref))))
	    (links (mapcar #'substring-no-properties
			   (mapcar #'xref-match-item-summary
				   links-xref)))
	    (to (mapcar (lambda (str)
			  (when (string-match denote-id-regexp str)
			    (match-string 0 str)))
			links)))
      (cl-mapcar 'cons from to)
    (user-error "No matching Denote links found")))

(defun denote-explore-network-save-json (regex)
  "Save the vertices and edges of the Denote network matching REGEX as JSON.

Saved to `denote-explore-json-vertices-filename' and
`denote-explore-json-edges-filename'."
  (interactive "MEnter regex (empty string for all notes): ")
  (let ((denote-edges (denote-explore--extract-edges regex))
	(denote-vertices (denote-explore--extract-vertices regex)))
    (with-temp-file denote-explore-json-edges-filename
      (insert (json-encode denote-edges)))
    (with-temp-file denote-explore-json-vertices-filename
      (insert (json-encode denote-vertices)))))

(defun denote-explore--script-call ()
  "Construct command for calling R script."
  (format "Rscript %sdenote-explore-network.R %s %s %s"
          denote-explore-load-directory
          denote-explore-json-edges-filename
          denote-explore-json-vertices-filename
	  denote-explore-network-filename))

;;;###autoload
(defun denote-explore-network-r (regex)
  "Generate a D3.js visualisation of Denote files matching REGEX.

Requires the R software to be available.  When this function runs for the
first time, R will install required packages."
  (interactive "MEnter regex (empty string for all notes): ")
  (unless (executable-find "Rscript")
    (user-error "Rscript is unavailable on this system - install R"))
  (denote-explore-network-save-json regex)
  ;; Correctly binding variables in the let* form
  (let ((exit-status (shell-command (denote-explore--script-call))))
    (cond ((eq exit-status 0)
           (if (file-exists-p denote-explore-network-filename)
               (progn (message "Network generation successful.")
		      (browse-url denote-explore-network-filename))
             (user-error "Network file does not exist")))
          (t (user-error "Network generation unsuccessful")))))

(provide 'denote-explore)
;;; denote-explore.el ends here
