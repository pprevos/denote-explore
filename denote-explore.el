;;; denote-explore.el --- Explore Denote files -*- lexical-binding: t -*-

;; Copyright (C) 2023 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-extra/
;; Version: 1.3
;; Package-Requires: ((emacs "29.1") (dash "2.19.1") (denote "2.2.4"))

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
;; explore Denote functionality:
;;
;; 1. Statistics (count notes and keywords)
;; 2. Random walks through your notes
;; 3. Maintaining notes (janitor)
;; 3. Visualisation

;;; Code:

(require 'denote)
(require 'dash)
(require 'chart)
(require 'cl-lib)
(require 'json)

;; Variables
(defgroup denote-explore ()
  "Variables for network visualisation."
  :group 'files)

(defcustom denote-explore-network-directory
  (expand-file-name "denote-network" user-emacs-directory)
  "Directory to store Denote network files.
Created upon generating the network when it does not yet exist"
  :group 'denote-explore
  :type 'string)

(defcustom denote-explore-network-json-filename
  (expand-file-name "denote-network.json" denote-explore-network-directory)
  "Filename of the JSON file for Denote vertices (notes) and edges (links)."
  :group 'denote-explore
  :type 'string)

(defcustom denote-explore-network-dot-filename
  (expand-file-name "denote-network.gv" denote-explore-network-directory)
  "Filename of the GraphViz file for Denote vertices (notes) and edges (links)."
  :group 'denote-explore
  :type 'string)

(defcustom denote-explore-network-svg-filename
  (expand-file-name "denote-network.svg" denote-explore-network-directory)
  "Filename of the Denote network SVG file."
  :group 'denote-explore
  :type 'string)

(defcustom denote-explore-network-filename
  (expand-file-name "denote-network.html" denote-explore-network-directory)
  "Filename and path of the HTML network visualisation files."
  :group 'denote-explore
  :type 'string)

(defvar denote-explore-load-directory
  (file-name-directory load-file-name)
  "Path of the denote-explore package.")

(make-obsolete-variable 'denote-explore-json-vertices-filename
			'denote-explore-network-json-filename "1.3")


(make-obsolete-variable 'denote-explore-json-edges-filename
			'denote-explore-network-json-filename "1.3")

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
  (when-let* ((keywords (denote-retrieve-filename-keywords file))
	      (filetype (denote-filetype-heuristics file))
	      (keywords (if (denote-file-is-note-p file)
			    (denote-retrieve-keywords-value file filetype)
			  (string-split  "_"))))
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
	    (keyword-regex (concat "_" (mapconcat #'identity keyword-list ".*_"))))
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
(defun denote-explore-identify-duplicate-notes (&optional filenames)
  "Identify duplicate Denote IDs or FILENAMES.
If FILENAMES is nil, check Denote IDs, otherwise check the full file names.
Using the FILENAMES option (using the universal argument) excludes
exported Denote files."
  (interactive "P")
  (let* ((denote-files (denote-directory-files))
         (candidates (if filenames
                         (mapcar (lambda (path)
                                   (file-name-nondirectory path))
                                 denote-files)
                       (mapcar #'denote-retrieve-filename-identifier
                               denote-files)))
         (tally (denote-explore--table candidates))
         (duplicates (mapcar #'car (cl-remove-if-not
                                    (lambda (note)
				      (> (cdr note) 1)) tally))))
    (if duplicates
        (message "Duplicate identifier(s): %s"
		 (mapconcat 'identity duplicates ", "))
      (message "No duplicate identifiers found"))))

(make-obsolete 'denote-explore-identify-duplicate-identifiers
	       'denote-explore-identify-duplicate-notes "Version 1.2")

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
  (when (file-exists-p file)
    (if (denote-file-is-note-p file)
	(denote-retrieve-title-value file (denote-filetype-heuristics file))
      (denote-desluggify-title (denote-retrieve-filename-title file)))))

;;;###autoload
(defun denote-explore-sort-keywords ()
  "Order the keywords of all Denote notes and attachments alphabetically."
  (interactive)
  (save-some-buffers)
  (let ((denote-rename-no-confirm nil)
	(notes (denote-directory-files)))
    (dolist (file notes)
      (let ((keywords (denote-explore--retrieve-keywords file)))
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

;;; Bar charts

(defun denote-explore--barchart (table var title &optional n)
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
  (denote-explore--barchart
   (denote-explore--table
    (denote--inferred-keywords)) "Keywords" "Denote Keywords" n))

;;;###autoload
(defun denote-explore-extensions-barchart ()
  "Visualise the to N Denote file and attachment types."
  (interactive)
  (let (ext-list)
    (dolist (file (denote-directory-files))
      (push (file-name-extension file) ext-list))
    ;; Replace nil extensions with string
    (setq ext-list (mapcar (lambda (extension)
              (if (null extension)
                  "nil" extension))
            ext-list))
    (denote-explore--barchart
     (denote-explore--table ext-list) "Extensions" "Denote file extensions")))

;; Network definitions

(defun denote-explore--network-zip-alists (from to)
  "Zip two lists into a list of alists, pairing FROM and TO."
  (let ((result nil))
    (while (and from to)
      (let ((pair (list (cons 'from (car from)) (cons 'to (car to)))))
        (push pair result)
        (setq from (cdr from))
        (setq to (cdr to))))
    (nreverse result)))

(make-obsolete 'denote-explore--extract-vertices
	       'denote-explore--network-extract-node "1.3")

(defun denote-explore--network-extract-edges (files)
  "Extract Denote network links as network edges from FILES."
  (when-let* ((links-xref (xref-matches-in-files
			   "\\[denote:[0-9]\\{8\\}" files))
	      (from (mapcar #'denote-retrieve-filename-identifier
			    (mapcar
			     #'xref-location-group
			     (mapcar #'xref-match-item-location
				     links-xref))))
	      (links (mapcar #'substring-no-properties
			     (mapcar #'xref-match-item-summary
				     links-xref)))
	      (to (mapcar
		   (lambda (str)
		     (when (string-match denote-id-regexp str)
		       (match-string 0 str))) links)))
    (denote-explore--network-zip-alists from to)))

(defun denote-explore--network-extract-node (file)
  "Extract metadata for note or attachment with FILE."
  (when (file-exists-p file)
    (let ((id (denote-retrieve-filename-identifier file))
	  (name (denote-explore--retrieve-title file))
	  (keywords (denote-retrieve-filename-keywords file))
	  (type (if (denote-file-is-note-p file)
		    "note"
		  "attachment")))
      (setq keywords (if keywords (string-split keywords "_") ""))
      ;; MINOR ISSUE: `id' is saved as a list.
      `((id ,id) (name . ,name) (keywords . ,keywords) (type . ,type)))))

(defun denote-explore--network-filter-links (nodes links)
  "Filter out LINKS where neither 'from' nor 'to' are in NODES."
  (let ((filtered-links '()))
    (dolist (link links filtered-links)
      (let ((from (cdr (assoc 'from link)))
            (to (cdr (assoc 'to link))))
        (when (and (member from nodes) (member to nodes))
          (push link filtered-links))))
    (nreverse filtered-links)))

(defun denote-explore--network-generate (files)
  "Generate Denote network structure from FILES."
  (let* ((file-nodes (mapcar #'denote-extract-id-from-string files))
	 (edges-alist (denote-explore--network-filter-links
		       file-nodes
		       (denote-explore--network-extract-edges files)))
	 (nodes-alist (mapcar #'denote-explore--network-extract-node files)))
    `((nodes . ,nodes-alist) (links . ,edges-alist))))

;;; JavaScript visualisation

(defun denote-explore-network-save-json (regex)
  "Save the network of the Denote files matching REGEX as a JSON file.

Saved to `denote-explore-network-json-filename'."
  (interactive "MEnter regex (empty string for all notes): ")
  (when (not (file-exists-p denote-explore-network-directory))
    (make-directory denote-explore-network-directory))
  (let ((files (denote-directory-files regex)))
    (with-temp-file denote-explore-network-json-filename
      (insert (json-encode
	       (denote-explore--network-generate files)))
      (json-pretty-print-buffer))))

(defun denote-explore--script-call ()
  "Construct command for calling R script."
  (format "Rscript %sdenote-explore-network.R %s %s"
          (shell-quote-argument denote-explore-load-directory)
          (shell-quote-argument denote-explore-network-json-filename)
	  (shell-quote-argument denote-explore-network-filename)))

;;;###autoload
(defun denote-explore-network-r (regex)
  "Generate a D3.js visualisation of Denote files matching REGEX.

Requires the R software to be available.  When this function runs for the
first time, R will install required packages."
  (interactive "MEnter regex (empty string for all notes): ")
  (unless (executable-find "Rscript")
    (user-error "Rscript is unavailable on this system - install R"))
  (denote-explore-network-save-json regex)
  (let ((exit-status (shell-command (denote-explore--script-call))))
    (cond ((eq exit-status 0)
           (if (file-exists-p denote-explore-network-filename)
               (progn (message "Network generation successful.")
		      (browse-url denote-explore-network-filename))
             (user-error "Network file does not exist")))
          (t (user-error "Network generation unsuccessful")))))

;;; GraphViz visualisation
(defun denote-explore--network-dot-encode (network)
  "Convert a Denote NETWORK association list to a GraphViz dot file."
  (let* (;; Graph settings
	(front-matter (concat "strict digraph G {\n"
			    "layout=neato;\n"
			    "forcelabels=true;\n"
			    "overlap=scale;\n"
			    "sep=\"+50\";\n"
			    "nodesep=1;\n"
			    "node[label=\"\" style=filled fillcolor=lightblue "
			    "color=lightblue shape=circle width=1 fontsize=48];\n"
			    "edge[arrowsize=3 color=dodgerblue4];\n"))
	;; Extract nodes and edges
        (nodes (cdr (assoc 'nodes network)))
	(edges (cdr (assoc 'links network))))
    ;; Append node definitions
    (setq dot-nodes nil)
    (dolist (node nodes)
      ;; TODO: Why is the id a list?
      (let* ((id (car (cdr (assoc 'id node))))
	     (name (cdr (assoc 'name node)))
	     (tags-list (cdr (assoc 'keywords node)))
	     (tags (mapconcat 'identity tags-list ", "))
	     (type (let ((note-type (cdr (assoc 'type node))))
		     (if (equal note-type "note") "lightblue" "dodgerblue4"))))
	(setq dot-nodes (concat dot-nodes
				(format "    \"%s\" [xlabel=\"%s\" tooltip=\"ID: %s\\nTitle: %s\\nKeywords: %s\" fillcolor=%s];\n"
					id name id name tags type)))))
    ;; Append edge definitions
    (setq dot-edges nil)
    (dolist (edge edges)
      (let ((source (cdr (assoc 'from edge)))
	    (target (cdr (assoc 'to edge))))
	(setq dot-edges (concat dot-edges
				(format "    \"%s\" -> \"%s\";\n" source target)))))
    (concat front-matter dot-nodes dot-edges "}\n")))

;;;###autoload
(defun denote-explore-network-save-dot (regex)
  "Save the Denote files and links matching REGEX as a GraphViz dot file.
Stored in `denote-explore-network-dot-filename'."
  (interactive "MEnter regex (empty string for all notes): ")
  (when (not (file-exists-p denote-explore-network-directory))
    (make-directory denote-explore-network-directory))
  (let ((files (denote-directory-files regex)))
    (with-temp-file denote-explore-network-dot-filename
      (insert (denote-explore--network-dot-encode
	       (denote-explore--network-generate files))))))

(defun denote-explore--network-dot-script-call ()
  "Construct command for calling GraphViz dot script.
Pre-processing with gvpr to calculate node sizes."
  (format "gvpr -c -f  %stest/graphviz.gvpr %s | neato -Tsvg > %s"
	  (shell-quote-argument denote-explore-load-directory)
          (shell-quote-argument denote-explore-network-dot-filename)
	  (shell-quote-argument denote-explore-network-svg-filename)))

;;;###autoload
(defun denote-explore-network-graphviz (regex)
  "Generate a GraphViz (dot) visualisation of Denote files matching REGEX.
Requires the GraphViz software (https://graphviz.org/)."
  ;; TODO: Add clustering
  (interactive "MEnter regex (empty string for all notes): ")
  (unless (executable-find "dot")
    (user-error "GraphViz is unavailable"))
  (denote-explore-network-save-dot regex)
  (let ((exit-status (shell-command (denote-explore--network-dot-script-call)))
	(n-nodes (length (denote-directory-files regex))))
    (cond ((eq exit-status 0)
           (if (file-exists-p denote-explore-network-svg-filename)
               (progn (message "Generated network with %s nodes" n-nodes)
		      ;; TODO: Enforce browser use (wrap in html?)
		      (browse-url-default-browser
		       (concat "file://"
			       denote-explore-network-svg-filename)))
             (user-error "Network file does not exist")))
          (t (user-error "Network generation unsuccessful")))))

(provide 'denote-explore)
;;; denote-explore.el ends here
