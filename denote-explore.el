;;; denote-explore.el --- Explore and visualise Denote files -*- lexical-binding: t -*-
;;
;; Copyright (C) 2023-2025 Peter Prevos
;;
;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-explore/
;; Package-Version: 20250618.1002
;; Package-Revision: 9d9a6399551d
;; Package-Requires: ((emacs "29.1") (denote "4.0") (dash "2.19.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;; Denote-Explore provides functionality to explore, maintain and visualise
;; your collection of Denote files. The major version number indicates
;; compatability with the relevant Denote major version.
;; 
;; Functionality:
;;
;; 1. Statistics: count and visualise notes and keywords
;; 2. Random walks: aces notes with serendipitous discovery
;; 3. Janitor: Maintenance on you Denote collection
;; 4. Network diagrams: visualise the structure of your notes
;;
;; The Denote-Explore manual is available in info-mode
;; (info "denote-explore") `C-h R denote-explore`
;;

;;; Code:
(require 'denote)
(require 'dash)
(require 'chart)
(require 'cl-lib)
(require 'json)
(require 'browse-url)

;;; CUSTOMISATION

(defgroup denote-explore ()
  "Explore and visualise Denote file collections."
  :group 'files
  :link '(url-link :tag "Homepage" "https://github.com/pprevos/denote-explore"))

(defcustom denote-explore-network-directory
  (expand-file-name "graphs/" denote-directory)
  "Directory to store Denote network files.
Created upon generating the network when it does not yet exist"
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type 'string)

(define-obsolete-variable-alias
  'denote-explore-json-vertices-filename
  'denote-explore-network-filename "1.3")

(define-obsolete-variable-alias
  'denote-explore-json-edges-filename
  'denote-explore-network-filename "1.3")

(defcustom denote-explore-network-filename
  "denote-network"
  "Base filename sans extension for Denote explore network files.
Stored in `denote-explore-network-directory'.
File type defined by `denote-explore-network-format'."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type 'string)

(defcustom denote-explore-network-format
  'd3.js
  "Output format for Denote network files."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type '(choice
	  (const :tag "D3 JavaScript (JSON)" d3.js)
	  (const :tag "GraphViz (Dot)" graphviz)
	  (const :tag "Graph Exchange XML Format (GEXF)" gexf)))

(defcustom denote-explore-network-keywords-ignore '()
  "List of keywords to be ignored in the keywords graph."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type '(repeat (string :tag "Keyword")))

(defcustom denote-explore-network-regex-ignore '()
  "Regular expression ignored in neighbourhood, community and sequence graphs."
  :group 'denote-explore
  :package-version '(denote-explore . "1.4")
  :type '(choice (const :tag "No Ignore Regexp" nil)
                 (regexp :tag "Ignore using Regexp")))

(defcustom denote-explore-random-regex-ignore '()
  "Regular expression to exclude form random walks."
  :group 'denote-explore
  :package-version '(denote-explore . "3.3.1")
  :type '(choice (const :tag "No Ignored Regexp" nil)
                 (regexp :tag "Ignore using Regexp")))

(defcustom denote-explore-network-d3-template
  nil
  "Fully qualified path of the D3.JS HTML template file."
  :group 'denote-explore
  :package-version '(denote-explore . "3.1")
  :type '(file :must-match t)
  :initialize 'custom-initialize-default)

;; Set default value at load time if not customised
(when (not denote-explore-network-d3-template)
  (setq denote-explore-network-d3-template
        (expand-file-name "denote-explore-network.html"
                          (file-name-directory (or load-file-name buffer-file-name)))))

(defcustom denote-explore-network-d3-js
  "https://d3js.org/d3.v7.min.js"
  "Location of the D3.js source code."
  :group 'denote-explore
  :type 'string)

(defcustom denote-explore-network-d3-colours
  "schemeObservable10"
  "Colour scheme for D3.js network visualisations.
Colours are assigned to fiule types in order of appearance in the JSON file.
Refer to URL `https://d3js.org/d3-scale-chromatic/categorical' for details."
  :group 'denote-explore
  :type '(choice
	  (const :tag "schemeCategory10" "schemeCategory10")
	  (const :tag "schemeAccent" "schemeAccent")
	  (const :tag "schemeDark2" "schemeDark2")
	  (const :tag "schemeObservable10" "schemeObservable10")
	  (const :tag "schemePaired" "schemePaired")
	  (const :tag "schemePastel1" "schemePastel1")
	  (const :tag "schemePastel2" "schemePastel2")
	  (const :tag "schemePastel3" "schemePastel3")
	  (const :tag "schemeSet1" "schemeSet1")
	  (const :tag "schemeSet2" "schemeSet2")
	  (const :tag "schemeTableau10" "schemeTableau10")))

(defcustom denote-explore-network-graphviz-header
  '("layout=neato"
    "size=20"
    "ratio=compress"
    "overlap=scale"
    "sep=1"
    "node[label=\"\" style=filled color=lightskyblue fillcolor=lightskyblue3
shape=circle fontsize=40 fontcolor=gray10 fontname = \"Helvetica, Arial, sans-serif\"]"
    "edge[arrowsize=3 color=gray30]")
  "List of strings for the header of a GraphViz DOT file.

Defines graph and layout properties and default edge and node attributes.
See graphviz.org for detailed documentation.

Properties for specific edges and nodes, as defined by the
`denote-explore-network-encode-graphviz' function, override these settings."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type '(repeat string))

(defcustom denote-explore-network-graphviz-filetype "svg"
  "Output file type for Denote GraphViz network files.

Use SVG or for interactivity (tootltips and hyperlinks).
See graphviz.org for detailed documentation."
  :group 'denote-explore
  :package-version '(denote-explore . "1.4")
  :type '(choice
	  (const :tag "Scalable Vector Graphics (SVG)" "svg")
	  (const :tag "Portable Document Format (PDF)" "pdf")
	  (const :tag "Portable Network Graphics (PNG)" "png")
	  (string :tag "Other option")))

(defcustom denote-explore-isolated-ignore-keywords (list)
  "List of keywords to ignore when identifying isolated notes."
  :group 'denote-explore
  :type '(choice
          (repeat :tag "List of Tags" (string :tag "Tag:"))
          (const :tag "No Tags Ignored" nil)))

;;; INTERNAL VARIABLES

(defvar denote-explore-network-graph-formats
  '((graphviz
     :file-extension ".gv"
     :encode denote-explore-network-encode-graphviz
     :display denote-explore-network-display-graphviz)
    (d3.js
     :file-extension ".json"
     :encode denote-explore-network-encode-json
     :display denote-explore-network-display-json)
    (gexf
     :file-extension ".gexf"
     :encode denote-explore-network-encode-gexf
     :display nil))
  "A-list of variables related to the network file formats.

Each element is of the form (SYMBOL . PROPERTY-LIST).
SYMBOL is one of those specified in `denote-explore-network-format'.

PROPERTY-LIST is a plist that consists of three elements:

- `:file-extension' File extension to save network.
- `:encode' function to encode network to graph type.
- `:display' function to display the graph in external software.")

(defvar denote-explore-graph-types
  `(("Community"
     :description "Notes matching a regular expression"
     :generate denote-explore-network-community
     :regenerate denote-explore-network-community-graph)
    ("Neighbourhood"
     :description "Search n-deep in a selected note"
     :generate denote-explore-network-neighbourhood
     :regenerate denote-explore-network-neighbourhood-graph)
    ("Keywords"
     :description "Relationships between keywords"
     :generate denote-explore-network-keywords
     :regenerate denote-explore-network-keywords-graph)
    ("Sequence"
     :description "Hierarchical relationship between signatures"
     :generate denote-explore-network-sequence
     :regenerate denote-explore-network-sequence-graph))
  "List of network types and their (re)generation functions.

PROPERTY-LIST is a plist that consists of three elements:

- `:description' Short explanation of graph type.
- `:generate': Function to select options and geenrate graph.
- `:regenerate': Function to regenerate graph from previous options.")

(define-obsolete-variable-alias
  'denote-explore-load-directory
  'denote-explore-network-filename "3.1")

(defvar denote-explore-network-previous
  nil
  "Store the previous network configuration to regenerate the last graph.
Parameters define the previous network, i.e.:
- `(\"keywords\")'
- `(\"neighbourhood\" \"20240101T084408\" 3)'
- `(\"community\" \"regex\")'
- `(\"sequence) \"root signature\"'")

;;; STATISTICS
;; Count number of notes, attachments and keywords

;;;###autoload
(defun denote-explore-count-notes (&optional attachments)
  "Count number of Denote notes and attachments.
A note is defined by `denote-file-types', anything else is an attachment.
Count only ATTACHMENTS by prefixing with universal argument."
  (interactive "P")
  (let* ((all-files (length (denote-directory-files)))
	 (denote-files (length (denote-directory-files nil nil t)))
	 (attachment-files (- all-files denote-files)))
    (if attachments
	(message "%s attachments" attachment-files)
      (message "%s notes (%s attachments)" denote-files attachment-files))))

;;;###autoload
(defun denote-explore-count-keywords ()
  "Count distinct Denote keywords."
  (interactive)
  (let ((all-keywords (length (mapcan #'denote-extract-keywords-from-path
				      (denote-directory-files))))
	(distinct-keywords (length (denote-keywords))))
    (message "%s used keywords (%s distinct keywords)"
	     all-keywords distinct-keywords)))

;;;###autoload
(defun denote-explore-barchart-timeline ()
  "Draw a column chart with the number of notes per year."
  (interactive)
  (let* ((files (denote-directory-files))
	 (file-names (mapcar #'file-name-nondirectory files))
	 ;; Extract years from file names
	 (years (mapcar (lambda (file)
			  (substring file 0 4))
			file-names))
	 (years-table (denote-explore--table years))
	 (years-table-sorted (sort years-table
				   (lambda (a b)
				     (string< (car a) (car b))))))
    (denote-explore--barchart years-table-sorted
			      "Year"
			      "Denote notes and attachments timeline")))

;;; RANDOM WALKS
;; Jump to a random note, random linked note or random note with selected tag(s).
;; With universal argument the sample includes attachments.

(defun denote-explore--jump (files)
  "Jump to a random note in the FILES file list.

Exclude riles matching regx in `denote-explore-random-regex-ignore'.

- `denote-explore-random-note': Jump to a random Denote file.
- `denote-explore-random-regex': Jump to a random Denote file that matches a
  regular expression.
- `denote-explore-random-link': Jump to a random linked note (either forward or
  backward) or attachments (forward only).
- `denote-explore-random-keyword': Jump to a random Denote file with the same
  selected keyword(s)."
  (let* ((ignore (if denote-explore-random-regex-ignore
		     (denote-directory-files denote-explore-random-regex-ignore)
		   nil))
	(denote-sample (cl-set-difference files ignore :test 'string=)))
    (find-file (nth (random (length denote-sample)) denote-sample))))

;;;###autoload
(defun denote-explore-random-note (&optional include-attachments)
  "Jump to a random Denote file and optionally INCLUDE-ATTACHMENTS.
With universal argument the sample includes attachments."
  (interactive "P")
  (if-let ((denotes (denote-directory-files nil t (not include-attachments))))
      (denote-explore--jump denotes)
    (user-error "No Denote files found")))

;;;###autoload
(defun denote-explore-random-link (&optional attachments)
  "Jump to a random linked from current buffer note.
With universal argument the sample includes links to ATTACHMENTS."
  (interactive "P")
  ;; Gather links
  (let* ((forward-links (denote-link-return-links))
	 (back-links (denote-link-return-backlinks))
	 (all-links (append forward-links back-links))
	 (links (if attachments
		    all-links
		  (seq-filter #'denote-file-is-note-p all-links))))
    (if links
	(denote-explore--jump links)
      (message "Not a Denote file or no (back)links in or to this buffer"))))

(defun denote-explore--retrieve-keywords (file)
  "Retrieve alphabetised list of keywords from Denote FILE.
Uses front matter for notes and the filename for attachments."
  (let* ((file (if (eq file nil) "" file))
	 (filetype (denote-filetype-heuristics file))
         (raw-keywords (if (denote-file-is-note-p file)
                           (denote-retrieve-keywords-value file filetype)
                         (denote-retrieve-filename-keywords file)))
         (keywords (cond ((or (null raw-keywords) (equal raw-keywords "")) nil)
                         ((stringp raw-keywords) (split-string raw-keywords "_"))
                         (t raw-keywords))))
    (when keywords (sort keywords 'string<))))

(defun denote-explore--select-keywords ()
  "Select Denote keyword(s) for random jump.
- Use \"*\" to select all listed keywords.
- If the current buffer has no Denote keywords, then choose from all available."
  (let* ((raw-keywords (denote-explore--retrieve-keywords (buffer-file-name)))
	 (buffer-keywords (if (null raw-keywords)
			      (denote-keywords)
			    raw-keywords))
	 (keywords (if (> (length buffer-keywords) 1)
		       (delete-dups
			(sort (completing-read-multiple
			       "Select keyword(s) (* selects all available keywords): "
			       buffer-keywords)
			      #'string<))
		     buffer-keywords)))
    (if (string= (car keywords) "*") buffer-keywords keywords)))

;;;###autoload
(defun denote-explore-random-keyword (&optional include-attachments)
  "Jump to a random note with selected keyword(s).

- Select one or more keywords from the active Denote buffer.
- Override the completion option by adding free text
- Use \"*\" to select all listed keywords.

Selecting multiple keywords requires `denote-sort-keywords' to be non-nil
or the target keywords are in the same order as the selection.

With universal argument the sample will INCLUDE-ATTACHMENTS."
  (interactive "P")
  (if-let* ((keyword-list (denote-explore--select-keywords))
	    (keyword-regex (concat "_" (mapconcat #'identity keyword-list ".*_"))))
      (denote-explore-random-regex keyword-regex include-attachments)))

;;;###autoload
(defun denote-explore-random-regex (regex &optional include-attachments)
  "Jump to a random not matching a regular expression REGEX.
Use Universal Argument to INCLUDE-ATTACHMENTS"
  (interactive "sRegular expression: \nP")
  (if-let ((sample (denote-directory-files regex t (not include-attachments))))
      (denote-explore--jump sample)
    (message "No matching Denote files found")))

;;; JANITOR
;; The Janitor provides various functions to maintain a Denote file collection.

(defun denote-explore--table (list)
  "Generate an ordered frequency table from a LIST."
  (sort (-frequencies list)
	(lambda (a b) (> (cdr a) (cdr b)))))

(defun denote-explore--filter-duplicates (tally)
  "Filter duplicates from an alist TALLY with frequencies.
Generate tally with `denote-explore-table'."
  (mapcar #'car (cl-remove-if-not
                   (lambda (note)
		     (> (cdr note) 1))
		   tally)))

(defun denote-explore--duplicate-notes (exports)
  "Find duplicate Denote files.
When EXPORTS, use complete filenames, else use Denote identifiers
and exclude exported Org files."
  ;; Count each unique identifier or filename
  (let* ((files (denote-directory-files))
	 (filenames (mapcar (lambda (path)
				   (file-name-sans-extension
				    (file-name-nondirectory path)))
				 files))
	 (ids (mapcar #'denote-retrieve-filename-identifier files))
	 (id-count (denote-explore--table ids))
	 (duplicate-ids (denote-explore--filter-duplicates id-count))
	 (file-count (denote-explore--table filenames))
	 (duplicate-filenames (denote-explore--filter-duplicates file-count))
	 (duplicate-filenames-ids (mapcar #'denote-retrieve-filename-identifier
					  duplicate-filenames)))
    (if exports
	(cl-set-difference duplicate-ids duplicate-filenames-ids :test 'string=)
      duplicate-ids)))

;;;###autoload
(defun denote-explore-duplicate-notes (&optional exclude-exports)
  "Identify duplicate Denote IDs and EXCLUDE-EXPORTS.

If EXCLUDE-EXPORTS is nil, check Denote IDs, otherwise use file names without
extension. Using the universal argument excludes exported Denote files from
duplicate detection.

Duplicate files are displayed in a temporary buffer with links to the
suspected duplicates."
  (interactive "P")
  (message "Finding duplicated notes")
  (if-let* ((duplicates (denote-explore--duplicate-notes exclude-exports)))
      (with-current-buffer-window "*denote-duplicates*" nil nil
        (erase-buffer)
	(insert "#+title: Duplicate Denote files ")
	(if exclude-exports
	    (insert "(Excluding exports)")
	  (insert "(Including exports)"))
	(insert "\n#+date: ")
	(org-insert-time-stamp (current-time) t t)
	(insert "\n\n")
	(if exclude-exports
	    (insert "Run without universal argument =C-u= to include exported files.\n")
	  (insert "Run with universal argument =C-u= to exclude exported files.\n"))
        (dolist (id duplicates)
          (insert (format "\n* Note ID [[denote:%s]]\n\n" id))
          (dolist (filename (denote-directory-files id))
            (insert (format " - [[file:%s][%s]]\n"
                            filename
                            (funcall denote-link-description-function filename)))))
        (org-mode)
        (read-only-mode))
    (message "No duplicates found")))

(define-obsolete-function-alias
  'denote-explore-identify-duplicate-identifiers
  'denote-explore-identify-duplicate-notes "1.2")

(define-obsolete-function-alias
  'denote-explore-identify-duplicate-notes
  'denote-explore-duplicate-notes "3.3")

;;;###autoload
(defun denote-explore-duplicate-notes-dired (&optional filenames)
  "Identify duplicate Denote IDs or FILENAMES.

If FILENAMES is nil, check Denote IDs, otherwise use complete file names.
Using the FILENAMES option (or using the universal argument) excludes
exported Denote files from duplicate-detection.

Duplicate files are displayed `find-dired'."
  (interactive "P")
  (if-let* ((duplicates (denote-explore--duplicate-notes filenames)))
      (find-dired denote-directory
                  (mapconcat (lambda (id)
                               (format "-name '%s*'" id))
                             duplicates
                             " -o "))
    (message "No duplicates found")))

(define-obsolete-function-alias
  'denote-explore-identify-duplicate-notes-dired
  'denote-explore-duplicate-notes-dired "3.3")

;;;###autoload
(defun denote-explore-single-keywords ()
  "Select a note or attachment with a keyword that is only used once."
  (interactive)
  ;; Count keywords and find singles
  (if-let* ((keywords (mapcan #'denote-extract-keywords-from-path
			      (denote-directory-files)))
	    (keywords-count (denote-explore--table keywords))
	    (single-keywords (mapcar #'car (cl-remove-if-not
					    (lambda (note)
					      (= (cdr note) 1))
					    keywords-count)))
	    (selected-keyword (if single-keywords
				  (completing-read "Select single keyword: "
						   single-keywords))))
      (find-file (car (denote-directory-files (concat "_" selected-keyword))))
    (message "No single keywords in use.")))

;;;###autoload
(defun denote-explore-zero-keywords ()
  "Select a note or attachment without any keywords."
  (interactive)
  ;; Find all notes without an underscore
  (if-let* ((zero-keywords (denote-directory-files "^[^_]*$")))
      (find-file (completing-read "Select file with zero keywords: "
				  zero-keywords))
    (message "All files have keywords.")))

(define-obsolete-function-alias
  'denote-explore--alphabetical-p
  nil "3.3")

(define-obsolete-function-alias
  'denote-explore-sort-keywords
  'denote-explore-sync-metadata "3.3")

;;;###autoload
(defun denote-explore-rename-keyword ()
  "Rename or remove keyword(s) across the Denote collection.

Use an empty string as new keyword to remove the selection.

When selecting more than one existing keyword, all selections are renamed
to the new version or removed.

The filename is taken as the source of truth for attchments and the front matter
for notes.

All open Denote note buffers should be saved for this function to work reliably."
  (interactive)
  ;; Save any open Denote files
  (save-some-buffers nil #'(lambda ()
			     (denote-filename-is-note-p buffer-file-name)))
  ;; Select keywords and file candidates
  (let* ((selected-keyword (denote-keywords-prompt "Keyword(s) to rename"))
         (keywords-regex (mapconcat
			  (lambda (keyword) (concat "_" keyword)) selected-keyword "\\|"))
         (files (denote-directory-files keywords-regex))
	 (new-keyword (read-from-minibuffer "New keyword: ")))
    ;; Loop through candidates
    (dolist (file files)
      (let* ((denote-rename-confirmations '(rewrite-front-matter modify-file-name))
             (denote-sort-keywords t)
	     (denote-known-keywords nil)
	     (current-keywords (denote-explore--retrieve-keywords file))
	     (new-keywords (if (equal new-keyword "")
			       (cl-set-difference current-keywords selected-keyword :test 'string=)
			     (mapcar (lambda (keyword)
				       (if (member keyword selected-keyword) new-keyword keyword))
				     current-keywords)))
	     (file-type (denote-filetype-heuristics file)))
1        (denote-rename-file file
	 		    (denote-retrieve-title-or-filename file file-type)
			    (if (equal new-keywords nil) "" (delete-dups new-keywords))
			    (or (denote-retrieve-filename-signature file) "")
			    (denote-retrieve-filename-identifier file))))))

(define-obsolete-function-alias
  'denote-explore--retrieve-title
  'denote-retrieve-title-or-filename "1.4.2")

;;;###autoload
(defun denote-explore-sync-metadata ()
  "Synchronise filenames with the metadata for all Denote notes.
The front matter is the source of truth. Keywords are saved alphabetically.
All open Denote note buffers need to be saved before invoking this function."
  (interactive)
  ;; Save open Denote notes
  (save-some-buffers nil #'(lambda ()
                             (denote-filename-is-note-p buffer-file-name)))
  (let ((denote-rename-confirmations '(rewrite-front-matter modify-file-name))
	(denote-sort-keywords t)
	(notes (denote-directory-files nil nil t)))
    ;; Construct new file names and rename when different
    (dolist (file notes)
      (let* ((dir (file-name-directory file))
	     (file-type (denote-filetype-heuristics file))
	     (id (denote-retrieve-filename-identifier file))
	     (title (denote-retrieve-front-matter-title-value file file-type))
	     (signature (denote-retrieve-filename-signature file))
	     (keywords (denote-retrieve-front-matter-keywords-value file file-type))
	     (file-keywords (denote-keywords-combine keywords))
	     (ext (file-name-extension file t))
	     (file-name (denote-format-file-name dir id keywords title ext signature)))
	(message file-name)
	(if (not (string= file file-name))
	    (denote-rename-file-using-front-matter file))))
    (message "Integrity check completed")))

(defun denote-explore--missing-denote-links ()
  "List all missing Denote link targets as an association list."
  (let* ((files (denote-directory-files))
	 (ids (mapcar #'denote-retrieve-filename-identifier files))
	 (links (denote-explore--network-extract-edges files)))
    (seq-filter
     (lambda (link)
       (not (member (cdr (assoc 'target link)) ids)))
     links)))

(defun denote-explore--missing-file-links ()
  "List all missing file links in Denote notes as association lists."
  (let* ((files (denote-directory-files nil nil t))
         (matches
          (mapcar (lambda (match)
                    (cons (xref-location-group (xref-match-item-location match))
                          (xref-match-item-summary match)))
                  (xref-matches-in-files "\\[\\[file:.*?\\]\\]" files))))
    (seq-filter 'identity
                (apply 'append
                       (mapcar (lambda (pair)
                                 (let* ((source-file (car pair))
                                        (match-text (cdr pair))
                                        (filenames (when match-text
                                                     (let (results)
                                                       (while (string-match "\\[\\[\\(?:file:\\)?\\(.*?\\.[^]]+\\)\\]\\]" match-text)
                                                         (push (match-string 1 match-text) results)
                                                         (setq match-text (substring match-text (match-end 0))))
                                                       (reverse results)))))
                                   (mapcar (lambda (filename)
                                             (let ((clean-filename (expand-file-name filename denote-directory)))
                                               (when (not (file-exists-p clean-filename))
                                                 (list (cons 'source (denote-retrieve-filename-identifier source-file))
                                                       (cons 'target clean-filename)))))
                                           filenames)))
                               matches)))))

;;;###autoload
(defun denote-explore-missing-links ()
  "Display a read-only Org buffer with missing Denote and file links.

Follow the links in the tables to review the suspect links."
  (interactive)
  (message "Searching for missing Denote and file links")
  (let ((missing-denote-links (denote-explore--missing-denote-links))
        (missing-file-links (denote-explore--missing-file-links))
	(missing-links-buffer "*Missing Denote and file links*"))
    (with-current-buffer-window  missing-links-buffer nil nil
      (erase-buffer)
      (org-mode)
      (insert "#+title: List of missing Denote and file links\n"
	      "#+date: ")
      (org-insert-time-stamp (current-time) t t)
      (insert "\n\nFollow the hyperlinks to remove or repair dead links.\n\n"
	      "To disable confirmations, customise ~org-link-elisp-confirm-function~.\n")
      (denote-explore--insert-missing-links "Denote" missing-denote-links)
      (denote-explore--insert-missing-links "file" missing-file-links))
    (pop-to-buffer missing-links-buffer)))

(defun denote-explore--insert-missing-links (type missing-links)
  "Insert table of MISSING-LINKS of TYPE (Denote or file) in an Org mode buffer."
  (let ((plural-p (if (> (length missing-links) 1) "s" "")))
    (insert (format "\n* Missing %s link%s\n" type plural-p))
    (insert (format "%s missing %s link%s\n\n"
		    (length missing-links) type plural-p))
    (insert "|-\n| Source | Target |\n|-\n")
    (dolist (link missing-links)
      (let* ((source (cdr (assoc 'source link)))
	     (target-raw (cdr (assoc 'target link)))
	     (target (if (eq type "Denote")
			 target-raw
		       (file-name-nondirectory target-raw)))
	     (file (car (denote-directory-files source)))
	     (file-type (denote-filetype-heuristics file))
	     (title (denote-retrieve-front-matter-title-value file file-type)))
	(insert "|[[elisp:(denote-explore--review-dead-link \""
		source "\" \"" target "\")][" title "]]")
	(insert "|" target "|\n")))
    (insert "|-\n"))
  (org-table-align))

(define-obsolete-function-alias
  'denote-explore-dead-links
  'denote-explore-missing-links "3.3.2")

(defun denote-explore--review-dead-link (source target)
  "Jump to the location of a dead link to TARGET found in SOURCE."
  (let* ((file (car (denote-directory-files source)))
	 (buffer (find-file-noselect file)))
    (pop-to-buffer buffer)
    (find-file file)
    (goto-char (point-min))
    (when org-link-descriptive (org-toggle-link-display))
    (if (search-forward-regexp target nil t)
	(message "Missing link found")
      (message "Link not found: %s" target))))

;;; VISUALISATION

;; Bar charts
;; Leverages the built-in chart package for plain text visualisation.

(defun denote-explore--barchart (table var title &optional n horizontal)
  "Create a barchart from a frequency TABLE with top N entries.
VAR and TITLE used for display."
  (chart-bar-quickie
   (if horizontal 'horizontal 'vertical)
   title
   (mapcar #'car table) var
   (mapcar #'cdr table) "Frequency" n))

;;;###autoload
(defun denote-explore-barchart-keywords (n)
  "Create a barchart with the top N most used Denote keywords."
  (interactive "nNumber of keywords: ")
  (let* ((keywords (mapcan #'denote-extract-keywords-from-path
			   (denote-directory-files)))
	 (keywords-table (denote-explore--table keywords)))
    (denote-explore--barchart
     keywords-table
     (concat "Top-" (number-to-string n) " Denote Keywords")
     (denote-explore-count-keywords) n)))

(define-obsolete-function-alias
  'denote-explore-keywords-barchart
  'denote-explore-barchart-keywords "3.0")

;;;###autoload
(defun denote-explore-barchart-filetypes (&optional attachments)
  "Visualise the Denote file types for notes and/or attachments.
With universal argument only visualises ATTACHMENTS, which excludes file
types listed in `denote-file-type-extensions'."
  (interactive "P")
  (let* ((files (if attachments
		    (cl-remove-if #'denote-file-is-note-p (denote-directory-files))
		  (denote-directory-files)))
	 (extensions (mapcar (lambda(file) (file-name-extension file))
			     files))
	 (ext-list (if attachments
		       (cl-set-difference extensions
					  (denote-file-type-extensions)
					  :test 'equal)
		     extensions))
	 (title (if attachments
		    (denote-explore-count-notes :attachments)
		  (denote-explore-count-notes))))
    (denote-explore--barchart
     (denote-explore--table ext-list) "Denote file extensions" title)))

(define-obsolete-function-alias
  'denote-explore-extensions-barchart
  'denote-explore-barchart-filetypes "3.0")

(defun denote-explore--network-sum-degrees (nodes)
  "Sum the degrees in NODES, producing a new alist with degree counts."
  (let ((degree-sums ()))
    (dolist (entry nodes degree-sums)
      (let* ((degree (cdr (assoc 'degree entry)))
             (current-count (cdr (assoc degree degree-sums))))
        (if current-count
            (setcdr (assoc degree degree-sums) (1+ current-count))
          (push (cons degree 1) degree-sums))))
    (sort degree-sums (lambda (a b) (< (car a) (car b))))))

;;;###autoload
(defun denote-explore-barchart-degree (&optional text-only)
  "Visualise the degree for each Denote file (total links and backlinks).
The universal argument includes TEXT-ONLY files in the analyis."
  (interactive "P")
  (message "Analysing Denote network ...")
  (let* ((graph (denote-explore-network-community-graph "" text-only))
	 (nodes (cdr (assoc 'nodes graph)))
	 (degrees (denote-explore--network-sum-degrees nodes))
	 (txt-degrees (mapcar (lambda (pair)
				(cons (number-to-string (car pair)) (cdr pair)))
			      degrees)))
    (denote-explore--barchart txt-degrees "Degree" "Node degree distribution")))

(define-obsolete-function-alias
  'denote-explore-degree-barchart
  'denote-explore-barchart-degree "3.0")

;;;###autoload
(defun denote-explore-barchart-backlinks (n)
  "Visualise the number of backlinks for N nodes in the Denote network."
  (interactive "nNumber of nodes: ")
  (message "Analysing Denote network ...")
  (let* ((graph (denote-explore-network-community-graph "" t))
	 (nodes (cdr (assoc 'nodes graph)))
         (backlinks (mapcar (lambda (node)
                              (cons
                               (cdr (assq 'name node))
                               (cdr (assq 'backlinks node))))
                            nodes)))
    (sort backlinks (lambda (a b)
                      (> (cdr a) (cdr b))))
    (denote-explore--barchart backlinks "Backlinks" "Node backlinks distribution" n t)))

(define-obsolete-function-alias
  'denote-explore-backlinks-barchart
  'denote-explore-barchart-backlinks "3.0")

(defun denote-explore--idenitfy-isolated (&optional text-only)
  "Identify Denote files without (back)links.
Using the universal argument provides TEXT-ONLY files (excludes attachments)."
  (let* ((all-files (denote-directory-files nil nil text-only))
         (files (if denote-explore-isolated-ignore-keywords
                    (-filter (lambda (filename)
                               (not (string-match-p (rx-to-string '(and  ?_ (eval `(or ,@denote-explore-isolated-ignore-keywords))))
                                                    filename)))
                             all-files)
                  all-files))
	 (all-ids (mapcar #'denote-retrieve-filename-identifier files))
	 (edges (denote-explore--network-extract-edges files))
	 (linked-ids (denote-explore--network-extract-unique-nodes edges))
	 (isolated-ids (seq-remove (lambda (id) (member id linked-ids)) all-ids)))
    (-map (lambda (id) (-filter (lambda (f) (string-match-p id f)) files))
	  isolated-ids)))

;;;###autoload
(defun denote-explore-isolated-files (&optional text-only)
  "Identify Denote files without (back)links.
Using the universal argument excludes attachments (TEXT-ONLY).

Files which have keywords listed in
`denote-explore-isolated-ignore-keywords' will not be included."
  (interactive "P")
  (message "Searching for isolated files ...")
  (let ((isolated (denote-explore--idenitfy-isolated text-only)))
    (find-file (completing-read "Select isolated file: " isolated))))

;;; DEFINE GRAPHS
;; Define various graph types as an association list

;; Community graph
(define-obsolete-function-alias
  'denote-explore--network-zip-alists
  'nil "3.3")

(defun denote-explore--network-extract-edges (files)
  "Extract forward links as network edges from FILES."
  (let* ((text-files (seq-filter #'denote-file-is-note-p files))
	 (links-xref (xref-matches-in-files
		      "\\[denote:[0-9]\\{8\\}T[0-9]\\{6\\}" text-files))
	 (source (mapcar #'denote-retrieve-filename-identifier
			 (mapcar
			  #'xref-location-group
			  (mapcar #'xref-match-item-location
				  links-xref))))
	 (links (mapcar #'substring-no-properties
			(mapcar #'xref-match-item-summary
				links-xref)))
	 (target (mapcar
		  (lambda (str)
		    (when (string-match denote-id-regexp str)
		      (match-string 0 str)))
		  links))
	 ;; Zip this lists as an alist (((source . "a") (target "b")) (...))
	 (all-edges (-map (lambda (pair)
			    `((source . ,(car pair)) (target . ,(cdr pair))))
			  (-zip-pair source target))))
    all-edges))

(define-obsolete-function-alias
  'denote-explore--extract-vertices
  'denote-explore--network-extract-node "1.2")

(defun denote-explore--network-extract-node (file)
  "Extract metadata for note or attachment FILE."
  (when (file-exists-p file)
    (let ((id (denote-retrieve-filename-identifier file))
	  (signature (denote-retrieve-filename-signature file))
	  (name (denote-retrieve-title-or-filename
		 file (denote-filetype-heuristics file)))
	  (keywords (denote-retrieve-filename-keywords file))
	  (type (file-name-extension file)))
      (setq keywords (if keywords (string-split keywords "_") ""))
      `((id . ,id) (signature . ,signature) (name . ,name)
	(keywords . ,keywords) (type . ,type) (filename . ,file)))))

(defun denote-explore--network-prune-edges (nodes edges)
  "Select EDGES (links) where both source and target are part of NODES.
Prunes any edges that link to outside a community of NODES."
  (let ((filtered-edges '()))
    (dolist (edge edges filtered-edges)
      (let ((source (cdr (assoc 'source edge)))
            (target (cdr (assoc 'target edge))))
        (when (and (member source nodes) (member target nodes))
          (push edge filtered-edges))))
    (nreverse filtered-edges)))

(defun denote-explore--network-count-edges (edges)
  "Count occurrences of EDGES to set their weight.

The result is an association list of all edges and their weights:
`((source . id1) (source . id2) (weight . w))'."
  (let* ((edges-count (-frequencies edges))
	 (edges-alist (mapcar (lambda (item)
				(let ((source (cdr (assoc 'source (car item))))
				      (target (cdr (assoc 'target (car item))))
				      (weight (cdr item)))
				  `((source . ,source)
				    (target . ,target)
				    (weight . ,weight))))
			      edges-count)))
    edges-alist))

(defun denote-explore--network-degree (nodes edges)
  "Calculate the degree of nodes in a network graph NODES and EDGES.
The degree of a Denote graph node is defined by the sum of links and backlinks
of a file."
  (mapcar (lambda (node)
            (let ((node-id (cdr (assoc 'id node)))
                  (degree 0))
	      (dolist (edge edges)
                (when (or (string= node-id (cdr (assoc 'source edge)))
                          (string= node-id (cdr (assoc 'target edge))))
                  (setq degree (+ degree 1))))
	      (append node (list (cons 'degree degree)))))
	  nodes))

(defun denote-explore--network-backlinks (nodes edges)
  "Calculate the number of backlinks for NODES and EDGES."
  (mapcar (lambda (node)
            (let ((node-id (cdr (assoc 'id node)))
                  (backlinks 0))
              (dolist (edge edges)
                (when (string= node-id (cdr (assoc 'target edge)))
                  (setq backlinks (+ backlinks (cdr (assoc 'weight edge))))))
              (append node (list (cons 'backlinks backlinks)))))
          nodes))

(defun denote-explore--network-filter-files (files)
  "Remove files matching `denote-explore-network-regex-ignore' from Denote FILES.
Removes selected files from neighbourhood or community visualisation."
  (let ((ignore (if denote-explore-network-regex-ignore
		    (denote-directory-files denote-explore-network-regex-ignore)
		  nil)))
    (cl-set-difference files ignore :test 'string=)))

(defun denote-explore-network-community-graph (regex &optional text-only)
  "Generate network community association list for note matching REGEX.
Optionally include TEXT-ONLY files (no attachments).
Links to notes outside the search area are pruned."
  (if-let* ((files (denote-explore--network-filter-files
		    (denote-directory-files regex nil text-only)))
	    (ids (mapcar #'denote-extract-id-from-string files))
	    (edges (denote-explore--network-extract-edges files))
	    (edges-pruned (denote-explore--network-prune-edges ids edges))
	    (edges-alist (denote-explore--network-count-edges edges-pruned))
	    (nodes (mapcar #'denote-explore--network-extract-node files))
	    (nodes-degrees (denote-explore--network-degree nodes edges-alist))
	    (nodes-alist (denote-explore--network-backlinks nodes-degrees edges-alist))
	    (meta-alist `((directed . t) (type . "Community") (parameters ,regex))))
      `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))
    (user-error "No Denote files or (back)links found for %s" regex)))

(defun denote-explore-network-community (&optional text-only)
  "Define inputs for a network community and generate graph.
Optionally include TEXT-ONLY files."
  (let ((regex (read-from-minibuffer
		"Enter regular expression (empty string for all notes):")))
    (setq denote-explore-network-previous `("Community" ,regex))
    (message "Building graph for \"%s\" community " regex)
    (denote-explore-network-community-graph regex text-only)))

;;; keywords graph
(defun denote-explore--network-keywords-extract (files)
  "Convert keywords from FILES to a list of lists.
Notes with only one keyword and keywords listed in
`denote-explore-network-keywords-ignore' are ignored."
  (let ((keywords (mapcar #'denote-retrieve-filename-keywords files))
	(processed-keywords '()))
    (dolist (keyword keywords processed-keywords)
      (when keyword
        (let* ((split-keywords (split-string keyword "_"))
               (filtered-keywords
		(seq-remove
		 (lambda (k)
		   (member k denote-explore-network-keywords-ignore))
		 split-keywords)))
          (when (> (length filtered-keywords) 1)
            (push filtered-keywords processed-keywords)))))
    (nreverse processed-keywords)))

(defun denote-explore--network-keyword-edges (keywords)
  "Generate complete graph from list of KEYWORDS.
In a complete graph (network), all  nodes are connected to each other."
  (let ((network '())
	(length (length keywords)))
    (dotimes (i length)
      (dotimes (j length)
        (unless (or (= i j) (> i j)) ; Avoid duplicate and reversed pairs
          (let ((source (nth i keywords))
                (target (nth j keywords)))
            (push (list (cons 'source source)
			(cons 'target target)) network)))))
    network))

(defun denote-explore-network-keywords (&optional text-only)
  "Enter a positive integer for a minimum weight and generate a keywords graph.
Optionally analyse TEXT-ONLY files."
  (let ((min-weight (read-number "Enter min-weight (integer > 0): " 1)))
    (while (<= min-weight 0)
      (setq min-weight (read-number "Invalid input. Enter an integer > 0: " 1)))
    (setq denote-explore-network-previous `("Keywords" ,min-weight))
    (denote-explore-network-keywords-graph min-weight text-only)))

(defun denote-explore--network-keywords-flatten-edges (edges)
  "Flatten list of network EDGES."
  (let ((edges-alist '()))
    (dolist (sublist edges)
      (dolist (edge sublist)
	(push edge edges-alist)))
    edges-alist))

(defun denote-explore-network-keywords-graph (min-weight text-only)
  "Generate Denote graph object from keywords with MIN-WEIGHT edges.
Optionally analyse TEXT-ONLY files."
  (let* ((files (denote-directory-files nil nil text-only))
	 (keywords (denote-explore--network-keywords-extract files))
	 (edges (mapcar #'denote-explore--network-keyword-edges keywords))
	 (all-edges-alist (denote-explore--network-count-edges
			   (denote-explore--network-keywords-flatten-edges edges)))
	 (edges-alist (cl-remove-if-not
		       (lambda (edge) (>= (cdr (assoc 'weight edge)) min-weight))
		       all-edges-alist))
	 (unique-nodes (denote-explore--network-extract-unique-nodes edges-alist))
	 (nodes '())
	 (nodes (dolist (node unique-nodes nodes)  ; Iterating over nodes
		  (push (list (cons 'id node) (cons 'name node)) nodes)))
	 (nodes-alist (denote-explore--network-degree (nreverse nodes) edges-alist))
	 (meta-alist `((directed . nil) (type . "Keywords") (parameters . ,min-weight))))
    `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))))

;;; Neighbourhood Graph
(defun denote-explore--network-extract-unique-nodes (edges-alist)
  "Extract all unique `source' and `target' nodes from EDGES-ALIST."
  (delete-dups
   (mapcan (lambda (entry)
	     (list (cdr (assoc 'source entry))
		   (cdr (assoc 'target entry))))
	   edges-alist)))

(define-obsolete-function-alias
  'denote-explore--network-find-edges nil "3.2.1")

(defun denote-explore--network-unique-edges (edges-a edges-b)
  "Return a list of unique edges from EDGES-A and EDGES-B."
  (let ((edges (append edges-a edges-b))
        (seen '())
        (unique '()))
    (dolist (edge edges)
      (unless (member edge seen)
        (push edge seen)
        (push edge unique)))
    unique))

(defun denote-explore--network-neighbourhood-edges (file depth text-only)
  "Itteratively Find all links to and from FILE up to DEPTH or less.
Optionally include TEXT-ONLY files."
  ;; Set initial conditions and define all edges as search space
  (let* ((all-files (denote-directory-files nil nil text-only))
	 (ids (mapcar #'denote-retrieve-filename-identifier all-files))
	 (all-file-edges (denote-explore--network-extract-edges all-files))
	 ;; Remove dead links (where target does not exist)
	 (all-edges (seq-filter (lambda (pair)
				  (member (cdr (assoc 'target pair)) ids))
				all-file-edges))
	 (current-files (list file))
	 (ids (mapcar #'denote-retrieve-filename-identifier current-files))
	 (neighbourhood-edges '())
         (current-depth 0))
    ;; Loop through until all steps are done or no more new links
    (while (and current-files (< current-depth depth))
      ;; Find edges for each file
      (let* ((new-edges (denote-explore--network-extract-neighbourhood-edges
			 all-edges current-files))
	     (edge-ids (denote-explore--network-extract-unique-nodes new-edges))
	     ;; Remove files already analysed
	     (new-ids (-difference edge-ids ids)))
	;; Replace current files with new ids
	(setq current-files (-filter
			     (lambda (file)
			       (-some (lambda (id) (string-match-p id file)) new-ids))
			     all-files))
	;; Append ids allready analysed
	(setq ids (append ids new-ids))
	;; Add new edges to neighbourhood
	(setq neighbourhood-edges (denote-explore--network-unique-edges
				   neighbourhood-edges new-edges))
	(setq current-depth (1+ current-depth)))
      (message "Itteration %s: %s nodes and %s edges identifed"
	       current-depth (length ids)(length neighbourhood-edges)))
    neighbourhood-edges))

(defun denote-explore--network-extract-neighbourhood-edges (edges files)
  "Generate alist of edges at one link deep for FILES using EDGES."
  (let ((neighbourhood-edges '()))
    (dolist (file files)
      (let* ((id (denote-extract-id-from-string file))
	     (f-target (delq nil
			     (mapcar (lambda (entry)
				       (when (equal (cdr (assoc 'source entry)) id)
					 (cdr (assoc 'target entry))))
				     edges)))
	     (f-source (make-list (length f-target) id))
	     (b-source (delq nil
			     (mapcar (lambda (entry)
				       (when (equal (cdr (assoc 'target entry)) id)
					 (cdr (assoc 'source entry))))
				     edges)))
	     (b-target (make-list (length b-source) id))
	     (source (append f-source b-source))
	     (target (append f-target b-target))
	     (new-edges (-map (lambda (pair)
				`((source . ,(car pair)) (target . ,(cdr pair))))
			      (-zip-pair source target))))
	(setq neighbourhood-edges (append neighbourhood-edges new-edges))))
    neighbourhood-edges))

(defun denote-explore-network-neighbourhood (&optional text-only)
  "Obtain inputs and define a neighbourhood graph association list.
Uses the ID of the current Denote buffer, or user selects a root node.
With TEXT-ONLY, exclude attachments in the network."
  (let* ((buffer (or (buffer-file-name) ""))
	 ;; If buffer is a Denote file use it, else use non-isolated notes
         (file (if (denote-file-is-note-p buffer)
                   buffer
		 (let* ((isolated (denote-explore--idenitfy-isolated t))
			(notes (denote-directory-files nil t t))
			(candidates (cl-set-difference notes isolated)))
		   (completing-read "Select file: " candidates))))
         (depth (let ((input (read-from-minibuffer
			      "Search depth (default 2, integer >= 1): " "2")))
                  (max 1 (string-to-number (if (string-empty-p input) "2" input)))))
         (id (denote-retrieve-filename-identifier file)))
    (setq denote-explore-network-previous `("Neighbourhood" (,id ,depth)))
    (denote-explore-network-neighbourhood-graph `(,id ,depth) text-only)))

(defun denote-explore-network-neighbourhood-graph (parameters &optional text-only)
  "Generate Denote neighbourhood graph with PARAMETERS.
PARAMETERS is a list of the root note identifier and the depth.
When TEXT-ONLY, exclude attachments from the graph."
  (if-let* ((id (car parameters))
	    (depth (cadr parameters))
	    (meta-alist `((directed . t) (type . "Neighbourhood") (parameters ,id ,depth)))
 	    (root (car (denote-directory-files id)))
	    ;; Find edges from root at depth
	    (edges (denote-explore--network-neighbourhood-edges root depth text-only))
	    (edges-alist (denote-explore--network-count-edges edges))
	    ;; Extract IDs from edges and create nodes
	    (ids (denote-explore--network-extract-unique-nodes edges-alist))
	    (denote-files (denote-directory-files))
	    (files (seq-filter
		    (lambda (file)
		      (cl-some (lambda (regex) (string-match regex file)) ids))
		    denote-files))
	    (nodes (mapcar #'denote-explore--network-extract-node files))
	    (nodes-alist-degree (denote-explore--network-degree nodes edges-alist))
	    (nodes-alist (denote-explore--network-backlinks nodes-alist-degree edges-alist)))
      `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))
    (user-error "No network neighbourhood found")))

;; Sequence Graphs
(defun denote-explore-network-sequence (text-only)
  "Generate a graph of signature sequences from a selected root node.
Optionally analyse TEXT-ONLY files."
  (let* ((signature-files (denote-explore--network-filter-files
			   (denote-directory-files denote-signature-regexp nil text-only)))
	 (signatures (mapcar #'denote-retrieve-filename-signature signature-files))
	 (root (completing-read "Select root node (empty for all)" signatures)))
    (setq denote-explore-network-previous `("Sequence" ,root))
    (denote-explore-network-sequence-graph root text-only)))

(defun denote-explore-network-sequence-graph (root text-only)
  "Generate a Denote graph object from signature sequences for ROOT.
Optionally analyse TEXT-ONLY files."
  (let* ((all-files (denote-directory-files (concat "==" root) nil text-only))
	 (files (denote-explore--network-filter-files all-files))
	 (sequences (denote-explore--network-sequence-edges files))
	 (edges (denote-explore--network-edges-from-sequences sequences files))
	 (edges-alist (denote-explore--network-count-edges edges))
	 (nodes (mapcar #'denote-explore--network-extract-node files))
	 (nodes-degrees (denote-explore--network-degree nodes edges-alist))
	 (nodes-alist (denote-explore--network-backlinks nodes-degrees edges-alist))
	 (meta-alist `((directed . t)
		       (type . ,(car denote-explore-network-previous))
		       (parameters ,(cadr denote-explore-network-previous)))))
    `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))))

(defun denote-explore--network-sequence-edges (files)
  "Create an edgle list of signatures from FILES."
  (if-let ((sequences (mapcar #'denote-retrieve-filename-signature files))
	   ((> (length files) 1)))
      ;; Extract edges from signatures
      (let ((result '()))
	(dolist (sequence sequences)
	  (let* ((parts (split-string sequence "="))
		 (target sequence)
		 (source (when (> (length parts) 1)
			   (mapconcat 'identity (butlast parts) "="))))
            (when source
	      (push `((source . ,source) (target . ,target)) result))))
	result)
    (user-error "Root node has no children")))

(defun denote-explore--network-edges-from-sequences (sequences files)
  "Replace signatures in SEQUENCES edge list with identifiers from FILES."
  (let* ((signature-map (denote-explore--network-signature-identifier-map files))
	 (edges (mapcar (lambda (seq)
			  (let ((source-id (cdr (assoc (cdr (assoc 'source seq)) signature-map)))
				(target-id (cdr (assoc (cdr (assoc 'target seq)) signature-map))))
			    `((source . ,source-id) (target . ,target-id))))
			sequences)))
    (seq-filter (lambda (seq)
                  (let ((source (cdr (assoc 'source seq))))
                    (and source (not (string= source "")))))
		edges)))

(defun denote-explore--network-signature-identifier-map (files)
  "Map signatures to identifiers from a list of FILES."
  (let (result)
    (dolist (file files result)
      (let ((signature (denote-retrieve-filename-signature file))
            (identifier (denote-retrieve-filename-identifier file)))
        (when (and signature identifier)
          (push (cons signature identifier) result))))))

;;; SAVE GRAPH
(defun denote-explore-network-encode-json (graph)
  "Encode a Denote GRAPH object to JSON and insert in a file."
  (insert (json-encode graph))
  (json-pretty-print-buffer))

(defun denote-explore-network-encode-graphviz (graph)
  "Encode a Denote GRAPH object to GraphViz and insert in a file."
  (let* ((meta (cdr (assoc 'meta graph)))
	 (type (cdr (assoc 'type meta)))
	 (nodes (cdr (assoc 'nodes graph)))
         (edges (cdr (assoc 'edges graph)))
	 (directed (cdr (assoc 'directed meta)))
	 (graphtype (list (if directed "digraph Denote {" "graph Denote {")))
         (dot-header-all (append graphtype denote-explore-network-graphviz-header))
	 (dot-header (if (not (string= type "Sequence"))
			 dot-header-all
		       (cl-remove-if (lambda (x) (string-match-p "layout=neato" x))
				     (mapcar (lambda (x)
					       (replace-regexp-in-string "circle" "square" x))
					     dot-header-all))))
	 (dot-content '())
	 (nb-core (when (string-match denote-id-regexp type) (match-string 0 type))))
    ;; Nodes
    (dolist (node nodes)
      (let* ((id (cdr (assoc 'id node)))
	     (core (if (equal nb-core id) "fillcolor=darkorchid" ""))
             (name (replace-regexp-in-string (rx ?\") "\\\\\"" (cdr (assoc 'name node))))
	     (degree (cdr (assoc 'degree node)))
	     (signature (cdr (assoc 'signature node)))
	     (label (if (string= type "Sequence")
			(concat "label=\"" signature "\"")
		      (if (or (> degree 2) (equal nb-core id)) (concat "xlabel=\"" name "\"") "")))
             (tags (mapconcat 'identity (cdr (assoc 'keywords node)) ", "))
             (type (cdr (assoc 'type node)))
	     (width (sqrt (+ degree 1)))
	     (file-name (cdr (assoc 'filename node)))
	     (hyperlink (if file-name file-name "#")))
	(push (format (concat "%S [%s tooltip=\"ID: %s\\nTitle: "
			      "%s\\nKeywords: %s\\nSignature: %s\\nType: %s\\n"
			      "Degree: %s\" width=%s %s URL=\"%s\"]\n")
                      id label id name tags signature type degree width core hyperlink)
	      dot-content)))
    ;; Edges
    (dolist (edge edges)
      (let* ((source (cdr (assoc 'source edge)))
             (target (cdr (assoc 'target edge)))
             (weight (cdr (assoc 'weight edge)))
             (arrow (if directed "->" "--")))
	(push (format "%S %s %S [penwidth=%s tooltip=\"%s %s %s (Weight: %s)\"]\n"
                      source arrow target weight source arrow target weight)
	      dot-content)))
    ;; Combine and return
    (insert (mapconcat #'identity dot-header "\n"))
    (insert "\n")
    (insert (mapconcat #'identity (nreverse dot-content)))
    (insert "}")))

(defun denote-explore--network-escape-xml (name)
  "Escape special XML characters in node NAME."
  (replace-regexp-in-string
   "&" "&amp;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string ">" "&gt;" name))))

(defun denote-explore-network-encode-gexf (graph)
  "Encode a Denote GRAPH object to GEXF and insert in a file."
  (let* ((nodes (cdr (assoc 'nodes graph)))
         (edges (cdr (assoc 'edges graph)))
	 (meta (cdr (assoc 'meta graph)))
	 (type (if (cdr (assoc 'directed meta)) "directed" "undirected"))
	 (lastmod (format-time-string "%F"))
	 (gexf-header `("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			"<gexf xmlns=\"http://gexf.net/1.3\" "
			"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \n"
			"xsi:schemaLocation=\"http://gexf.net/1.3 http://gexf.net/1.3/gexf.xsd\" "
			"version=\"1.3\">\n"
			"<meta lastmodifieddate=\"" ,lastmod "\">\n"
			"<creator>Emacs Denote-Explore package</creator>\n"
			"<description>A network of Denote files/keywords</description>\n"
			"</meta>\n"
			"<graph mode=\"static\" defaultedgetype=\"" ,type "\">\n"
			"<attributes class=\"node\">\n"
			"<attribute id=\"degree\" title=\"degree\" type=\"integer\"/>\n"
			"</attributes>\n"
			"<nodes>\n"))
	 (gexf-lines '()))
    (dolist (node nodes)
      (let* ((id (cdr (assoc 'id node)))
             (name (cdr (assoc 'name node)))
	     (degree (cdr (assoc 'degree node)))
	     (label (denote-explore--network-escape-xml name)))
        (push (format "<node id=%S label=%S>\n<attvalues>\n<attvalue for=\"degree\" value=\"%S\"/>\n</attvalues>\n</node>"
		      id label degree) gexf-lines)))
    (push "</nodes>\n<edges>\n" gexf-lines)
    (dolist (edge edges)
      (let ((source (cdr (assoc 'source edge)))
            (target (cdr (assoc 'target edge)))
	    (weight (cdr (assoc 'weight edge))))
        (push (format "<edge source=%S target=%S weight=\"%s\" />\n"
		      source target weight) gexf-lines)))
    (insert (mapconcat #'identity gexf-header))
    (insert (mapconcat #'identity (nreverse gexf-lines)))
    (insert "</edges>\n</graph>\n</gexf>")
    (indent-region (point-min) (point-max))))

(defun denote-explore--network-save (graph)
  "Save a GRAPH to disk in the `denote-explore-network-format'."
  (let* ((format (assoc denote-explore-network-format
			denote-explore-network-graph-formats))
	 (convert-fn (plist-get (cdr format) :encode))
	 (file-extension (plist-get (cdr format) :file-extension))
	 (file-name (file-name-concat denote-explore-network-directory
			    (concat denote-explore-network-filename file-extension))))
    (when (not (file-exists-p denote-explore-network-directory))
      (make-directory denote-explore-network-directory))
    (with-temp-file file-name (funcall convert-fn graph))
    (message "Graph data saved to %s" file-name)))

;;; VISUALISE NETWORK

(defun denote-explore-network-display-graphviz (gv-file)
  "Convert GraphViz GV-FILE to an SVG file and display in external application.
Output is saved to the `denote-explore-network-directory' in the
`denote-explore-network-graphviz-filetype' file format."
  (let* ((file-type denote-explore-network-graphviz-filetype)
	 (output-file (expand-file-name (concat denote-explore-network-filename "."
						file-type)
					denote-explore-network-directory))
	 (script-call (format "dot %s -T%s -o %s"
			      (shell-quote-argument gv-file)
			      file-type
			      (shell-quote-argument output-file)))
	 (exit-status))
    (message script-call)
    (delete-file output-file)
    (setq exit-status (shell-command script-call))
    (if (eq exit-status 0)
	(if (file-exists-p output-file)
	    (browse-url-default-browser output-file nil)
	  (user-error "No output file produced"))
      (user-error "GraphViz image generation unsuccessful"))))

(defun denote-explore-network-display-json (json-file)
  "Convert GraphViz JSON-FILE to an D3.js HTML file and display in system browser.
Output is saved to `denote-explore-network-directory'."
  ;; Add variable for template file.
  (let* ((template-file denote-explore-network-d3-template)
	 (output-file (expand-file-name (concat denote-explore-network-filename ".html")
					denote-explore-network-directory))
         (json-content (with-temp-buffer
                         (insert-file-contents json-file)
                         (buffer-string)))
	 (json-object (json-parse-string json-content))
	 (type (car (split-string
		     (gethash "type" (gethash "meta" json-object))
		     "\\s-+" t))))
    ;; Replace shortcodes with data
    (with-temp-buffer
      (insert-file-contents template-file)
      (goto-char (point-min))
      (while (re-search-forward "{{\\([^}]+\\)}}" nil t)
        (let ((variable-name (match-string 1)))
	  (cond
	   ((string= variable-name "graph-type")
	    (replace-match type t t))
	   ((string= variable-name "d3-js")
	    (replace-match denote-explore-network-d3-js t t))
	   ((string= variable-name "json-content")
	    (replace-match (format "%s" json-content) t t))
	   ((string= variable-name "d3-colourscheme")
	    (replace-match (format "%s" denote-explore-network-d3-colours) t t)))))
      (write-file output-file))
    (browse-url-default-browser output-file nil)))

(defun denote-explore-network-view ()
  "Recreate the most recently generated Denote graph with external software."
  (let* ((format (assoc denote-explore-network-format
			denote-explore-network-graph-formats))
	 (display-fn (plist-get (cdr format) :display))
	 (ext (plist-get (cdr format) :file-extension))
	 (graph-file (expand-file-name
		      (concat denote-explore-network-filename ext)
		      denote-explore-network-directory)))
    (when display-fn
      (funcall display-fn graph-file))))

;;;###autoload
(defun denote-explore-network (&optional text-only)
  "Generate a network of Denote files or keywords by selecting a type.

- Community: Network of notes matching a regular expression.
  Links to notes not matching the regular expression are pruned.
- Neighbourhood: Network of notes from a root at a given depth.
  Depth = 1 notes linked to root; depth 2: notes linked to linked notes, etc.
- Sequence: Hierarchy of signatures, split at the = symbol.
- Keywords: Network of keywords.  Each note with two or more keywords
  forms a complete graph, which are merged into a weighted undirected graph.

Refer to the manual for a more detailed explanation.

Using the universal argument excludes attachments from the analysis (TEXT-ONLY).

The code generates a nested association list that holds all relevant metadata
for the selected graph:

- Meta data e.g.: `((directed . t) (type . \"Keywords\"))'
- Association list of nodes and their degrees, e.g.:
  `(((id . \"20210104T194405\") (name . \"Platonic Solids\") (keywords \"math\"'
  `\"geometry\") (type . \"org\") (degree . 4)) ...)'.
  In the context of Denote, the degree of a node is the unweighted sum of links
  and backlinks from and to a note.
- Association list of edges and their weights, e.g.:
  `(((source . \"20220529T190246\") (target . \"20201229T143000\")'
  `(weight . 1)) ...)'.
  The weight of an edge indicates the number of time it occurs in the graph.

This list is passed on to an encoding function to generate the desired graph
format.  In the last step, a visualisation function displays the graph in the
external web browser, except for the GEXF format.

The parameters for the generated graph are stored in
`denote-explore-network-previous', which is used to renegerate the same graph
after making changes to notes with `denote-explore-network-regenerate'.

The `denote-explore-graph-types' variable stores the functions required to
generate and regenerate graphs.

The `denote-explore-network-graph-formats' variable contains a list of functions
to encode and display each graph format."
  (interactive "P")
  (let* ((options (mapcar (lambda (type)
			    (format "%s (%s)" (car type)
				    (plist-get (cdr type) :description)))
			  denote-explore-graph-types))
	 (selection (completing-read "Network type?" options))
	 (graph-type (substring selection 0 (string-match " " selection)))
	 (config (assoc graph-type denote-explore-graph-types))
	 (generate-graph (plist-get (cdr config) :generate))
	 (graph (funcall generate-graph text-only)))
    (denote-explore--network-save graph)
    (denote-explore-network-view)))

(define-obsolete-function-alias
  'denote-explore-network-r
  'denote-explore-network "1.3")

;;;###autoload
(defun denote-explore-network-regenerate (&optional text-only)
  "Recreate the most recent Denote graph with external software.
Universal argument excludes attachments from the analysis (TEXT-ONLY)."
  (interactive "P")
  (if-let* ((graph-type (car denote-explore-network-previous))
	    (query (car (cdr denote-explore-network-previous)))
	    (config (assoc graph-type denote-explore-graph-types))
	    (regenerate-fn (plist-get (cdr config) :regenerate))
	    (graph (funcall regenerate-fn query text-only)))
      (progn (denote-explore--network-save graph)
	     (denote-explore-network-view))
    (message "No previous network defined")))

;;;###autoload
(defun denote-explore-list-keywords ()
  "List all Denote keywords with the number of notes that use each keyword.

This command:
- Scans `denote-directory` and collects per-note keywords.
- Builds a (KEYWORD . COUNT) table, where COUNT is the number of notes
  that include KEYWORD (one count per note, even if repeated in metadata).
- Sorts by COUNT descending, then KEYWORD ascending.
- When called interactively, displays a two-column table and appends
  a footer with the total number of distinct keywords and the sum of counts.

Return a plist for programmatic use:
  (:table ALIST :keywords N-DISTINCT :notes SUM-OF-COUNTS)."
  (interactive)
  (require 'denote)
  (declare-function denote-directory-files "denote")
  (declare-function denote-extract-keywords-from-path "denote")
  (declare-function denote-explore--table "denote-explore")

  (let* ((keywords (mapcan #'denote-extract-keywords-from-path
                           (denote-directory-files)))
         ;; Use the module helper to build (KEYWORD . COUNT)
         (raw (denote-explore--table keywords))
         ;; Sort by count (desc) and then by keyword (asc)
         (table (sort raw (lambda (a b)
                            (if (/= (cdr a) (cdr b))
                                (> (cdr a) (cdr b))
                              (string-lessp (car a) (car b))))))
         (n-distinct (length table))
         (sum-counts (apply #'+ 0 (mapcar #'cdr table))) ;; <-- FIJO AQUÍ
         (result `(:table ,table :keywords ,n-distinct :notes ,sum-counts)))
    (when (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*Denote Keywords*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%-40s %s\n" "Keyword" "Notes"))
          (insert (make-string 52 ?-) "\n")
          (dolist (kv table)
            (insert (format "%-40s %d\n" (car kv) (cdr kv))))
          (insert (make-string 52 ?-) "\n")
          (insert (format "Total keywords: %d | Sum of notes: %d\n"
                          n-distinct sum-counts))
          (goto-char (point-min))
          (view-mode 1))
        (pop-to-buffer (current-buffer))))
    result))

(provide 'denote-explore)
;;; denote-explore.el ends here
