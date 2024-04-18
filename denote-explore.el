;;; denote-explore.el --- Explore Denote files -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-explore/
;; Version: 1.4.3
;; Package-Requires: ((emacs "29.1") (denote "2.3.5") (dash "2.19.1"))

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
;; Denote-Explore provides functionality to explore, maintain and visualise
;; your collection fo Denote file.
;; 
;; Functionality:
;;
;; 1. Statistics: count and visualise notes and keywords
;; 2. Random walks: aces notes with serendipitous discovery
;; 3. Janitor: Maintenance on you Denote collection
;; 3. Network diagrams: visualise the structure of your notes

;; The Denote-Explore manual:
;; https://lucidmanager.org/productivity/denote-explore/

;;; Code:

(require 'denote)
(require 'dash)
(require 'chart)
(require 'cl-lib)
(require 'json)
(require 'browse-url)

;; Variables

(defgroup denote-explore ()
  "Explore and visualise Denote file collections."
  :prefix "denote-explore-"
  :group 'files
  :link '(url-link :tag "Homepage" "https://github.com/pprevos/denote-explore"))

(defcustom denote-explore-network-directory
  (expand-file-name "graphs/" denote-directory)
  "Directory to store Denote network files.
Created upon generating the network when it does not yet exist"
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type 'string)

(defcustom denote-explore-network-filename
  "denote-network"
  "Base filename sans extension for Denote explore network files.
Stored in `denote-explore-network-directory'.
File type defined with `denote-explore-network-format'."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type 'string)

(define-obsolete-variable-alias
  'denote-explore-json-vertices-filename
  'denote-explore-network-filename "1.3")

(define-obsolete-variable-alias
  'denote-explore-json-edges-filename
  'denote-explore-network-filename "1.3")

(defcustom denote-explore-network-format
  'graphviz
  "Output format for Denote network files."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type '(choice
	  (const :tag "GraphViz (Dot)" graphviz)
	  (const :tag "D3 JavaScript (JSON)" d3.js)
	  (const :tag "Graph Exchange XML Format (GEXF)" gexf)))

(defcustom denote-explore-network-keywords-ignore '("bib")
  "List of keywords to be ignored in the keywords graph."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type 'list)

(defcustom denote-explore-network-regex-ignore '()
  "Regular expression for notes ignored in neighbourhood and community graphs."
  :group 'denote-explore
  :package-version '(denote-explore . "1.4")
  :type 'list)

(defcustom denote-explore-network-graphviz-header
  '("layout=neato"
    "size=20"
    "ratio=compress"
    "overlap=scale"
    "sep=1"
    "node[label=\"\" style=filled color=lightskyblue fillcolor=lightskyblue3
shape=circle fontsize=80 fontcolor=gray10 fontname = \"Helvetica, Arial, sans-serif\"]"
    "edge[arrowsize=3 color=gray30]")
  "List of strings for the header of a GraphViz DOT file.

Defines graph and layout properties and default edge and node attributes.
See https://graphviz.org for documentation.

Properties for specific edges and nodes, as defined by the
`denote-explore-network-encode-graphviz' function, override these settings."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type '(repeat string))

(defcustom denote-explore-network-graphviz-filetype
  "svg"
  "Output file type for Denote GraphViz network files.

Use SVG or for interactivity (tootltips and hyperlinks).
See https://graphviz.org for documentation."
  :group 'denote-explore
  :package-version '(denote-explore . "1.4")
  :type '(choice
	  (const :tag "Scalable Vector Graphics (SVG)" "svg")
	  (const :tag "Portable Document Format (PDF)" "pdf")
	  (const :tag "Portable Network Graphics (PNG)" "png")
	  (string :tag "Other option")))

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
     :generate denote-explore-network-community
     :regenerate denote-explore-network-community-graph)
    ("Neighbourhood"
     :generate denote-explore-network-neighbourhood
     :regenerate denote-explore-network-neighbourhood-graph)
    ("Keywords"
     :generate denote-explore-network-keywords
     :regenerate denote-explore-network-keywords-graph))
  "List of network types and their (re)generation functions.")

(defvar denote-explore-load-directory
  (file-name-directory load-file-name)
  "Path of the denote-explore package required to start non-elisp scripts.")

(defvar denote-explore-network-previous
  nil
  "Store the previous network configuration to regenerate the last graph.
Parameters define the previous network, e.g.:
- `(keywords)'
- `(neighbourhood \"20240101T084408\" 3)'
- `(community \"_regex\")'")

;;; STATISTICS
;; Count number of notes, attachments and keywords

;;;###autoload
(defun denote-explore-count-notes ()
  "Count number of Denote text files and attachments.
A note is defined by `denote-file-types', anything else is an attachment."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
	 (denote-files (length (denote-directory-files nil nil t)))
	 (attachments (- all-files denote-files)))
    (message "%s notes (%s attachments)" denote-files attachments)))

;;;###autoload
(defun denote-explore-count-keywords ()
  "Count distinct Denote keywords."
  (interactive)
  (message "%s distinct keywords" (length (denote-keywords))))

;;; RANDOM WALKS
;; Jump to a random note, random linked note or random note with selected tag(s).
;; With universal argument the sample includes attachments.

(defun denote-explore--jump (denote-sample)
  "Jump to a random note in the DENOTE-SAMPLE file list.
Used in `denote-explore-random-*' functions."
  (let ((sample denote-sample))
    (find-file (nth (random (length sample)) sample))))

;;;###autoload
(defun denote-explore-random-note (&optional include-attachments)
  "Jump to a random Denote file and optionally INCLUDE-ATTACHMENTS.
With universal argument, the sample includes attachments."
  (interactive "P")
  (if-let ((denotes (denote-directory-files nil t (not include-attachments))))
      (denote-explore--jump denotes)
    (user-error "No Denote files found")))

;;;###autoload
(defun denote-explore-random-link ()
  "Jump to a random linked note (forward or backward).
With universal argument the sample includes links to attachments."
  (interactive)
  (let* ((forward-links (denote-link-return-links))
	 (back-links (denote-link-return-backlinks))
	 (all-links (append forward-links back-links))
	 (links (if current-prefix-arg
		    all-links
		  (seq-filter #'denote-file-is-note-p all-links))))
    (if links (denote-explore--jump links)
      (user-error "Not a Denote file or no (back)links in or to this buffer"))))

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
  "Jump to a random note with selected keyword(s), optionally INCLUDE-ATTACHMENTS.

- Manually select one or more keywords from the active Denote buffer.
- Can override the completion option by adding free text
- Use \"*\" to select all listed keywords.

Selecting multiple keywords requires `denote-sort-keywords' to be non-nil
or target keywords are in the same order as the selection. Alternatively, use
`denote-explore-sort-keywords' to order keywords in all Denote files.

With universal argument the sample includes attachments."
  (interactive "P")
  (if-let* ((keyword-list (denote-explore--select-keywords))
	    (keyword-regex (concat "_" (mapconcat #'identity keyword-list ".*_")))
	    (sample (denote-directory-files
		     keyword-regex t (not include-attachments))))
      (denote-explore--jump sample)
    (message "No matching Denote files found")))

;;; JANITOR
;; The Janitor provides various functions to maintain a Denote file collection.

(defun denote-explore--table (list)
  "Generate an ordered frequency table from a LIST."
  (sort (-frequencies list)
        (lambda (a b) (> (cdr a) (cdr b)))))

;;;###autoload
(defun denote-explore-identify-duplicate-notes (&optional filenames)
  "Identify duplicate Denote IDs or FILENAMES.

If FILENAMES is nil, check Denote IDs, otherwise use complete file names.
Using the FILENAMES option (or using the universal argument) excludes
exported Denote files from duplicate-detection."
  ;; TODO: Instead of comparing files, remove all files with non-denote extensions
  (interactive "P")
  (let* ((denote-files (denote-directory-files))
         (candidates (if filenames
                         (mapcar (lambda (path) (file-name-nondirectory path)) denote-files)
                       (mapcar #'denote-retrieve-filename-identifier denote-files)))
         (tally (denote-explore--table candidates))
         (duplicates (mapcar #'car (cl-remove-if-not
                                    (lambda (note)
				      (> (cdr note) 1)) tally))))
    (if duplicates (message "Duplicates: %s" (mapconcat 'identity duplicates ", "))
      (message "No duplicates found"))))

(define-obsolete-function-alias
  'denote-explore-identify-duplicate-identifiers
  'denote-explore-identify-duplicate-notes
  "1.2")

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
	 (zero-keywords (seq-remove (lambda (note)
				      (member note keywords))
				    (denote-directory-files))))
    (find-file (completing-read "Select file with zero keywords: " zero-keywords))))

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
	 		    (denote-retrieve-filename-title file)
	 		    (denote-keywords-sort
	 		     (if (equal (car keywords) "") nil keywords))
	 		    (denote-retrieve-filename-signature file)))))
  (message "All keywords ordered alphabetically"))

;;;###autoload
(defun denote-explore-rename-keyword ()
  "Rename or remove keyword(s) across the whole Denote collection.
When selecting more then one existing keyword, all selections are renamed.
Use empty string as new keyword to remove the selection."
  (interactive)
  (save-some-buffers)
  (let* ((denote-rename-no-confirm nil)
	 (denote-sort-keywords t)
	 (selected (denote-keywords-prompt "Keyword to rename"))
	 (new-keyword (read-from-minibuffer "New keyword: "))
	 (_keywords (mapcar (lambda (keyword) (concat "_" keyword)) selected))
	 (notes (denote-directory-files
		 (mapconcat #'identity (sort _keywords 'string<) "\\|"))))
    (dolist (file notes)
      (let* ((current-keywords (denote-explore--retrieve-keywords file))
	     (new-keywords (if (equal new-keyword "")
			       (cl-set-difference current-keywords selected :test 'string=)
			     (mapcar (lambda (keyword)
				       (if (member keyword selected) new-keyword keyword))
				     current-keywords))))
	(denote-rename-file file
			    (denote-retrieve-title-or-filename
			     file (denote-filetype-heuristics file))
			    (if (equal new-keywords nil) "" new-keywords)
			    (denote-retrieve-filename-signature file))))))

(define-obsolete-function-alias
  'denote-explore--retrieve-title
  'denote-retrieve-title-or-filename
  "1.4.2")

;;;###autoload
(defun denote-explore-sync-metadata ()
  "Synchronise the filenames with the metadata for all Denote files.

Set `denote-rename-buffer-mode' to ensure synchronised notes."  ;; TODO Elaborate
  (interactive)
  (save-some-buffers)
  (let ((denote-rename-no-confirm nil)
	(denote-sort-keywords t)
	(notes (denote-directory-files nil nil t)))
    (dolist (file notes)
      (message file)
      (denote-rename-file-using-front-matter file)))
  (message "Integrity check completed"))

;;; VISUALISATION

;; Bar charts
;; Leverages the built-in chart package for plain text visualisation.

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
  "Visualise the Denote file and attachment types."
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
(defun denote-explore-degree-barchart ()
  "Visualise the number of degrees for each node in the Denote network."
  (interactive)
  (message "Analysing Denote network.")
  (let* ((graph (denote-explore-network-community-graph ""))
	 (nodes (cdr (assoc 'nodes graph)))
	 (degrees (denote-explore--network-sum-degrees nodes))
	 (txt-degrees (mapcar (lambda (pair)
				(cons (number-to-string (car pair)) (cdr pair)))
			      degrees)))
    (denote-explore--barchart txt-degrees "Degree" "Node degree distribution")))

;;;###autoload
(defun denote-explore-isolated-notes (&optional include-attachments)
  "Identify Denote files without (back)links and optionally INCLUDE-ATTACHMENTS.
Using the universal argument includes attachments."
  (interactive "P")
  (let* ((files (denote-directory-files nil nil (not include-attachments)))
	 (all-ids (mapcar #'denote-retrieve-filename-identifier files))
	 (edges (denote-explore--network-extract-edges files))
	 (linked-ids (denote-explore--network-extract-unique-nodes edges))
	 (isolated-ids (seq-remove (lambda (id) (member id linked-ids)) all-ids))
	 (regex (mapconcat (lambda (item) (concat "\\b" item "\\b"))
			   isolated-ids "\\|"))
	 (isolated-files (seq-filter (lambda (item) (string-match regex item)) files)))
    (find-file (completing-read "Select isolated file: " isolated-files))))

;;; DEFINE GRAPHS
;; Define various graph types as an association list

;; Community graph

(defun denote-explore--network-zip-alists (source target)
  "Combine two lists into an alists, pairing SOURCE and TARGET, defining edges."
  (let ((result nil))
    (while (and source target)
      (let ((pair (list (cons 'source (car source))
			(cons 'target (car target)))))
        (push pair result)
        (setq source (cdr source))
        (setq target (cdr target))))
    (nreverse result)))

(defun denote-explore--network-extract-edges (files)
  "Extract Denote network links as network edges from FILES."
  (when-let* ((links-xref (xref-matches-in-files
			   "\\[denote:[0-9]\\{8\\}" files))
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
			   (match-string 0 str))) links)))
    (denote-explore--network-zip-alists source target)))

(define-obsolete-function-alias
  'denote-explore--extract-vertices
  'denote-explore--network-extract-node
  "1.2")

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
  "Count occurrences of EDGES to set their weight."
  (let (result)
    (dolist (edge edges)
      (let* ((source (cdr (assoc 'source edge)))
             (target (cdr (assoc 'target edge)))
             (key (format "%s-%s" source target))
             (found (assoc key result)))
        (if found
            (setcdr found (1+ (cdr found)))
          (push (cons key 1) result))))
    ;; Transform the list back into the desired format
    (let (final-result)
      (dolist (item result final-result)
        (let* ((key (car item))
               (weight (cdr item))
               (parts (split-string key "-"))
               (source (nth 0 parts))
               (target (nth 1 parts)))
          (push (list (cons 'source source)
                      (cons 'target target)
                      (cons 'weight weight))
		final-result))))))

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
	      (append node (list (cons 'degree degree))))) nodes))

(defun denote-explore--network-filter-files (files)
  "Remove files matching `denote-explore-network-regex-ignore' from Denote FILES.
Removes selected files from neighbourhood or community visualisation."
  (let ((ignore (if denote-explore-network-regex-ignore
		    (denote-directory-files denote-explore-network-regex-ignore)
		  nil)))
    (cl-set-difference files ignore :test 'string=)))

(defun denote-explore-network-community-graph (regex)
  "Generate network community association list for note matching REGEX.
Links to notes outside the search area are pruned."
  (if-let* ((files (denote-explore--network-filter-files
		    (denote-directory-files regex)))
	    (ids (mapcar #'denote-extract-id-from-string files))
	    (edges (denote-explore--network-extract-edges files))
	    (edges-pruned (denote-explore--network-prune-edges ids edges))
	    (edges-alist (denote-explore--network-count-edges edges-pruned))
	    (nodes (mapcar #'denote-explore--network-extract-node files))
	    (nodes-alist (denote-explore--network-degree nodes edges-alist))
	    (meta-alist `((directed . t) (type . ,(format "Community '%s'" regex)))))
      `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))
    (user-error "No Denote files or (back)links found for %s" regex)))

(defun denote-explore-network-community ()
  "Define inputs for a network community and generate graph."
  (let ((regex (read-from-minibuffer
		"Enter search term / regular expression (empty string for all notes):")))
    (setq denote-explore-network-previous `("Community" ,regex))
    (message "Building graph for %s community " regex)
    (denote-explore-network-community-graph regex)))

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

(defun denote-explore-network-keywords ()
  "Enter a positive integer for a minimum weight and generate a keywords graph."
  (interactive)
  (let ((min-weight (read-number "Enter min-weight (integer > 0): " 1)))
    (while (<= min-weight 0)
      (setq min-weight (read-number "Invalid input. Enter an integer > 0: " 1)))
    (setq denote-explore-network-previous `("Keywords" ,min-weight))
    (denote-explore-network-keywords-graph min-weight)))

(defun denote-explore--network-keywords-flatten-edges (edges)
  "Flatten list of network EDGES."
  (let (edges-alist '())
    (dolist (sublist edges)
      (dolist (edge sublist)
	(push edge edges-alist)))
    edges-alist))

(defun denote-explore-network-keywords-graph (min-weight)
  "Generate Denote graph object from keywords with MIN-WEIGHT edges."
  (let* ((files (denote-directory-files))
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
	 (meta-alist `((directed . nil) (type . "Keywords") (min-weight . ,min-weight))))
    `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))))

;;; Neighbourhood Graph
(defun denote-explore--network-extract-unique-nodes (edges)
  "Extract all unique `source' and `target' nodes from EDGES."
  (let ((nodes '()))
    (dolist (edge edges)
      (dolist (key '(source target))
        (let ((node (cdr (assoc key edge))))
          (unless (member node nodes)
            (push node nodes)))))
    (nreverse nodes)))

(defun denote-explore--network-find-edges (id edges)
  "Find edges matching ID as source or target node among alist of EDGES."
  (seq-filter (lambda (edge)
		(or (string= (cdr (assoc 'source edge)) id)
		    (string= (cdr (assoc 'target edge)) id))) edges))

(defun denote-explore-network--unique-edges (edges-a edges-b)
  "Return a list of unique edges from EDGES-A and EDGES-B."
  (let ((edges (append edges-a edges-b))
        (seen '())
        (unique '()))
    (dolist (edge edges)
      (unless (member edge seen)
        (push edge seen)
        (push edge unique)))
    (nreverse unique)))

(defun denote-explore--network-neighbourhood-edges (id depth search-space)
  "Search for all edges originating from ID to DEPTH within SEARCH-SPACE."
  (let ((current-ids (list id))
	(edges '())
	(new-edges '()))
    ;; Search depth steps deep
    (dotimes (_ depth edges)
      (setq new-edges (apply #'append
			     (mapcar (lambda (id)
				       (denote-explore--network-find-edges id search-space))
				     current-ids)))
      (setq edges (denote-explore-network--unique-edges edges new-edges))
      (setq current-ids (seq-filter
			 (lambda (element)
			   (not (member element current-ids)))
			 (denote-explore--network-extract-unique-nodes edges))))))

(defun denote-explore-network-neighbourhood ()
  "Obtain inputs to define neighbourhood graph.
Uses the ID of the current Denote buffer or user selects via completion menu."
  (let*  ((buffer (or (buffer-file-name) ""))
	  (file (if (denote-file-is-note-p buffer)
		    buffer
		  (completing-read "Select Denote source file:"
				   (denote-directory-files nil t t))))
	  (depth (string-to-number (read-from-minibuffer "Search depth: ")))
	  (id (denote-retrieve-filename-identifier file)))
    (setq denote-explore-network-previous `("Neighbourhood" (,id ,depth)))
    (denote-explore-network-neighbourhood-graph `(,id ,depth))))


(defun denote-explore-network-neighbourhood-graph (id-depth)
  "Generate Denote graph object from the neighbourhood with ID-DEPTH.
ID-DEPTH is a list containing the starting ID and the DEPTH of the links."
  (if-let* ((id (car id-depth))
	    (depth (nth 1 id-depth))
	    (text-files (denote-explore--network-filter-files
			 (denote-directory-files nil nil t)))
	    (denote-edges (denote-explore--network-extract-edges text-files))
	    (edges (denote-explore--network-neighbourhood-edges id depth denote-edges))
	    (edges-alist (denote-explore--network-count-edges edges))
	    (ids (denote-explore--network-extract-unique-nodes edges-alist))
	    (denote-files (denote-directory-files))
	    (files (seq-filter
		    (lambda (file)
		      (cl-some (lambda (regex) (string-match regex file)) ids))
		    denote-files))
	    (nodes (mapcar #'denote-explore--network-extract-node files))
	    (nodes-alist (denote-explore--network-degree nodes edges-alist))
	    (meta-alist `((directed . t) (type . ,(format "Neighbourhood '%s' (depth: %s)" id depth)))))
      `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))
    (user-error "No network neighbourhood found")))

;;; SAVE GRAPH

(defun denote-explore-network-encode-json (graph)
  "Encode a Denote GRAPH object to JSON and insert in a file."
  (insert (json-encode graph))
  (json-pretty-print-buffer))

(defun denote-explore-network-encode-graphviz (graph)
  "Encode a Denote GRAPH object to GraphViz and insert in a file."
  (let* ((meta (cdr (assoc 'meta graph)))
	 (nodes (cdr (assoc 'nodes graph)))
         (edges (cdr (assoc 'edges graph)))
	 (directed (cdr (assoc 'directed meta)))
	 (graphtype (list (if directed "digraph Denote {" "graph Denote {")))
         (dot-header (append graphtype denote-explore-network-graphviz-header))
	 (dot-content '())
	 (type (cdr (assoc 'type meta)))
	 (nb-core (when (string-match denote-id-regexp type) (match-string 0 type))))
    ;; Nodes
    (dolist (node nodes)
      (let* ((id (cdr (assoc 'id node)))
	     (core (if (equal nb-core id) "fillcolor=darkorchid" ""))
             (name (cdr (assoc 'name node)))
	     (degree (cdr (assoc 'degree node)))
	     (label (if (or (> degree 2) (equal nb-core id)) name ""))
             (tags (mapconcat 'identity (cdr (assoc 'keywords node)) ", "))
             (type (cdr (assoc 'type node)))
	     (width (sqrt (+ degree 1)))
	     (file-name (cdr (assoc 'filename node)))
	     (hyperlink (if file-name file-name "#")))
	(push (format (concat "%S [xlabel=%S tooltip=\"ID: %s\\nTitle: "
			      "%s\\nKeywords: %s\\nType: %s\\nDegree: %s\" "
			      "width=%s %s URL=\"%s\"]\n")
                      id label id name tags type degree width core hyperlink)
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
        (push (format "<node id=%S label=%S>\n<attvalues>\n<attvalue for=\"degree\" value=%S/>\n</attvalues>\n</node>"
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
	 (file-name (concat denote-explore-network-directory
			    denote-explore-network-filename file-extension)))
    (when (not (file-exists-p denote-explore-network-directory))
      (make-directory denote-explore-network-directory))
    (with-temp-file file-name (funcall convert-fn graph))
    (message "Graph data saved to %s" file-name)))

;;; Visualise network

(defun denote-explore-network-display-graphviz (gv-file)
  "Convert GraphViz GV-FILE to an SVG file and display in external application.
Output is saved to the `denote-explore-network-directory' in the
`denote-explore-network-graphviz-filetype' file format."
  (let* ((file-type denote-explore-network-graphviz-filetype)
	 (out-file (concat (file-name-sans-extension gv-file) "." file-type))
	 (script-call (format "dot %s -T%s -o %s"
			      (shell-quote-argument gv-file)
			      file-type
			      (shell-quote-argument out-file))))
    (message script-call)
    (delete-file out-file)
    (setq exit-status (shell-command script-call))
    (if (eq exit-status 0)
	(if (file-exists-p out-file)
	    (browse-url-default-browser out-file nil)
	  (user-error "No output file produced"))
      (user-error "GraphViz image generation unsuccessful"))))

(defun denote-explore-network-display-json (json-file)
  "Convert GraphViz JSON-FILE to an HTML file and display in external browser.
Output is saved to `denote-explore-network-directory'.
This functionality currently requires a working version of the R language."
  ;; TODO: Develop D3.js template to negate the need for R.
  (let* ((html-file (concat (file-name-sans-extension json-file) ".html"))
	 (script-call (format "Rscript %sdenote-explore-network.R %s"
			      (shell-quote-argument denote-explore-load-directory)
			      (shell-quote-argument json-file))))
    (message script-call)
    (delete-file html-file)
    (setq exit-status (shell-command script-call))
    (if (eq exit-status 0)
	(if (file-exists-p html-file)
	    (browse-url-default-browser html-file nil)
	  (user-error "No output file found"))
      (user-error "Graphic generation unsuccessful"))))

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
(defun denote-explore-network ()
  "Generate a network of Denote files or keywords by selecting a type.

- Community: Network of notes matching a regular expression.
  Links to notes not matching the regular expression are pruned.
- Neighbourhood: Generate a network of notes from a parent at a given depth.
  Depth = 1 shows linked notes; depth 2 all notes linked to linked notes etc.
- Keywords: Generate network of keywords.  Each note with two or more keywords
  forms a complete graph, which are merged into a weighted undirected graph.

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
external web browser.

The `denote-explore-graph-types' variable stores the functions required to
generate and regenerate graphs.

The `denote-explore-network-graph-formats' variable contains a list of functions
to encode and display each graph format."
  (interactive)
  (let* ((options (mapcar #'car denote-explore-graph-types))
	 (graph-type (completing-read "Network type?" options))
	 (config (assoc graph-type denote-explore-graph-types))
	 (generate-fn (plist-get (cdr config) :generate))
	 (graph (funcall generate-fn)))
    (denote-explore--network-save graph)
    (denote-explore-network-view)))

(define-obsolete-function-alias
  'denote-explore-network-r
  'denote-explore-network "1.3")

(defun denote-explore-network-regenerate ()
  "Recreate the most recent Denote graph with external software."
  (interactive)
  (if-let* ((graph-type (car denote-explore-network-previous))
	    (query (car (cdr denote-explore-network-previous)))
	    (config (assoc graph-type denote-explore-graph-types))
	    (regenerate-fn (plist-get (cdr config) :regenerate))
	    (graph (funcall regenerate-fn query)))
      (progn (denote-explore--network-save graph)
	     (denote-explore-network-view))
    (message "No previous network defined")))

(provide 'denote-explore)
;;; denote-explore.el ends here
