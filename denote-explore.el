;;; denote-explore.el --- Explore Denote files -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-extra/
;; Version: 1.4
;; Package-Requires: ((emacs "29.1") (denote "2.2.4") (dash "2.19.1"))

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
(require 'dash)
(require 'chart)
(require 'cl-lib)
(require 'json)

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

(defcustom denote-explore-network-graphviz-header
  '("layout=neato"
    "size=20"
    "ratio=compress"
    "overlap=scale"
    "sep=1"
    "node[label=\"\" style=filled color=lightskyblue fillcolor=lightskyblue3
shape=circle fontsize=80 fontcolor=gray10 fontname = \"Helvetica, Arial,sans-serif\"]"
    "edge[arrowsize=3 color=gray10]")
  "List of strings for the header of a GraphViz DOT file.
Defines graph and layout properties and default edge and node attributes.
Properties for specific edges and nodes, as defined in the
`denote-explore--network-encode-graphviz' function override these settings."
  :group 'denote-explore
  :package-version '(denote-explore . "1.3")
  :type '(repeat string))

(defcustom denote-explore-network-graphviz-filetype
  "svg"
  "Output file type for Denote network files.
Use SVG or PDF for best results.
Read GraphViz documentation for other output formats."
  :group 'denote-explore
  :package-version '(denote-explore . "1.4")
  :type '(choice
	  (const :tag "Scalable Vector Graphics (SVG)" "svg")
	  (const :tag "Portable Document Format (PDF)" "pdf")))

(define-obsolete-variable-alias
  'denote-explore-json-vertices-filename
  'denote-explore-network-filename "1.3")

(define-obsolete-variable-alias
  'denote-explore-json-edges-filename
  'denote-explore-network-filename "1.3")

(define-obsolete-variable-alias
  'denote-explore--extract-vertices
  'denote-explore--network-extract-node "1.3")

(define-obsolete-variable-alias
  'denote-explore-network-r
  'denote-explore-network "1.3")

(defvar denote-explore-network-graph-formats
  `((graphviz
     :file-extension ".gv"
     :encode denote-explore--network-encode-graphviz
     :display denote-explore--network-display-graphviz)
    (d3.js
     :file-extension ".json"
     :encode denote-explore--network-encode-json
     :display denote-explore--network-display-json)
    (gexf
     :file-extension ".gexf"
     :encode denote-explore--network-encode-gexf
     :display nil))
  "A-list of variables related to the network file formats.

Each element is of the form (SYMBOL . PROPERTY-LIST).  SYMBOL is
one of those specified in `denote-explore-network-format'.

PROPERTY-LIST is a plist that consists of two elements:

- `:file-extension' File extension to save network.
- `:encode' function to encode network to graph type.
- `:display' function to display the graph in external software.")

(defvar denote-explore-graph-types
  `(("Community"
     :generate denote-explore--network-community)
    ("Neighbourhood"
     :generate denote-explore--network-neighbourhood)
    ("Keywords"
     :generate denote-explore--network-keywords))
  "List of possible network types and their generation functions.")

(defvar denote-explore-load-directory
  (file-name-directory load-file-name)
  "Path of the denote-explore package required to start R script.")

(defvar denote-explore-network-previous
  '()
  "Store the previous network configuration for fast reproduction.
Parameters to define the previous network, e.g.:
- '(keywords)
- '(neighbourhood \"20240101t084408\" 3)
- '(community \"_regex\")")

;;; STATISTICS
;; Count number of notes, attachments and keywords

;;;###autoload
(defun denote-explore-count-notes ()
  "Count number of Denote text files and attachments.
A text file is defined by `denote-file-types', anything else is an attachment."
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

;;; RANDOM WALKS
;; Jump to a random note, random linked note or random note with selected tag(s).
;; With universal argument the sample includes links to attachments.

(defun denote-explore--jump (denote-sample)
  "Jump to a random note in the DENOTE-SAMPLE file list.
Used in `denote-explore-random-*' functions."
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
  "Retrieve alphabetised list of keywords from Denote FILE or attachment.
Uses front matter for notes and the filename for attachments."
  (let* ((filetype (denote-filetype-heuristics file))
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
- If no keyword is defined in the current buffer, then choose from all
  available keywords."
  (let* ((file (buffer-file-name))
	 (raw-keywords (denote-explore--retrieve-keywords file))
	 (buffer-keywords (if (null raw-keywords)
			      (denote-keywords)
			    raw-keywords))
	 (keywords (if (> (length buffer-keywords) 1)
		       (delete-dups (sort (completing-read-multiple
					   "Select keyword(s) (* selects all available keywords): " buffer-keywords)
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
      (if-let* ((sample (if current-prefix-arg
			    (denote-directory-files keyword-regex t nil)
			  (denote-directory-files keyword-regex t t))))
	  (denote-explore--jump sample)
	(user-error "No matching Denote file found"))
    (user-error "No keywords found")))

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
  (interactive "P")
  (let* ((denote-files (denote-directory-files))
         (candidates (if filenames
                         (mapcar (lambda (path) (file-name-nondirectory path)) denote-files)
                       (mapcar #'denote-retrieve-filename-identifier denote-files)))
         (tally (denote-explore--table candidates))
         (duplicates (mapcar #'car (cl-remove-if-not
                                    (lambda (note)
				      (> (cdr note) 1)) tally))))
    (if duplicates
        (message "Duplicates: %s"
		 (mapconcat 'identity duplicates ", "))
      (message "No duplicates found"))))

(make-obsolete 'denote-explore-identify-duplicate-identifiers
	       'denote-explore-identify-duplicate-notes "Version 1.2")

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
  "Select a note or attachment without keywords."
  (interactive)
  (let* ((with-keyword-regex "--\\([[:alnum:][:nonascii:]-]*_\\)")
	 (keywords (denote-directory-files with-keyword-regex))
	 (zero-keywords (seq-remove (lambda (note)
				      (member note keywords))
				    (denote-directory-files))))
    (find-file (completing-read "Select file with zero keywords: " zero-keywords))))

(defun denote-explore--retrieve-title (file)
  "Retrieve the title from a Denote FILE or an attachment."
  (when (file-exists-p file)
    (if (denote-file-is-note-p file)
	(denote-retrieve-title-value file (denote-filetype-heuristics file))
      (denote-retrieve-filename-title file))))

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
  "Rename or remove a keyword across the whole Denote collection.
When selecting more then one existing keyword, all will be renamed.
Use empty string as new keyword to remove the selection."
  (interactive)
  (save-some-buffers)
  (let* ((denote-rename-no-confirm nil)
	 (denote-sort-keywords t)
	 (selected (denote-keywords-prompt "Keyword to rename"))
	 (keywords (mapcar (lambda (keyword) (concat "_" keyword)) selected))
	 (new-keyword (read-from-minibuffer "New keyword: "))
	 (notes (denote-directory-files
		 (mapconcat #'identity (sort keywords 'string<) "\\|"))))
    (dolist (file notes)
      (let* ((current-keywords (denote-explore--retrieve-keywords file))
	     (keywords (if (equal new-keyword "")
			   (remove old-keyword current-keywords)
			 (mapcar (lambda (keyword)
				   (if (member keyword selected) new-keyword keyword))
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

;; DEFINE GRAPHS

;;; Community graph

(defun denote-explore--network-zip-alists (source target)
  "Zip two lists into a list of alists, pairing SOURCE and TARGET."
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

(defun denote-explore--network-extract-node (file)
  "Extract metadata for note or attachment with FILE."
  (when (file-exists-p file)
    (let ((id (denote-retrieve-filename-identifier file))
	  (signature (denote-retrieve-filename-signature file))
	  (name (denote-explore--retrieve-title file))
	  (keywords (denote-retrieve-filename-keywords file))
	  (type (file-name-extension file)))
      (setq keywords (if keywords (string-split keywords "_") ""))
      `((id . ,id) (signature . ,signature)(name . ,name)
	(keywords . ,keywords) (type . ,type) (filename . ,file)))))

(defun denote-explore--network-prune-edges (nodes edges)
  "Select EDGES (links) where both source and target are part of NODES.
Prunes any edges that link to outside the community."
  (let ((filtered-edges '()))
    (dolist (edge edges filtered-edges)
      (let ((source (cdr (assoc 'source edge)))
            (target (cdr (assoc 'target edge))))
        (when (and (member source nodes) (member target nodes))
          (push edge filtered-edges))))
    (nreverse filtered-edges)))

(defun denote-explore--network-count-edges (edges)
  "Count occurrences of EDGES to set weight."
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

(defun denote-explore--network-community-graph (regex)
  "Generate network community association list for note matching REGEX.
Links to notes outside the search area are pruned."
  (if-let* ((files (denote-directory-files regex))
	    (ids (mapcar #'denote-extract-id-from-string files))
	    (edges (denote-explore--network-extract-edges files))
	    (edges-pruned (denote-explore--network-prune-edges ids edges))
	    (edges-alist (denote-explore--network-count-edges edges-pruned))
	    (nodes (mapcar #'denote-explore--network-extract-node files))
	    (nodes-alist (denote-explore--network-degree nodes edges-alist))
	    (meta-alist `((directed . t) (type . ,(format "Community '%s'" regex)))))
      `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))
    (user-error "No Denote files or (back)links found for %s" regex)))

(defun denote-explore--network-community ()
  "Define inputs for a network community and generate graph."
  (let ((regex (read-from-minibuffer
		"Enter search term / regex (empty string for all notes):")))
    (setq denote-explore-network-previous `("community" ,regex))
    (denote-explore--network-community-graph regex)))

;;; keywords graph

(defun denote-explore--network-keywords-extract (files)
  "Convert keywords from FILES to a list.
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
  (let ((network '()) ; Empty list to hold our network
	(length (length keywords)))
    (dotimes (i length)
      (dotimes (j length)
        (unless (or (= i j) (> i j)) ; Avoid duplicate and reversed pairs
          (let ((source (nth i keywords))
                (target (nth j keywords)))
            (push (list (cons 'source source)
			(cons 'target target)) network)))))
    (car network)))

(defun denote-explore--network-extract-unique-nodes (edges)
  "Extract all unique 'source' and 'target' nodes from EDGES."
  (let ((nodes '()))
    (dolist (edge edges)
      (dolist (key '(source target))
        (let ((node (cdr (assoc key edge))))
          (unless (member node nodes)
            (push node nodes)))))
    (nreverse nodes)))

(defun denote-explore--network-keywords ()
  "Generate Denote graph object from keywords."
  (let* ((files (denote-directory-files))
	 (keywords (denote-explore--network-keywords-extract files))
	 (edges (mapcar #'denote-explore--network-keyword-edges keywords))
	 (edges-alist (denote-explore--network-count-edges edges))
	 ;; Remove nodes with degree < 1 (denote-explore-single-keywords)
	 (all-keywords (denote-keywords))
	 (select-keywords (seq-remove
			   (lambda (node)
			     (member node denote-explore-network-keywords-ignore))
			   all-keywords))
	 (nodes (mapcar
		 (lambda (keyword)
		   (list (cons 'id keyword)
			 (cons 'name (capitalize keyword)))) select-keywords))
	 (nodes-alist (denote-explore--network-degree nodes edges-alist))
	 (meta-alist `((directed . nil) (type . "Keywords"))))
    `((meta . ,meta-alist) (nodes . ,nodes-alist) (edges . ,edges-alist))))

;;; Neighbourhood Graph

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
	(edges '()))
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

(defun denote-explore--network-neighbourhood ()
  "Generate Denote graph object from the neighbourhood of ID at DEPTH links.
Uses the ID of the current Denote buffer or user selects via completion menu."
  ;; TODO: use https://github.com/alphapapa/org-graph-view/tree/master
  (if-let* ((buffer (or (buffer-file-name) ""))
	    (file-name (if (denote-file-is-note-p buffer)
			   buffer
			 (completing-read "Select Denote source file:"
					  (denote-directory-files nil t t))))
	    (depth (string-to-number (read-from-minibuffer "Search depth: ")))
	    (id (denote-retrieve-filename-identifier file-name))
	    (denote-text-files (denote-directory-files nil nil t))
	    (denote-edges (denote-explore--network-extract-edges denote-text-files))
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

(defun denote-explore--network-encode-json (graph)
  "Encode a Denote GRAPH object to JSON and insert in a file."
  (insert (json-encode graph))
  (json-pretty-print-buffer))

(defun denote-explore--network-encode-graphviz (graph)
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
	     (file-name (cdr (assoc 'filename node))))
	(push (format (concat "%S [xlabel=%S tooltip=\"ID: %s\\nTitle: "
			      "%s\\nKeywords: %s\\nType: %s\\nDegree: %s\" "
			      "width=%s %s URL=\"%s\"]\n")
                      id label id name tags type degree width core file-name)
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

(defun denote-explore--network-encode-gexf (graph)
  "Encode a Denote GRAPH object to GEXF and insert in a file."
  (let* ((nodes (cdr (assoc 'nodes graph)))
         (edges (cdr (assoc 'edges graph)))
	 (type (if (cdr (assoc 'directed graph)) "directed" "undirected"))
	 (lastmod (format-time-string "%F"))
	 (gexf-header `("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
			"<gexf xmlns=\"http://gexf.net/1.3\" "
			"  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
			"  xsi:schemaLocation=\"http://gexf.net/1.3 "
			"  http://gexf.net/1.3/gexf.xsd\" version=\"1.3\">\n"
			"<meta lastmodifieddate=\"" ,lastmod "\">\n"
			"<creator>Emacs Denote-Explore package</creator>\n"
			"<description>A network of Denote files</description>\n"
			"</meta>\n"
			"<graph defaultedgetype=\"" ,type "\">\n"
			"<nodes>\n"))
	 (gexf-lines '()))
    (dolist (node nodes)
      (let* ((id (cdr (assoc 'id node)))
             (name (cdr (assoc 'name node)))
	     (label (denote-explore--network-escape-xml name)))
        (push (format "<node id=%S label=%S />\n" id label) gexf-lines)))
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

(defun denote-explore--network-display-graphviz (gv-file)
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
      (user-error "Graphic generation unsuccessful"))))

(defun denote-explore--network-display-json (json-file)
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
	    ;; TODO: Open in the same browser tab
	    (browse-url-default-browser html-file nil)
	  (user-error "No output file found"))
      (user-error "Graphic generation unsuccessful"))))

;; TODO: Recreate previous graph
(defun denote-explore-network-view ()
  "Recreate the most recent Denote graph with external software."
  (interactive)
  (let* ((graph-type denote-explore-network-previous)
	 (format (assoc denote-explore-network-format
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
- Neighbourhood: Generate a network of notes from a parent at depth.
  Depth = 1 shows linked notes; depth 2 all notes linked to linked notes etc.
- Keywords: Generate network of keywords.  Each note with two or more keywords
  forms a complete graph, which are merged into a weighted undirected graph."
  (interactive)
  (let* ((options (mapcar #'car denote-explore-graph-types))
	 (graph-type (completing-read "Network type?" options))
	 (config (assoc graph-type denote-explore-graph-types))
	 (generate-fn (plist-get (cdr config) :generate))
	 (graph (funcall generate-fn)))
    (denote-explore--network-save graph)
    (denote-explore-network-view)))

;;;###autoload
(defun denote-explore-isolated-notes ()
  "Identify Denote note files without any links or backlinks."
  (interactive)
  (let* ((files (denote-directory-files nil nil t))
	 (all-ids (mapcar #'denote-retrieve-filename-identifier files))
	 (edges (denote-explore--network-extract-edges files))
	 (linked-ids (denote-explore--network-extract-unique-nodes edges))
	 (isolated-ids (seq-remove (lambda (id) (member id linked-ids))
				   all-ids))
	 (regex (mapconcat (lambda (item)
			     (concat "\\b" item "\\b")) isolated-ids "\\|"))
	 (isolated-files (seq-filter (lambda (item)
				       (string-match regex item)) files)))
    (find-file (completing-read "Select file with zero keywords: "
				isolated-files))))

(provide 'denote-explore)
;;; denote-explore.el ends here
