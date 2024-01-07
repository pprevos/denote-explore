;;; denote-explore-dashboard.el --- Denote dashboard widget -*- lexical-binding: t -*-

;; Copyright (C) 2023 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-extra/
;; Version: 1.1
;; Package-Requires: ((emacs "29.1") (dashboard "2.19.1"))

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

;; Add a Denote widget to the Emacs dashboard

;; Dashboard widget for Denote statistics


;;; Code:

(require 'dashboard nil t)
(require 'all-the-icons nil t)

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
    (insert prefix (denote-explore-count-notes) "\n")
    (insert prefix (denote-explore-count-keywords)))
  (dashboard-insert-shortcut 'denote "d" "Denote:"))

;;;###autoload
(defun denote-explore-dashboard-activate ()
  "Add the Denote statistics to the Emacs dashboard."
  (interactive)
  (add-to-list 'dashboard-item-generators
               '(denote . denote-explore--dashboard))
  (add-to-list 'dashboard-items '(denote) t)
  (add-to-list 'dashboard-item-shortcuts '(denote . "d")))

;;;###autoload
(defun denote-explore-dashboard-deactivate ()
  "Remove the Denote statistics to the Emacs dashboard."
  (interactive)
  (setq dashboard-items
	(assq-delete-all 'denote dashboard-items)))

(provide 'denote-explore-dashboard)

;;; denote-explore-dashboard.el ends here
