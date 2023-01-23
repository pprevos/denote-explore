;;; denote-explore.el --- Collection of functions to explore Denote files -*- lexical-binding: t -*-

;; Copyright (C) 2023 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/denote-extra/
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (dash "2.19.1") (f "0.20.0"))

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
;; 1. Statistics
;; 2. Random walks
;; 3. Network diagrams (TODO)
;; 4. Quality Assurance

;;; Code:

(require 'denote-explore-stats)
(require 'denote-explore-random)
(require 'denote-explore-qa)

(provide 'denote-explore)
;;; denote-explore.el ends here
