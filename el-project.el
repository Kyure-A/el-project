;;; el-project.el --- Generate project skelton for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Kyure_A

;; Author: Kyure_A <twitter.com/kyureq>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/Kyure-A/el-project

;; SPDX-License-Identifier:  GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate project skelton for Emacs Lisp

;;; Code:

(require 'f)
(require 's)

(defgroup el-project ()
  "Generate project skelton for Emacs Lisp."
  :group 'tools
  :prefix "el-project:"
  :link '(url-link "https://github.com/Kyure-A/el-project"))

(defun el-project:make-elisp (project-name
			      project-short-description
			      year
			      full-name
			      contact
			      keyword
			      github-user-name
			      github-repo-name)
  ""
  (let ((format-str (f-read-text "./skelton/.el")))
    (s-format format-str
	      'aget
	      `(("project-name" . ,project-name)
		("project-short-description" . ,project-short-description)
		("year" . ,year)
		("full-name" . ,full-name)
		("contact" . ,contact)
		("keyword" . ,keyword)
		("github-user-name" . ,github-user-name)
		("github-repo-name" . ,github-repo-name)))
    (f-append-text "")))

(defun el-project:make-readme (project-name
			       project-short-description
			       github-user-name
			       github-repo-name)
  ""
  (let ((format-str (f-read-text "./skelton/README.org")))
    (s-format format-str
	      'aget
	      `(("project-name" . ,project-name)
		("project-short-description" . ,project-short-description)
		("github-user-name" . ,github-user-name)
		("github-repo-name" . ,github-repo-name)))
    (f-append-text "")))

(defun el-project:make-project ()
  "")


(provide 'el-project)
;;; el-project.el ends here
