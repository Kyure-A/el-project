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

(defcustom el-project:default-github-user-name "Your GitHub user name")

(defcustom el-project:default-full-name "Your name")

;; (el-project::get-year :: (function () number))
(defun el-project::get-year ()
  "Return year."
  (nth 5 (decode-time (current-time))))

;; (el-project::get-current-dir :: (function () string))
(defun el-project::get-current-dir ()
  "Return current directory."
  default-directory)

;; (el-project::get-el-project-dir :: (function () string))
(defun el-project::get-el-project-dir ()
  "Return el-project directory."
  (f-dirname (locate-library "el-project")))

;; (el-project::get-skelton-dir :: (function () string))
(defun el-project::get-skelton-dir ()
  "Return el-project/skelton."
  (f-join (el-project::get-el-project-dir) "/skelton"))

;; (el-project::get-file-path :: (function (string) string))
(defun el-project::get-file-path (filename)
  "Return FILENAMEs path."
  (f-join (el-project::get-skelton-dir) filename))

;; (el-project::create-file :: (function (string (list (cons string string)))))
(defun el-project::create-file (skelton value-list)
  "Create new file from SKELTON and VALUE-LIST."
  (let ((format-str (f-read-text (el-project::get-file-path skelton))))
    (s-format format-str
              'aget
              value-list)
    (f-append-text (el-project::get-current-dir))))

;; (el-project::create-el :: (function (string string string string string string string string)))
(defun el-project::create-el (project-name
                              project-short-description
                              year
                              full-name
                              contact
                              keyword
                              github-user-name
                              github-repo-name)
  "Takes PROJECT-NAME, PROJECT-SHORT-DESCRIPTION, YEAR, FULL-NAME, CONTACT, KEYWORD, GITHUB-USER-NAME, GITHUB-REPO-NAME and create Emacs Lisp file."
  (let ((value-list (list
                     `("project-name" . ,project-name)
                     `("project-short-description" . ,project-short-description)
                     `("year" . ,year)
                     `("full-name" . ,full-name)
                     `("contact" . ,contact)
                     `("keyword" . ,keyword)
                     `("github-user-name" . ,github-user-name)
                     `("github-repo-name" . ,github-repo-name))))
    (el-project::create-file "${project-name}.el" value-list)))

;; (el-project::create-test-el :: (function (string string string string)))
(defun el-project::create-test-el (project-name
                                   year
                                   full-name
                                   contact)
  "Takes PROJECT-NAME, YEAR, FULL-NAME, CONTACT and create Emacs Lisp file for test."
  (let ((value-list (list
                     `("project-name" . ,project-name)
                     `("year" . ,year)
                     `("full-name" . ,full-name)
                     `("contact" . ,contact))))
    (el-project::create-file "test/${project-name}-test.el" value-list)))

;; (el-project::create-readme-org :: (function (string string string string)))
(defun el-project::create-readme-org (project-name
                                      project-short-description
                                      github-user-name
                                      github-repo-name)
  "Takes PROJECT-NAME, PROJECT-SHORT-DESCRIPTION, GITHUB-USER-NAME, GITHUB-REPO-NAME and create README.org."
  (let ((value-list (list
                     `("project-name" . ,project-name)
                     `("project-short-description" . ,project-short-description)
                     `("github-user-name" . ,github-user-name)
                     `("github-repo-name" . ,github-repo-name))))
    (el-project::create-file "README.org" value-list)))

;; (el-project::create-readme-md :: (function (string string string string)))
(defun el-project::create-readme-md (project-name
                                     project-short-description
                                     github-user-name
                                     github-repo-name)
  "Takes PROJECT-NAME, PROJECT-SHORT-DESCRIPTION, GITHUB-USER-NAME, GITHUB-REPO-NAME and create README.md."
  (let ((value-list (list
                     `("project-name" . ,project-name)
                     `("project-short-description" . ,project-short-description)
                     `("github-user-name" . ,github-user-name)
                     `("github-repo-name" . ,github-repo-name))))
    (el-project::create-file "README.md" value-list)))

;;
(defun el-project::create-eask-file (project-name
                                     project-short-description
                                     github-user-name
                                     github-repo-name)
  "Takes PROJECT-NAME, PROJECT-SHORT-DESCRIPTION, GITHUB-USER-NAME, GITHUB-REPO-NAME and create Eask file."
  (let ((value-list (list
                     `("project-name" . ,project-name)
                     `("project-short-description" . ,project-short-description)
                     `("github-user-name" . ,github-user-name)
                     `("github-repo-name" . ,github-repo-name))))
    (el-project::create-file "Eask" value-list)))

;;
(defun el-project::create-cask-file (project-name)
  "Takes PROJECT-NAME and create Cask file."
  (let ((value-list (list
                     `("project-name" . ,project-name))))
    (el-project::create-file "Eask" value-list)))

;;
(defun el-project::create-keg-file (project-name
                                    github-user-name
                                    github-repo-name)
  "Takes PROJECT-NAME, GITHUB-USER-NAME, GITHUB-REPO-NAME and create Keg file."
  (let ((value-list (list
                     `("project-name" . ,project-name)
                     `("github-user-name" . ,github-user-name)
                     `("github-repo-name" . ,github-repo-name))))
    (el-project::create-file "Keg" value-list)))

;;
(defun el-project::create-readme (project-name
                                  project-short-description
                                  github-user-name
                                  github-repo-name)
  "A dialog box for selecting README.org or README.md is displayed in the echo area (take PROJECT-NAME, PROJECT-SHORT-DESCRIPTION, GITHUB-USER-NAME, GITHUB-REPO-NAME)."
  (let* ((choices `(("README.org" . (el-project::create-readme-org ,project-name ,project-short-description ,github-user-name ,github-repo-name))
                    ("README.md" . (el-project::create-readme-md ,project-name ,project-short-description ,github-user-name ,github-repo-name))))
         (chosen (completing-read "[8/10] Do you use Markdown or Org?: " choices)))
    (let ((create-readme (cdr (assoc chosen choices))))
      (funcall create-readme))))

;;
(defun el-project::create-pmtools-file (project-name
                                        project-short-description
                                        github-user-name
                                        github-repo-name)
  "A dialog box for selecting project management tools is displayed in the echo area (take PROJECT-NAME, PROJECT-SHORT-DESCRIPTION, GITHUB-USER-NAME, GITHUB-REPO-NAME)."
  (let* ((choices `(("Eask" . (el-project::create-eask-file ,project-name ,project-short-description ,github-user-name ,github-repo-name))
                    ("Cask" . (el-project::create-cask-file ,project-name))
                    ("Keg" . (el-project::create-keg-file ,project-name ,github-user-name ,github-repo-name))))
         (chosen (completing-read "[9/10] Project management tools?: " choices)))
    (let ((create-pmtools-file (cdr (assoc chosen choices))))
      (funcall create-pmtools-file))))

;;
(defun el-project::select-keyword ()
  "A dialog box for selecting keywords is displayed in the echo area."
  (let* ((choices '("abbrev" "bib" "c" "calendar" "comm" "convenience" "data" "docs" "emulations" "extensions" "faces" "files" "frames" "games" "hardware" "help" "hypermedia" "i18n"
                    "internal" "languages" "lisp" "local" "maint" "mail" "matching" "mouse" "multimedia" "news" "outlines" "processes" "terminals" "tex" "tools" "unix" "vc" "wp"))
         (chosen (completing-read "[10/10] Select keyword?: " choices)))
    chosen))

;;
(defun el-project:create-project ()
  ""
  (interactive)
  (let* ((project-name (read-string "[1/10] project-name (Your project name)?: "))
         (github-repo-name (read-string "[2/10] github-repo-name (project-name)?: "))
         (project-short-description (read-string "[3/10] project-short-description (Short description of your project)?: "))
         (contact (read-string "[4/10] contact (Contact address for example, Email, Twitter, Mastodon)?: "))
         (full-name (read-string "[5/10] full-name (Your name)?: "))
         (github-user-name (read-string "[6/10] github-user-name (github)?: "))
         (year (read-string "[7/10] year (2023)?: ")))
    (el-project::create-readme project-name project-short-description github-user-name github-repo-name)
    (el-project::create-pmtools-file project-name project-short-description github-user-name github-repo-name)
    (el-project::select-keyword)))

(provide 'el-project)
;;; el-project.el ends here
