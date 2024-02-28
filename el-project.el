;;; el-project.el --- Generate project skeleton for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Kyure_A

;; Author: Kyure_A <twitter.com/kyureq>
;; Keywords: tools

;; Version: 1.1.1
;; Package-Requires: ((emacs "24.1") (f "0.20.0") (s "1.13.1"))
;; URL: https://github.com/Kyure-A/el-project

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; Generate project skeleton for Emacs Lisp

;;; Code:

(require 'f)
(require 's)

(defgroup el-project ()
  "Generate project skeleton for Emacs Lisp."
  :group 'tools
  :prefix "el-project:"
  :link '(url-link "https://github.com/Kyure-A/el-project"))

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

;; (el-project::get-skeleton-dir :: (function () string))
(defun el-project::get-skeleton-dir ()
  "Return el-project/skeleton."
  (f-join (el-project::get-el-project-dir) "skeleton"))

(defcustom el-project:default-skeleton-dir (el-project::get-skeleton-dir)
  "This variable that sets the skeleton directory location."
  :type 'string)

;; (el-project::get-file-path :: (function (string) 'mixed))
(defun el-project::get-file-path (filename)
  "Return FILENAMEs path."
  (f-join el-project:default-skeleton-dir filename))

(defcustom el-project:default-github-user-name "Your GitHub user name"
  "Enter your GitHub user name."
  :type 'string)

(defcustom el-project:default-full-name "Your name"
  "Enter your name."
  :type 'string)

(defcustom el-project:default-contact "Contact address for example, Email, Twitter, Mastodon"
  "Enter your contact address for example, Email, Twitter, Mastodon."
  :type 'string)

(defcustom el-project:skeleton-files '("${project-name}.el" "codecov.yml" ".dir-locals.el" ".gitignore" "LICENSE" "test/${project-name}-test.el")
  ""
  :type '(repeat string))

;; (el-project::create-file :: (function (string string (list (cons string string))) string))
(defun el-project::create-file (project-name skeleton value-list)
  "Create new file from PROJECT-NAME, SKELETON and VALUE-LIST."
  (let* ((format-str (f-read-text (el-project::get-file-path skeleton)))
         (dir-path (f-join (el-project::get-current-dir) project-name))
         (path (f-join dir-path (s-format skeleton 'aget value-list)))
         (test-path (f-join (el-project::get-current-dir) project-name "test")))
    
    (unless (f-exists-p dir-path)
      (f-mkdir-full-path dir-path))

    (unless (f-exists-p test-path)
      (f-mkdir-full-path test-path))
    
    (if (not (f-exists-p path))
        (progn
          (f-append-text (s-format format-str 'aget value-list) 'utf-8 path)
          path)
      nil)))

;; (el-project::create-files :: (function (string (list string) (list (cons string string))) string))
(defun el-project::create-files (project-name skeletons value-list)
  ""
  (mapcar #'(lambda (skeleton) (el-project::create-file project-name skeleton value-list)) skeletons))

;; (el-project::select-readme :: (function () string))
(defun el-project::select-readme ()
  "A dialog box for selecting README.org or README.md is displayed in the echo area."
  (let* ((choices '(("Org" . "README.org")
                    ("Markdown" . "README.md")))
         (chosen (completing-read "[8/10] Do you use Markdown or Org?: " choices))
         (result (cdr (assoc chosen choices))))
    (if result
        result
      "README.org")))

;; (el-project::select-pmtools-file :: (function () string))
(defun el-project::select-pmtools-file ()
  "A dialog box for selecting project management tools is displayed in the echo area."
  (let* ((choices '("Eask" "Cask" "Keg"))
         (chosen (completing-read "[9/10] Project management tools (Eask)?: " choices)))
    (if (member chosen choices)
        chosen
      "Eask")))

;; (el-project::select-keyword :: (function () string))
(defun el-project::select-keyword ()
  "A dialog box for selecting keywords is displayed in the echo area."
  (let* ((choices '("abbrev" "bib" "c" "calendar" "comm" "convenience" "data" "docs" "emulations" "extensions" "faces" "files" "frames" "games" "hardware" "help" "hypermedia" "i18n"
                    "internal" "languages" "lisp" "local" "maint" "mail" "matching" "mouse" "multimedia" "news" "outlines" "processes" "terminals" "tex" "tools" "unix" "vc" "wp"))
         (chosen (completing-read "[10/10] Select keyword (tools)?: " choices)))
    (if (member chosen choices)
        chosen
      "tools")))

;; (el-project::make-value-list :: (function (string string string string string string string string) (list (cons string string))))
(defun el-project::make-value-list (project-name
                                    project-short-description
                                    year
                                    full-name
                                    contact
                                    keyword
                                    github-user-name
                                    github-repo-name)
  "aa"
  (list
   `("project-name" . ,project-name)
   `("project-short-description" . ,project-short-description)
   `("year" . ,year)
   `("full-name" . ,full-name)
   `("contact" . ,contact)
   `("keyword" . ,keyword)
   `("github-user-name" . ,github-user-name)
   `("github-repo-name" . ,github-repo-name)))

;; (el-project:make-project :: (function () ()))
(defun el-project:make-project ()
  "Receive information about the project interactively from the echo area and create a template for the project."
  (interactive)
  (let* ((project-name (read-string "[1/10] project-name (Your project name)?: " nil nil "project-name"))
         (github-repo-name (read-string (format "[2/10] github-repo-name (%s)?: " project-name) nil nil project-name))
         (project-short-description (read-string "[3/10] project-short-description (Short description of your project)?: " nil nil "Short description of your project"))
         (contact (read-string (format "[4/10] contact (%s)?: " el-project:default-contact) nil nil el-project:default-contact))
         (full-name (read-string (format "[5/10] full-name (%s)?: " el-project:default-full-name) nil nil el-project:default-full-name))
         (github-user-name (read-string (format "[6/10] github-user-name (%s)?: " el-project:default-github-user-name) nil nil el-project:default-github-user-name))
         (year (read-string (format "[7/10] year (%d)?: " (el-project::get-year)) nil nil (el-project::get-year)))
         (readme (el-project::select-readme))
         (pmtools (el-project::select-pmtools-file))
         (keyword (el-project::select-keyword))
         (skeleton-files (append el-project:skeleton-files (list readme pmtools)))
         (value-list (el-project::make-value-list project-name project-short-description year full-name contact keyword github-user-name github-repo-name)))
    (el-project::create-files project-name skeleton-files value-list)))

(provide 'el-project)
;;; el-project.el ends here
