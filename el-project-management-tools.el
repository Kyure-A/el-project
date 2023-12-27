;;; el-project-management-tools.el --- Generate project skelton for Emacs Lisp  -*- lexical-binding: t; -*-

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

(defun el-project:make-eask ()
  "Make Eask.")

(defun el-project:make-cask ()
  "Make Cask.")

(defun el-project:make-keg ()
  "Make Keg.")


(provide 'el-project-management-tools)
;;; el-project-management-tools.el ends here
