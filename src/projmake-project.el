;; -*- coding: utf-8; lexical-binding: t; fill-column: 80 -*-
;; Copyright (C) 2012 Eric Merritt
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project definition and initialization
(defstruct projmake-project
  file
  dir
  name
  shell
  warning-count
  error-count
  inturrupted
  (build-counter 0)
  ;; mutable build oriented bits
  residual
  error-info
  overlays
  (build? t)
  (is-building? nil)
  (build-again? nil))

(defun* projmake-prj (file &key name shell)
  "Creates a project object for projmake-prj."
  (let ((dir (file-name-directory file)))
    (make-projmake-project
     :file  file
     :dir dir
     :name (cond ((eq nil name)
                  (file-name-nondirectory (directory-file-name dir)))
                 ((stringp name)
                  name)
                 (t
                  (error "Invalid project name provided")))
     :shell (cond ((consp shell)
                   shell)
                  ((stringp shell)
                   (split-string shell "[ \n\t]+"))
                  (t
                   (error "Shell command required!")))
     :build? t
     :is-building? nil
     :build-again? nil)))

(defun projmake-project-has-warnings? (project)
  (> (projmake-project-warning-count project) 0))

(defun projmake-project-has-errors? (project)
  (> (projmake-project-error-count project) 0))

(defun projmake-project-has-errors-or-warnings? (project)
  (or (projmake-project-has-warnings? project)
      (projmake-project-has-errors? project)))

(defun projmake-cleanup-transient-project-data (project)
  "Clean up the build oriented bits of the project"
  (setf (projmake-project-inturrupted project) nil)
  (setf (projmake-project-residual project) nil)
  (setf (projmake-project-is-building? project) nil))

(provide 'projmake-project)
