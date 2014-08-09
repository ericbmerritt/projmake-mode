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
(require 'cl-lib)
(require 'projmake-log)

(declare-function projmake-default-parse-engine/make
                  "projmake-default-parse-engine.el"
                  nil)

(cl-defstruct projmake-project
  file
  dir
  name
  shell
  parse-engine
  file-name-rectifier
  last-exitcode
  process
  (build-counter 0)
  (build? t))

(cl-defun projmake-project/make-project (file &key name shell parse-engine file-name-rectifier)
  "Creates a project object for projmake-prjoject."
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
     :parse-engine parse-engine
     :file-name-rectifier file-name-rectifier)))

(defun projmake-project/parse-engine-name (project)
  "Make it easy to call the current parse engine name"
  (projmake-parse-engine-name (projmake-project-parse-engine project)))

(defun projmake-project/parse-engine-init (project)
  "Make it easy to call the current parse engine"
  (funcall (projmake-parse-engine-init
            (projmake-project-parse-engine project))))

(defun projmake-project/find-by-file (projects file)
  "This searches the list of projects search for the project
associated with this buffer. The buffer related projet is defined
as anything under the directory where the project configuration
file exists."
  (projmake-log/debug "Looking for project for file: %s" file)
  ;;we do this to ignore cl warnings about find-if. Wish we could turn
  ;;off that globally
  (with-no-warnings
    (cdr (find-if (lambda (prj-kv)
                    (let* ((prj (cdr prj-kv)))
                      (projmake-project/is-file-part-of-project prj file)))
                  projects))))

(defun projmake-project/find-by-buffer (projects buffer)
  "Grab the filename from the buffer and use
projmake-find-project-by-file to find the related project."
  (projmake-project/find-by-file projects (buffer-file-name buffer)))

(defun projmake-project/is-file-part-of-project (prj file)
  "Given a project and a file tests to see if the file belongs to the
project"
  (let ((projmake-dir (projmake-project-dir prj)))
    (eql t (compare-strings projmake-dir
                            0 nil
                            file 0 (length projmake-dir)))))

(defun projmake-project/is-buffer-part-of-project? (prj buffer)
  "Given a project and a buffer tests to see if the file belongs to
the project"
  (if (buffer-file-name buffer)
      (projmake-project/is-file-part-of-project prj (buffer-file-name buffer))
    nil))

(defmacro projmake-project/do-for-project-buffers (project &rest actions)
  `(dolist (buffer (buffer-list))
     (when (projmake-project/is-buffer-part-of-project? ,project buffer)
       (with-current-buffer buffer
         ,@actions))))

(provide 'projmake-project)
