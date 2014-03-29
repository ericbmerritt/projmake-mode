;; -*- coding: utf-8; lexical-binding: t -*-
;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.
;; Copyright (C) 2012 Eric Merritt

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
(with-no-warnings
  (require 'cl))

(defconst PROJMAKE-NONE -1)
(defconst PROJMAKE-ERROR 0)
(defconst PROJMAKE-WARNING 1)
(defconst PROJMAKE-INFO 2)
(defconst PROJMAKE-DEBUG 3)

;; Defined in project root
(defvar projmake-log-level)
(defvar projmake-projects)

(defun projmake-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `projmake-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (when (<= level projmake-log-level)
    (let ((msg (apply 'format text args)))
      (message "%s" msg))))

(defun projmake-find-project-by-file (file)
  "This searches the list of projects search for the project
associated with this buffer. The buffer related projet is defined
as anything under the directory where the project configuration
file exists."
  (projmake-log PROJMAKE-DEBUG
                "Looking for project for file: %s" file)
  ;;we do this to ignore cl warnings about find-if. Wish we could turn
  ;;off that globally
  (with-no-warnings
    (cdr (find-if (lambda (prj-kv)
                    (let* ((prj (cdr prj-kv)))
                      (projmake-is-file-part-of-project prj file)))
                  projmake-projects))))

(defun projmake-find-project-by-buffer (buffer)
  "Grab the filename from the buffer and use
projmake-find-project-by-file to find the related project."
  (projmake-find-project-by-file (buffer-file-name buffer)))

(defun projmake-is-file-part-of-project (prj file)
  "Given a project and a file tests to see if the file belongs to the
project"
  (let ((projmake-dir (projmake-project-dir prj)))
    (eql t (compare-strings projmake-dir
                            0 nil
                            file 0 (length projmake-dir)))))

(defun projmake-is-buffer-part-of-project (prj buffer)
  "Given a project and a buffer tests to see if the file belongs to
the project"
  (if (buffer-file-name buffer)
      (projmake-is-file-part-of-project prj (buffer-file-name buffer))
    nil))

(defun projmake-find-first (function items)
  "Return the first non-nil value that function returns"
  (catch 'break
    (dolist (el items)
      (let ((result (funcall function el)))
        (when result
          (throw 'break result))))))

(defvar projmake-dir
  (file-name-directory load-file-name))

(defmacro projmake-do-for-project-buffers (project &rest actions)
  `(dolist (buffer (buffer-list))
     (when (projmake-is-buffer-part-of-project ,project buffer)
       (with-current-buffer buffer
         ,@actions))))


(provide 'projmake-util)

;; Local Variables:
;; coding: utf-8
;; lexical-binding: t
;; End:
