;; -*- coding: utf-8; lexical-binding: t; fill-column: 80 -*-
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
(require 'cl-lib)

(defconst PROJMAKE-NONE -1)
(defconst PROJMAKE-ERROR 0)
(defconst PROJMAKE-WARNING 1)
(defconst PROJMAKE-INFO 2)
(defconst PROJMAKE-DEBUG 3)

;; Defined in project root
(defvar projmake-log/level)
(defvar projmake-projects)

(defun projmake-log/set-level (level)
  (interactive)
  (setf projmake-log/level level))

(defun projmake-log/set-level-error ()
  (interactive)
  (projmake-log/set-level PROJMAKE-ERROR))

(defun projmake-log/set-level-warning ()
  (interactive)
  (projmake-log/set-level PROJMAKE-WARNING))

(defun projmake-log/set-level-info ()
  (interactive)
  (projmake-log/set-level PROJMAKE-INFO))

(defun projmake-log/set-level-debug ()
  (interactive)
  (projmake-log/set-level PROJMAKE-DEBUG))

(defun projmake-log/log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `projmake-log/level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (when (<= level projmake-log/level)
    (let ((msg (apply 'format text args)))
      (message "%s" msg))))

(defmacro projmake-log/error (text &rest args)
  `(projmake-log/log PROJMAKE-ERROR ,text ,@args))

(defmacro projmake-log/warning (text &rest args)
  `(projmake-log/log PROJMAKE-WARNING ,text ,@args))

(defmacro projmake-log/info (text &rest args)
  `(projmake-log/log PROJMAKE-INFO ,text ,@args))

(defmacro projmake-log/debug (text &rest args)
  `(projmake-log/log PROJMAKE-DEBUG ,text ,@args))

(provide 'projmake-log)
