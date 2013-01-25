;; -*- coding: utf-8; lexical-binding: t -*-
;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.
;; Copyright (C) 2012 Eric Merritt

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
(require 'cl)


(defconst PROJMAKE-NONE -1)
(defconst PROJMAKE-ERROR 0)
(defconst PROJMAKE-WARNING 1)
(defconst PROJMAKE-INFO 2)
(defconst PROJMAKE-DEBUG 3)

(defun projmake-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `projmake-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (when (<= level projmake-log-level)
    (let* ((msg (apply 'format text args)))
      (message "%s" msg))))

(defun projmake-find-first (function items)
  "Return the first non-nil value that function returns"
  (catch 'break
    (dolist (el items)
      (let ((result (funcall function el)))
        (when result
          (throw 'break result))))))

(defvar projmake-dir
  (file-name-directory load-file-name))

(defun projmake-pad-header-line (string)
  (let* ((size (window-total-width))
         (padding (- size (length string)))
         (pad (+ (length string) padding))
         (line-format (format "%%%ds" (- pad))))
    (format line-format string)))

(defun projmake-update-header (str &rest error)
  (if (not str)
      (setq header-line-format nil)
    (let ((header-line-string (projmake-pad-header-line str))
          (hface (if error
                     'projmake-notify-err
                   'projmake-notify-normal)))
      (setq header-line-format
            (list :propertize header-line-string
                  'face hface)))))

(defmacro projmake-do-for-project-buffers (project &rest actions)
  `(dolist (buffer (buffer-list))
    (when (projmake-is-buffer-part-of-project ,project buffer)
      (with-current-buffer buffer
        ,@actions))))


(provide 'projmake-util)
