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

(defun projmake-dbus-notify (project message)
  (if (fboundp 'dbus-call-method)
      (let ((title (projmake-make-process-name project)))
        (require 'notifications)
        (notify-notifications :title  title
                              :body message
                              :urgency 'low)
        t)
    nil))

(defun projmake-growl-notify (project message)
  (let* ((title (projmake-make-process-name project))
         (cmd (concat "-a Emacs -m \"" message "\" -t \"" title "\" ")))
    (= 0 (call-process "growlnotify" nil nil nil cmd))))

(defun projmake-notify (project message)
  (cond
   ((projmake-dbus-notify project message) t)
   ((projmake-growl-notify project message) t)
   (t nil)))


(provide 'projmake-util)
