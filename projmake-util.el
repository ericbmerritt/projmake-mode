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


(defun projmake-dbus-notify (project message good)
  (condition-case nil
      (progn
        (require 'notifications)
        (if (fboundp 'dbus-call-method)
            (let ((title (projmake-make-process-name project)))
              (if good
                  (notifications-notify :title  title
                                        :body message
                                        :image-path (concat "file://" projmake-dir "good-build.png")
                                        :urgency 'low)

                (notifications-notify :title  title
                                      :body message
                                      :image-path (concat "file://" projmake-dir "bad-build.png")
                                      :urgency 'low))
              t)
          nil))
    (error
     nil)))

(defun projmake-growl-notify (project message good)
  (let* ((title (projmake-make-process-name project))
         (cmd (concat "-a Emacs -m \"" message "\" -t \"" title "\" ")))
    (condition-case nil
        (= 0 (call-process "growlnotify" nil nil nil cmd))
      (error
       nil))))

(defun projmake-notify (project message good)
  (cond
   ((projmake-dbus-notify project message good) t)
   ((projmake-growl-notify project message good) t)
   (t nil)))


(provide 'projmake-util)
