;; -*- coding: utf-8; lexical-binding: t; fill-column: 80 -*-
;; Copyright (C) 2012 Eric Merritt
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provides a banner used to display information about the build process
(require 'projmake-util)
(require 'projmake-project)
(require 'projmake-parse-engine)

(defun projmake-banner-building (project)
  "Notify the banner that we are building the project"
  (projmake-banner--notify project "BUILDING ...."))

(defun projmake-banner-show (project)
  "Show a correct banner based on the details of the project itself"
  (setf (projmake-project-warning-count project)
        (projmake-banner--get-err-count project "w"))
  (setf (projmake-project-error-count project)
        (projmake-banner--get-err-count project "e"))
  (cond
   ((projmake-project-inturrupted project)
    (projmake-banner--notify project "Inturrupted"))
   ((not (= 0 (projmake-project-last-exitcode project)))
    (projmake-banner--notify-failed project))
   (t (projmake-banner-clear project))))

(defun projmake-banner-clear (project)
  (projmake-do-for-project-buffers project
                                   (setf header-line-format nil)))

(defun projmake-banner--notify-failed (project)
  ;; Add warning to the top of the file
  (projmake-banner--notify
   project
   (format "R:%d E:%d W:%d BUILD FAILED IN THIS PROJECT"
           (projmake-project-last-exitcode project)
           (projmake-project-error-count project)
           (projmake-project-warning-count project))
   t))

(defun projmake-banner--notify (project detail &rest error)
  ;; Add warning to the top of the file
  (projmake-do-for-project-buffers project
                                   (projmake-banner--update-header
                                    detail error)))


(defun projmake-banner--pad-header-line (string)
  (let* ((size (window-total-width))
         (padding (- size (length string)))
         (pad (+ (length string) padding))
         (line-format (format "%%%ds" (- pad))))
    (format line-format string)))

(defun projmake-banner--update-header (str &rest error)
  (let ((header-line-string (projmake-banner--pad-header-line str))
        (hface (if error
                   'projmake-notify-err
                 'projmake-notify-normal)))
    (setq header-line-format
          (list :propertize header-line-string
                'face hface))))

(defun projmake-banner--get-err-count (project type)
  "Return number of errors of specified TYPE for ERR-INFO-LIST."
  (let* ((error-info-list (projmake-project-error-info project))
         (err-count 0))
    (dolist (err error-info-list)
      (when (string-equal type (projmake-error-info-type err))
        (setq err-count (+ err-count 1))))
    err-count))

(provide 'projmake-banner)
