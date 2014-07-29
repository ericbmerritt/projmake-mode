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
(require 'projmake-error)
(require 'projmake-build-state)

(defun projmake-banner/building (project)
  "Notify the banner that we are building the project"
  (projmake-banner/notify project "BUILDING ...."))

(defun projmake-banner/show (build-state)
  "Show a correct banner based on the details of the project itself"
  (setf (projmake-build-state-warning-count build-state)
        (projmake-banner/get-err-count build-state "w"))
  (setf (projmake-build-state-error-count build-state)
        (projmake-banner/get-err-count build-state "e"))
  (cond
   ((projmake-build-state-inturrupted build-state)
    (projmake-banner/notify build-state "Inturrupted"))
   ((not (= 0 (projmake-build-state-exitcode build-state)))
    (projmake-banner/notify-failed build-state))
   (t (projmake-banner/clear (projmake-build-state-project build-state)))))

(defun projmake-banner/clear (project)
  (projmake-project/do-for-project-buffers project
                                           (setf header-line-format nil)))

(defun projmake-banner/notify-failed (build-state)
  ;; Add warning to the top of the file
  (projmake-banner/notify
   build-state
   (format "R:%d E:%d W:%d BUILD FAILED IN THIS PROJECT"
           (projmake-build-state-exitcode build-state)
           (projmake-build-state-error-count build-state)
           (projmake-build-state-warning-count build-state))
   t))

(defun projmake-banner/notify (build-state detail &rest error)
  ;; Add warning to the top of the file
  (projmake-project/do-for-project-buffers
   (projmake-build-state-project build-state)
   (projmake-banner/update-header detail error)))


(defun projmake-banner/pad-header-line (string)
  (let* ((size (window-total-width))
         (padding (- size (length string)))
         (pad (+ (length string) padding))
         (line-format (format "%%%ds" (- pad))))
    (format line-format string)))

(defun projmake-banner/update-header (str &rest error)
  (let ((header-line-string (projmake-banner/pad-header-line str))
        (hface (if error
                   'projmake-notify-err
                 'projmake-notify-normal)))
    (setq header-line-format
          (list :propertize header-line-string
                'face hface))))

(defun projmake-banner/get-err-count (build-state type)
  "Return number of errors of specified TYPE for ERR-INFO-LIST."
  (let* ((error-info-list (projmake-build-state-error-info build-state))
         (err-count 0))
    (dolist (err error-info-list)
      (when (string-equal type (projmake-error-type err))
        (setq err-count (+ err-count 1))))
    err-count))

(provide 'projmake-banner)
