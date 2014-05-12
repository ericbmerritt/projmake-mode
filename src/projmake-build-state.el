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

(cl-defstruct projmake-build-state
  project
  exitcode
  warning-count
  error-count
  inturrupted
  ;; mutable build oriented bits
  parse-engine-state
  error-info
  build-again?)

(defun* projmake-build-state (project)
  "Creates a project object for projmake-prj."
  (make-projmake-build-state
   :project project
   :warning-count 0
   :error-count 0))

(defun projmake-project-has-warnings? (project)
  (> (projmake-build-state-warning-count project) 0))

(defun projmake-project-has-errors? (project)
  (> (projmake-build-state-error-count project) 0))

(defun projmake-project-has-errors-or-warnings? (project)
  (or (projmake-project-has-warnings? project)
      (projmake-project-has-errors? project)))

(defun projmake-build-state-parse-output (build-state output)
  "Helper function to call parse output on the current parse engine"
  (let* ((engine-state (projmake-build-state-parse-engine-state build-state))
         (parse-engine-parse-output
          (projmake-parse-engine-parse-output
           (projmake-project-parse-engine
            (projmake-build-state-project build-state))))
         (result (funcall parse-engine-parse-output engine-state output))
         (new-state (car result))
         (error-infos (cadr result)))
    (setf (projmake-build-state-parse-engine-state build-state) new-state)
    error-infos))

(defun projmake-build-state-parse-engine (build-state)
  (projmake-project-parse-engine (projmake-build-state-project build-state)))

(defun projmake-build-state-parse-engine-stop (build-state)
  (let* ((engine-state (projmake-build-state-parse-engine-state build-state))
         (parse-engine-stop
          (projmake-parse-engine-stop
           (projmake-build-state-parse-engine build-state)))
         (error-infos (funcall parse-engine-stop engine-state)))
    (setf (projmake-build-state-parse-engine-state build-state) nil)
    error-infos))

(provide 'projmake-build-state)
