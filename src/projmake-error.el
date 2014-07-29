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

(cl-defstruct (projmake-error)
  ;; The path to the file either relative to the the project root or absolute
  file
  line
  (char 0)
  (end-char 0)
  ;; The line in the compile output that reported this error
  output-line
  ;; The type should be a string of either 'e' or 'w'
  (type "e")
  ;; The text of the error
  text)

(defun projmake-error/get-face-for-error (error)
  (if (string= (projmake-error-type error) "e")
      'projmake-errline
    'projmake-warnline))

(defun projmake-error/expand-type (error)
  (if (string= (projmake-error-type error) "e")
      "error"
    "warning"))

(provide 'projmake-error)
