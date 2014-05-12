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
;; Provides the default values for a parse engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This provides the methods for creating an error parsing engine along
;; with the relevant bits for creating errors and the like.
;;
;; The parse engine contains three main functions. `init` takes no
;; arguments but is expected to return a `state` value that will be
;; passed to future functions. `init` is called before the build starts.
;;
;; The function `parse-output` takes the state created by `init` and
;; the current output of build command. The function should return a list
;; containing the new state of the process at index 1, and a list of
;; generated error `projmake-error-info` structs at position 2.
;;
;; The `stop` function takes the last state return by `parse-output` and returns
;; the final parsed `projmake-error-info` structs. The idea here is taht if
;; there are any partial lines or the like the final parsing and cleanup can be
;; done.
(require 'cl-lib)
(require 'projmake-project)

(cl-defstruct projmake-parse-engine
  name
  init
  parse-output
  stop)

(cl-defstruct (projmake-error-info)
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


(defun projmake-parse-engine-split-output (residual new-output)
  "Split OUTPUT into lines.
Return last one as residual if it does not end with newline char.
Returns ((LINES) RESIDUAL)."
  (let ((output (if residual
                    (concat residual new-output)
                  new-output)))
    (when (and output (> (length output) 0))
      (let* ((lines (split-string output "[\n\r]+"))
             (complete (equal "\n" (char-to-string
                                    (aref output (1- (length output))))))
             (residual nil))
        (when (not complete)
          (setf residual (car (last lines)))
          (setf lines (butlast lines)))
        (list lines residual)))))

(provide 'projmake-parse-engine)
