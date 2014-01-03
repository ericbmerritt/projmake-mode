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
;; Error parsing and handling
(require 'cl-lib)
(require 'projmake-util)
(require 'projmake-parse-engine)

(defvar projmake-error-start-parsing
  "^File\s\"\\(.+\\)\",\sline\s\\([0-9]+\\),\
\scharacters\s\\([0-9]+-[0-9]+\\|[0-9+]\\):")

(cl-defstruct projmake-ocaml-state
  (current-state 'initial)
  (current-output-line 0)
  file
  type
  (line 0)
  char
  (residual "")
  (text ""))

(defun projmake-ocaml-parse-engine-make ()
  "Build the struct for the default parse engine"
  (make-projmake-parse-engine
   :init  'projmake-ocaml-parse-engine-init
   :parse-output 'projmake-ocaml-parse-engine-parse-output
   :stop 'projmake-ocaml-parse-engine-stop))

(defun projmake-ocaml-parse-engine-init ()
  "Initialize the engine state. nothing much to do here. Our state is always
going to be the unparsed remainder."
  (make-projmake-ocaml-state))

(defun projmake-ocaml-parse-engine-parse-output (state output)
  "Split OUTPUT into lines, merge in residual if necessary."
  (let* ((residual (projmake-ocaml-state-residual state))
         (result (projmake-parse-engine-split-output residual output))
         (lines (car result))
         (new-residual (cadr result)))
    (setf (projmake-ocaml-state-residual state) new-residual)
    (let ((x (projmake-ocaml-parse-engine--parse-lines state lines)))
      (list state x))))

(defun projmake-ocaml-parse-engine-stop (state)
  "Parse residual if it's non empty."
  (let* ((residual (projmake-ocaml-state-residual state))
         (lines (car (projmake-parse-engine-split-output residual ""))))
    (projmake-ocaml-parse-engine--parse-lines state lines)))

(defun projmake-ocaml-parse-engine--parse-lines (state lines)
  "Parse error lines"
  (let ((acc '()))
    (while lines
      (let* ((line (car lines))
             (line-err-info
              (projmake-ocaml-parse-engine--parse-line state line)))
        (setq lines (cdr lines))
        (projmake-log PROJMAKE-DEBUG "parsed '%s', %s line-err-info"
                      line (if line-err-info "got" "no"))
        (when line-err-info
          (push line-err-info acc))))
    acc))

(defun projmake-ocaml-parse-engine--initial-line (state line)
  (if (string-match projmake-error-start-parsing line)
      (let* ((file (match-string 1 line))
             (line-number (match-string 2 line))
             (raw-char (match-string 3 line))
             (char (if (string-match "^\\([0-9]+\\)-[0-9]+$" raw-char)
                       (match-string 1 raw-char)
                     raw-char)))
        (setf (projmake-ocaml-state-file state) file)
        (setf (projmake-ocaml-state-line state)
              (string-to-number line-number))
        (setf (projmake-ocaml-state-char state)
              (string-to-number char))
        (setf (projmake-ocaml-state-current-state state) 'body-start)
        nil)
    nil))

(defun projmake-ocaml-parse-engine--body (state line)
  (if (string-match "^[[:blank:]]+" line)
      (progn
        (setf (projmake-ocaml-state-text state)
              (concat (projmake-ocaml-state-text state) line))
        nil)
    (let ((err (make-projmake-error-info
                :file (projmake-ocaml-state-file state)
                :line (projmake-ocaml-state-line state)
                :type (projmake-ocaml-state-type state)
                :text (projmake-ocaml-state-text state))))
      (setf (projmake-ocaml-state-current-state state) 'initial)
      (setf (projmake-ocaml-state-type state) "e")
      (setf (projmake-ocaml-state-file state) nil)
      (setf (projmake-ocaml-state-line state) nil)
      (setf (projmake-ocaml-state-char state) nil)
      (setf (projmake-ocaml-state-text state) "")
      err)))

(defun projmake-ocaml-parse-engine--body-start (state line)
  (if (or (string-match "^\\(Error\\):\\(.+\\)" line)
          (string-match "^\\(Warning\\):\\(.+\\)" line))
      (let ((type (if (string= (match-string 1 line) "Error")
                      "e"
                    "w"))
            (text (match-string 2 line)))
        (setf (projmake-ocaml-state-type state) type)
        (setf (projmake-ocaml-state-text state) text)
        (setf (projmake-ocaml-state-current-state state) 'body)
        nil)
    nil))

(defun projmake-ocaml-parse-engine--parse-line (state line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (let* ((parse-state (projmake-ocaml-state-current-state state))
        (r (cond
            ((eql 'initial parse-state)
             (projmake-ocaml-parse-engine--initial-line state line))
            ((eql 'body-start parse-state)
             (projmake-ocaml-parse-engine--body-start state line))
            ((eql 'body parse-state)
             (projmake-ocaml-parse-engine--body state line)))) )
    r
))


(provide 'projmake-ocaml-parse-engine)
