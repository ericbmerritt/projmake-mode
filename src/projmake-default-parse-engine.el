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
(require 'compile)
(require 'projmake-log)
(require 'projmake-parse-engine)

(defun projmake-default-parse-engine/make ()
  "Build the struct for the default parse engine"
  (make-projmake-parse-engine
   :name "Default single line parse engine"
   :init  'projmake-default-parse-engine/init
   :parse-output 'projmake-default-parse-engine-parse/output
   :stop 'projmake-default-parse-engine/stop))

(defun projmake-default-parse-engine/init ()
  "Initialize the engine state. nothing much to do here. Our state is always
going to be the unparsed remainder."
  "")

(defun projmake-default-parse-engine/parse-output (residual output)
  "Split OUTPUT into lines, merge in residual if necessary."

  (let* ((result (projmake-parse-engine/split-output residual output))
         (lines (car result))
         (new-residual (cadr result)))
    (list new-residual
          (projmake-default-parse-engine/parse-lines lines))))

(defun projmake-default-parse-engine/stop (residual)
  "Parse residual if it's non empty."
  (let ((lines (car (projmake-parse-engine/split-output residual ""))))
    (projmake-default-parse-engine/parse-lines lines)))

(defun projmake-default-parse-engine/parse-lines (lines)
  "Parse error lines"
  (let ((acc '()))
    (while lines
      (let* ((line (car lines))
             (line-err-info
              (projmake-default-parse-engine/parse-line line)))
        (setq lines (cdr lines))
        (projmake-log/debug "parsed '%s', %s line-err-info"
                            line (if line-err-info "got" "no"))
        (when line-err-info
          (push line-err-info acc))))
    acc))

(defun projmake-default-parse-engine/import-from-compile-el (original-list)
  "Grab error line patterns from ORIGINAL-LIST in compile.el format.
Convert it to flymake internal format."
  (let ((result '()))
    (while original-list
      (let* ((item (cdr (car original-list)))
             (regexp (nth 0 item))
             (file (nth 1 item))
             (line (nth 2 item))
             (col (nth 3 item)))
        (setf original-list (cdr original-list))
        (when (not (functionp line))
          (push (list
                 regexp
                 (if (consp file)
                     (car file)
                   file)
                 (if (consp line)
                     (car line)
                   :                   line)
                 (if (consp col)
                     (car col)
                   col))
                result))))))

(defvar projmake-default-parse-engine/err-line-patterns
  (append
   '(
     ;; MS Visual C++ 6.0
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \
\\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; jikes
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\
\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\|Semantic Error\\)\
:[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; MS midl
     ("midl[ ]*:[ ]*\\(command line error .*\\)"
      nil nil nil 1)
     ;; MS C#
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\),[0-9]+)\: \
\\(\\(error\\|warning\\|fatal error\\) \\(CS[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; perl
     ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
     ;; PHP
     ("\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \
\\([0-9]+\\)" 2 3 nil 1)
     ;; ant/javac
     (" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\
\\([0-9]+\\)\:[ \t\n]*\\(.+\\)"
      2 4 nil 5))
   (projmake-default-parse-engine/import-from-compile-el
    compilation-error-regexp-alist-alist))
  "Patterns for matching error/warning lines.  Each pattern has the form
\(REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).
Use `projmake-reformat-err-line-patterns-from-compile-el' to add patterns
from compile.el")

(defun projmake-default-parse-engine/apply-pattern (pattern-list line)
  "Apply an individual pattern to a single string return the
error-info struct if successful and nil if not"
  (let ((pattern (car pattern-list)))
    (if (string-match pattern line)
        (let ((file-idx (nth 1 pattern-list))
              (line-idx (nth 2 pattern-list))
              (raw-file-name nil)
              (line-no 0)
              (err-type "e")
              (err-text nil))

          (setq raw-file-name
                (if file-idx
                    (match-string file-idx line)
                  nil))

          (setq line-no
                (if line-idx
                    (string-to-number (match-string line-idx line))
                  0))

          (setq err-text
                (if (> (length pattern-list) 4)
                    (match-string (nth 4 pattern-list) line)
                  nil))

          (unless err-text
            (setq err-text "<no error text>"))

          (when (and err-text (string-match "^[wW]arning" err-text))
            (setq err-type "w"))

          (projmake-log/debug
           (concat  "parse line: file-idx=%s line-idx=%s file=%s "
                    "line=%s text=%s")
           file-idx line-idx
           raw-file-name line-no err-text)
          (make-projmake-error-info :file raw-file-name
                                    :line line-no
                                    :type err-type
                                    :text err-text))
      nil)))

(defun projmake-default-parse-engine/parse-line (line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (projmake-find-first
   #'(lambda (pattern-list)
       (projmake-default-parse-engine/apply-pattern pattern-list line))
   projmake-default-parse-engine/err-line-patterns))

(provide 'projmake-default-parse-engine)
