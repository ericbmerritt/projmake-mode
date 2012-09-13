;; -*- coding: utf-8; lexical-binding: t -*-
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
;; Error parsing and handlingf
(require 'projmake-util)
(require 'projmake-project)

(defstruct (projmake-error-info)
  file line type text)

(defun projmake-get-err-count (project type)
  "Return number of errors of specified TYPE for ERR-INFO-LIST."
  (let* ((error-info-list (projmake-project-error-info project))
         (count (length error-info-list))
         (err-count 0))
    (dolist (err error-info-list)
      (when (string-equal type (projmake-error-info-type err))
        (setq err-count (+ err-count 1))))
    err-count))

(defun projmake-parse-residual (project)
  "Parse residual if it's non empty."
  (let ((output-residual (projmake-project-residual project)))
    (if output-residual
        (setf (projmake-project-error-info project)
              (projmake-parse-err-lines
               (projmake-project-error-info project)
               (list output-residual)))
      (setf (projmake-project-residual project) nil))
    (setf (projmake-project-warning-count project)
          (projmake-get-err-count project "w"))
    (setf (projmake-project-error-count project)
          (projmake-get-err-count project "e"))))

(defun projmake-buffer-has-error (project buffer)
  (let* ((error-infos (projmake-project-error-info project))
         (buffer-file (buffer-file-name buffer)))
    (projmake-find-first #'(lambda (error-info)
                             (when (string-equal (projmake-error-info-file error-info)
                                                 buffer-file)
                               error-info)) error-infos)))

(defun projmake-parse-output-and-residual (project output)
  "Split OUTPUT into lines, merge in residual if necessary."
  (let* ((buffer-residual (projmake-project-residual project))
         (total-output (if buffer-residual (concat buffer-residual output) output))
         (lines-and-residual  (projmake-split-output total-output))
         (lines (nth 0 lines-and-residual))
         (new-residual  (nth 1 lines-and-residual)))
    (setf (projmake-project-residual project) new-residual)
    (setf (projmake-project-error-info project)
          (projmake-parse-err-lines
           (projmake-project-error-info project) lines))))

(defun projmake-split-output (output)
  "Split OUTPUT into lines.
Return last one as residual if it does not end with newline char.
Returns ((LINES) PESIDUAL)."
  (when (and output (> (length output) 0))
    (let* ((lines (split-string output "[\n\r]+"))
           (complete (equal "\n" (char-to-string (aref output (1- (length output))))))
           (residual nil))
      (when (not complete)
        (setf residual (car (last lines)))
        (setf lines (butlast lines)))
      (list lines residual))))

(defun projmake-parse-err-lines (err-info-list lines)
  "Parse err LINES, store info in ERR-INFO-LIST."
  (let ((acc err-info-list))
    (dolist (line lines acc)
      (let ((line-err-info (projmake-parse-line line)))
        (projmake-log PROJMAKE-DEBUG "parsed '%s', %s line-err-info"
                      line (if line-err-info "got" "no"))
        (when line-err-info
          (push line-err-info acc))))
    acc))

(defun projmake-reformat-err-line-patterns-from-compile-el (original-list)
  "Grab error line patterns from ORIGINAL-LIST in compile.el format.
Convert it to flymake internal format."
  (let (result)
    (dolist (item original-list result)
      (setq item (cdr item))
      (let ((regexp (nth 0 item))
            (file (nth 1 item))
            (line (nth 2 item))
            (col (nth 3 item)))
        (when (not (functionp line))
          (push (list
                 regexp
                 (if (consp file)
                     (car file)
                   file)
                 (if (consp line)
                     (car line)
                   line)
                 (if (consp col)
                   (car col)
                   col))
                result))))))

(defvar projmake-err-line-patterns
  (append
   '(
     ;; MS Visual C++ 6.0
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; jikes
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\|Semantic Error\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; MS midl
     ("midl[ ]*:[ ]*\\(command line error .*\\)"
      nil nil nil 1)
     ;; MS C#
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\),[0-9]+)\: \\(\\(error\\|warning\\|fatal error\\) \\(CS[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; perl
     ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
     ;; PHP
     ("\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)" 2 3 nil 1)
     ;; ant/javac
     (" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[ \t\n]*\\(.+\\)"
      2 4 nil 5))
   (projmake-reformat-err-line-patterns-from-compile-el compilation-error-regexp-alist-alist))
  "Patterns for matching error/warning lines.  Each pattern has the form
\(REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).
Use `projmake-reformat-err-line-patterns-from-compile-el' to add patterns
from compile.el")

(defun projmake-apply-pattern (pattern-list line)
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

          (projmake-log PROJMAKE-DEBUG
                        (concat  "parse line: file-idx=%s line-idx=%s file=%s "
                                 "line=%s text=%s")
                        file-idx line-idx
                        raw-file-name line-no err-text)
          (make-projmake-error-info :file raw-file-name
                                    :line line-no
                                    :type err-type
                                    :text err-text))
      nil)))

(defun projmake-parse-line (line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (projmake-find-first #'(lambda (pattern-list)
                           (projmake-apply-pattern pattern-list line))
                       projmake-err-line-patterns))

(defun projmake-patch-err-text (string)
  (if (string-match "^[\n\t :0-9]*\\(.*\\)$" string)
      (match-string 1 string)
    string))

(provide 'projmake-error-parsing)
