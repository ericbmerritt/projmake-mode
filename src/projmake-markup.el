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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay management
(require 'projmake-util)
(require 'projmake-project)
(require 'projmake-parse-engine)

(defvar compilation-error-regexp-alist-alist)

(defun projmake-delete-overlays (project)
  "Delete all overlays in the project"
  (dolist (ol (projmake-project-overlays project))
    (when ol
      (delete-overlay ol)))
  (setf (projmake-project-overlays project) nil))

(defun projmake-highlight-err-lines (project error-infos)
  "Highlight error lines in BUFFER using info from project-error-info"
  (save-excursion
    (dolist (err error-infos)
      (projmake-highlight-line project err))))

(defun projmake-markup--relative-line (line-error)
  "Find the line number line relative to the current line"
  (+ 1 (- (projmake-error-info-line line-error)
          (line-number-at-pos (point)))))

(defun projmake-markup--beginning-char (relative-line line-error)
  (let ((line-beg (line-beginning-position relative-line))
        (start-char (projmake-error-info-char line-error)))
    (if (> start-char 0)
        (+ line-beg start-char)
      line-beg)))

(defun projmake-markup--end-char (relative-line line-error)
  (let ((end-char (projmake-error-info-end-char line-error)))
    (if (> end-char 0)
        (+ end-char (line-beginning-position relative-line))
      (line-end-position relative-line))))

(defun projmake-highlight-line (project line-error)
  "Highlight line LINE-NO in current buffer.
Perhaps use text from line-error to enhance highlighting."
  (let* ((error-file (expand-file-name
                      (projmake-error-info-file line-error)
                      (projmake-project-dir project)))
         (buffer (get-file-buffer error-file)))
    (projmake-log PROJMAKE-DEBUG "Looking for buffer for %s" error-file)
    (when buffer
      (projmake-log PROJMAKE-DEBUG "Got buffer for %s" error-file)
      (with-current-buffer buffer
        (let* ((relative-line (projmake-markup--relative-line line-error))
               (line-beg (line-beginning-position relative-line))
               (highlight-beg (projmake-markup--beginning-char
                               relative-line line-error))
               (highlight-end (projmake-markup--end-char
                               relative-line line-error))
               (tooltip-text (projmake-error-info-text line-error))
               (face nil))

          (if (string-equal (projmake-error-info-type line-error) "e")
              (setq face 'projmake-errline)
            (setq face 'projmake-warnline))

          (projmake-markup--add-overlays  project
                                          line-beg
                                          highlight-beg
                                          highlight-end
                                          tooltip-text
                                          face
                                          nil))))))

(defun projmake-util-attr (face str)
  "add some properties to a text string and return it"
  (put-text-property 0 (length str) 'face face str)
  str)

(defun projmake-region-has-projmake-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one projmake overlay, nil if no overlay."
  (let ((ovs (overlays-in beg end)))
    (projmake-find-first 'projmake-overlay-p ovs)))

(defun projmake-overlay-p (ov)
  "Determine whether overlay OV was created by projmake."
  (and (overlayp ov)
       (overlay-get ov 'projmake-overlay)))

(defun projmake-markup--add-overlay
  (project beg end tooltip-text face mouse-face)
  "Allocate a projmake overlay in range BEG and END."
  (let ((ov (make-overlay beg end nil t t)))
    (overlay-put ov 'face face)
    (overlay-put ov 'mouse-face mouse-face)
    (overlay-put ov 'help-echo tooltip-text)
    (overlay-put ov 'projmake-overlay t)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'evaporate t)
    (when tooltip-text
      (overlay-put ov 'before-string
                   (projmake-util-attr face
                                       (concat tooltip-text ": \n"))))
    (projmake-log PROJMAKE-DEBUG "created an overlay at (%d-%d)" beg end)

    (push ov (projmake-project-overlays project))))

(defun projmake-markup--add-tool-tip-overlay (project
                                              beg-of-line
                                              tooltip-text
                                              face
                                              mouse-face)
  (let ((beg beg-of-line)
        (end (+ 1 beg-of-line)))
    (projmake-markup--add-overlay project beg end tooltip-text
                                  face mouse-face)))

(defun projmake-markup--add-highlight-overlay (project
                                               beg
                                               end
                                               face
                                               mouse-face)
  (projmake-markup--add-overlay project beg end nil
                                face mouse-face))

(defun projmake-markup--add-overlays (project
                                      beg-of-line
                                      beg
                                      end
                                      tooltip-text
                                      face
                                      mouse-face)
  "Allocate a projmake overlay in range BEG and END."
  (projmake-markup--add-tool-tip-overlay
   project beg-of-line tooltip-text face mouse-face)
  (projmake-markup--add-highlight-overlay
   project beg end face mouse-face))

(provide 'projmake-markup)
