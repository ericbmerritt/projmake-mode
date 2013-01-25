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
;; Overlay management

(require 'projmake-util)
(require 'projmake-project)
(require 'projmake-error-parsing)


(defalias 'projmake-line-beginning-position
  (if (fboundp 'line-beginning-position)
      'line-beginning-position
    (lambda (&optional arg) (save-excursion (beginning-of-line arg) (point)))))

(defalias 'projmake-line-end-position
  (if (fboundp 'line-end-position)
      'line-end-position
    (lambda (&optional arg) (save-excursion (end-of-line arg) (point)))))

(defun projmake-delete-overlays (project)
  "Delete all overlays in the project"
  (dolist (ol (projmake-project-overlays project))
    (when ol
      (delete-overlay ol)))
  (setf (projmake-project-overlays project) nil))

(defun projmake-highlight-err-lines (project)
  "Highlight error lines in BUFFER using info from project-error-info"
  (save-excursion
    (dolist (err (projmake-project-error-info project))
      (projmake-highlight-line project err))))

(defun projmake-highlight-line (project line-error)
  "Highlight line LINE-NO in current buffer.
Perhaps use text from line-error to enhance highlighting."
  (let* ((error-file (expand-file-name
                      (projmake-error-info-file line-error)
                      (projmake-project-dir project)))
         (error-text (projmake-error-info-text line-error))
         (buffer (get-file-buffer error-file)))
    (projmake-log PROJMAKE-DEBUG "Looking for buffer for %s" error-file)
    (when buffer
      (projmake-log PROJMAKE-DEBUG "Got buffer for %s" error-file)
      (with-current-buffer buffer
        (goto-line (projmake-error-info-line line-error))
        (let* ((line-beg (projmake-line-beginning-position))
               (line-end (projmake-line-end-position))
               (tooltip-text (projmake-error-info-text line-error))
               (face nil))

          (goto-char line-beg)
          (while (looking-at "[ \t]")
            (forward-char))

          (if (string-equal (projmake-error-info-type line-error) "e")
              (setq face 'projmake-errline)
            (setq face 'projmake-warnline))

          (projmake-add-overlay project
                                line-beg line-end tooltip-text face nil))))))

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

(defun projmake-add-overlay (project beg end tooltip-text face mouse-face)
  "Allocate a projmake overlay in range BEG and END."
  (when (not (projmake-region-has-projmake-overlays beg end))
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face face)
      (overlay-put ov 'mouse-face mouse-face)
      (overlay-put ov 'help-echo tooltip-text)
      (overlay-put ov 'projmake-overlay t)
      (overlay-put ov 'priority 100)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'before-string
                   (projmake-util-attr face (concat tooltip-text ": \n")))
      (projmake-log PROJMAKE-DEBUG "created an overlay at (%d-%d)" beg end)

      (push ov (projmake-project-overlays project)))))

(provide 'projmake-markup)
