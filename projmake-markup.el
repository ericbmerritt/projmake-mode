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
    (delete-overlay ol))
  (setf (projmake-project-overlays nil)))

(defun projmake-highlight-err-lines (project)
  "Highlight error lines in BUFFER using info from project-error-info"
  (save-excursion
    (dolist (err (projmake-project-error-info project))
      (projmake-highlight-line project err))))

(defun projmake-highlight-line (project line-error)
  "Highlight line LINE-NO in current buffer.
Perhaps use text from line-error to enhance highlighting."
  (let ((buffer (get-file-buffer (projmake-error-info-file line-error))))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))

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

          (push
           (projmake-make-overlay beg end tooltip-text face nil)
           (projmake-project-overlays project)))))))

(defun projmake-util-attr (face str)
  "add some properties to a text string and return it"
  (put-text-property 0 (length str) 'face face str)
  str)

(defun projmake-region-has-projmake-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one projmake overlay, nil if no overlay."
  (let ((ovs (overlays-in beg end)))
    (find-first 'projmake-overlay-p ovs)))

(defun projmake-overlay-p (ov)
  "Determine whether overlay OV was created by projmake."
  (and (overlayp ov)
       (overlay-get ov 'projmake-overlay)))

(defun projmake-make-overlay (beg end tooltip-text face mouse-face)
  "Allocate a projmake overlay in range BEG and END."
  (when (not (projmake-region-has-projmake-overlays beg end))
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face face)
      (overlay-put ov 'mouse-face mouse-face)
      (overlay-put ov 'help-echo tooltip-text)
      (overlay-put ov 'projmake-overlay t)
      (overlay-put ov 'priority 100)
      (overlay-put ov 'after-string
                   (projmake-util-attr face (concat " <--- " tooltip-text)))
      (projmake-log PROJMAKE-DEBUG "created an overlay at (%d-%d)" beg end)
      ov)))

(provide 'projmake-markup)
