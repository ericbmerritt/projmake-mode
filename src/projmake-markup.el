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
(require 'projmake-log)
(require 'projmake-util)
(require 'projmake-error)
(require 'projmake-project)
(require 'projmake-parse-engine)

(defvar compilation-error-regexp-alist-alist)

(defun projmake-markup/project-overlays (project)
  (let ((overlays nil))
    (projmake-project/do-for-project-buffers
     project
     (let ((buffer-overlays (overlays-in (point-min) (point-max))))
       (while buffer-overlays
         (let ((overlay (car buffer-overlays)))
           (when (projmake-markup/overlay-p overlay)
             (setq overlays (cons overlay overlays))))
         (setq buffer-overlays (cdr buffer-overlays)))))

    overlays))

(defun projmake-markup/delete-overlays (project)
  "Delete all overlays in the project"
  (when (fboundp 'ind-clear-indicators) (ind-clear-indicators))
  (dolist (ol (projmake-markup/project-overlays project))
    (when ol
      (delete-overlay ol))))

(defun projmake-markup/highlight-err-lines (project error-infos)
  "Highlight error lines in BUFFER using info from project-error-info"
  (save-excursion
    (dolist (err error-infos)
      (projmake-markup/highlight-line project err))))

(defun projmake-markup/relative-line (line-error)
  "Find the line number line relative to the current line"
  (+ 1 (- (projmake-error-line line-error)
          (line-number-at-pos (point)))))

(defun projmake-markup/beginning-char (relative-line line-error)
  (let ((line-beg (line-beginning-position relative-line))
        (start-char (projmake-error-char line-error)))
    (if (> start-char 0)
        (+ line-beg start-char)
      line-beg)))

(defun projmake-markup/end-char (relative-line line-error)
  (let ((end-char (projmake-error-end-char line-error)))
    (if (> end-char 0)
        (+ end-char (line-beginning-position relative-line))
      (line-end-position relative-line))))

(defun projmake-markup/highlight-line (project line-error)
  "Highlight line LINE-NO in current buffer.
Perhaps use text from line-error to enhance highlighting."
  (let ((buffer (projmake-util/buffer-for-error line-error project)))
    (when buffer
      (with-current-buffer buffer
        (let* ((relative-line (projmake-markup/relative-line line-error))
               (line-beg (line-beginning-position relative-line))
               (highlight-beg (projmake-markup/beginning-char
                               relative-line line-error))
               (highlight-end (projmake-markup/end-char
                               relative-line line-error))
               (face nil))

          (if (string-equal (projmake-error-type line-error) "e")
              (setq face 'projmake-errline)
            (setq face 'projmake-warnline))

          (projmake-markup/add-overlays line-beg
                                        highlight-beg
                                        highlight-end
                                        face
                                        nil))))))

(defun projmake-markup/util-attr (face str)
  "add some properties to a text string and return it"
  (put-text-property 0 (length str) 'face face str)
  str)

(defun projmake-markup/overlay-p (ov)
  "Determine whether overlay OV was created by projmake."
  (and (overlayp ov)
       (overlay-get ov 'projmake-overlay)))

(defun projmake-markup/add-overlay (beg end tooltip-text face mouse-face)
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
                   (projmake-markup/util-attr face
                                              (concat tooltip-text " "))))
    (projmake-log/debug "created an overlay at (%d-%d)" beg end)))

(defun projmake-markup/add-tool-tip-overlay (beg-of-line
                                             face
                                             mouse-face)
  (when (fboundp 'ind-create-indicator)
    (ind-create-indicator beg-of-line
                          :managed t
                          :relative nil
                          :fringe 'left-fringe
                          :bitmap 'exclamation-mark
                          :face 'fringe)))

(defun projmake-markup/add-highlight-overlay (beg
                                              end
                                              face
                                              mouse-face)
  (projmake-markup/add-overlay beg end nil
                               face mouse-face))

(defun projmake-markup/add-overlays (beg-of-line
                                     beg
                                     end
                                     face
                                     mouse-face)
  "Allocate a projmake overlay in range BEG and END."
  (projmake-markup/add-tool-tip-overlay beg-of-line face mouse-face)
  (projmake-markup/add-highlight-overlay beg end face mouse-face))

(provide 'projmake-markup)
