;; -*- coding: utf-8; lexical-binding: t; fill-column: 80 -*-
;; Original code by Sebastian Wiesner <lunaryorn@gmail.com>
;; at https://flycheck.readthedocs.org
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Error list
(require 'dash)

(define-derived-mode projmake-elmm/error-list-mode
  tabulated-list-mode
  "Projmake errors"
  "Major mode for listing Projmake errors."
  (setq tabulated-list-format
        [("file" 25 nil)
        ("Line" 4 nil :right-align t)
        ("Col" 3 nil :right-align t)
        ("type" 7 nil)
        ("Message" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-entries #'projmake-elmm/error-list-entries)
  (tabulated-list-init-header))

(defun projmake-elmm/get-project-list-buffer-name (build-state)
  "Get the buffer name for the project"
  (let ((project (projmake-elmm/get-project build-state)))
    (concat "Projmake Error List ["
            (projmake-project-name project)
            "]")))

(defun projmake-elmm/get-project-list-buffer (build-state)
  "Get the list buffer for the project. There is only ever one"
  (let ((buffer (get-buffer-create
                  (projmake-elmm/get-project-list-buffer-name build-state))))
    (with-current-buffer buffer
      (projmake-elmm/error-list-mode))
    (when projmake-show-error-list-buffer
      (display-buffer buffer))
    buffer))

(defun projmake-elmm/get-project (build-state)
  (projmake-build-state-project build-state))

(defun projmake-elmm/remove-project-list-buffer (build-state)
  (let* ((buffer (projmake-elmm/get-project-list-buffer build-state)))
    (delete-window (get-buffer-window buffer))))

(defvar-local projmake-elmm/source-build-state nil
  "The currently displaying build state")
(put 'projmake-elmm/source-build-state 'permanent-local t)

(defun projmake-elmm/error-list-make-number-cell (number face)
  "Make a table cell for a NUMBER with FACE.

Convert NUMBER to string, fontify it with FACE and return the
string with attached text properties."
  (if (numberp number)
      (propertize (number-to-string number) 'font-lock-face face)
    ""))

(defvar projmake-file-name-elements-to-display-in-error-buffer)
(defun projmake-elmm/parse-file-name-for-display (name)
  (let* ((name-components (last (split-string name "/")
                                projmake-file-name-elements-to-display-in-error-buffer)))
    (mapconcat 'identity name-components "/")))

(defun projmake-elmm/error-list-make-entry (error)
  "Make a table cell for the given ERROR.

Return a list with the contents of the table cell."

  (let ((file (projmake-elmm/parse-file-name-for-display
               (projmake-error-file error)))
        (line (projmake-error-line error))
        (column (projmake-error-char error))
        (type (projmake-error/expand-type error))
        (message (or (projmake-error-text error) "Unknown error"))
        (face (projmake-error/get-face-for-error error)))
    (list error
          (vector (list file 'follow-link t
                        'projmake-error error
                        'font-lock-face face
                        'action #'projmake-elmm/goto-source)
                  (projmake-elmm/error-list-make-number-cell line face)
                  (projmake-elmm/error-list-make-number-cell column face)
                  (propertize type 'font-lock-face face)
                  message))))

(defun projmake-elmm/error-list-entries ()
  "Create the entries for the error list."
  (-map #'projmake-elmm/error-list-make-entry
        (projmake-build-state-error-info projmake-elmm/source-build-state)))

(defun projmake-elmm/goto-source (button)
  "Go to the source of the error associated to BUTTON."
  (let* ((error (button-get button 'projmake-error))
         (buffer (projmake-util/buffer-for-error error
                                                 (projmake-elmm/get-project
                                                  projmake-elmm/source-build-state))))
    (display-buffer buffer)
    (pop-to-buffer buffer)
    (let ((line (projmake-error-line error))
          (column (projmake-error-char error)))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column))))

(defvar projmake-elmm/mode-line-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for error list mode line.")


(defun projmake-elmm/refresh (build-state)
  "Refresh the current error list.

Add all errors currently reported for the current
`flycheck-error-list-source-buffer', and recenter the error
list."
  ;; We only refresh the error list, when it is visible in a window, and we
  ;; select this window while reverting, because Tabulated List mode attempts to
  ;; recenter the error at the old location, so it must have the proper window
  ;; selected.
  (with-current-buffer (projmake-elmm/get-project-list-buffer build-state)
    (setf projmake-elmm/source-build-state build-state)
    (revert-buffer)))

(provide 'projmake-elmm)
