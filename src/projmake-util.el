;; -*- coding: utf-8; lexical-binding: t; fill-column: 80 -*-
;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.
;; Copyright (C) 2012 Eric Merritt

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
(require 'cl-lib)

(defun projmake-find-first (function items)
  "Return the first non-nil value that function returns"
  (catch 'break
    (dolist (el items)
      (let ((result (funcall function el)))
        (when result
          (throw 'break result))))))

(defvar projmake-dir
  (file-name-directory load-file-name))

(defmacro projmake-util/do-for-buffers (&rest actions)
  `(dolist (buffer (buffer-list))
     (with-current-buffer buffer
       ,@actions)))

(provide 'projmake-util)
