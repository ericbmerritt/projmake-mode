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
(require 'projmake-util)

;;;###autoload
(defun projmake-search-load-project ()
  (interactive)
  (when (not (projmake-search-for-dominating))
    (projmake-find-add-manual-project)))

(defun projmake-search-for-dominating ()
  "Search for a useful set of dominating files "
  (catch 'break
    (dolist (proj-desc projmake-project-descs)
      (let ((result (apply 'projmake-find-dominating-file-make-project proj-desc)))
        (when result
          (throw 'break result))))))

(defun projmake-find-dominating-top (dirname filename)
  "Look for the topmost dominating file with the specified file name"
  (let* ((project-dir (locate-dominating-file dirname filename)))
    (if project-dir
        (let* ((parent-dir (file-name-directory (directory-file-name project-dir)))
               (top-project-dir (if (and parent-dir (not (string= parent-dir "/")))
                                    (projmake-find-dominating-top parent-dir filename)
                                  nil))
               (return-dir
                (if top-project-dir
                    top-project-dir
                  project-dir)))
          (projmake-log PROJMAKE-DEBUG "found project dir %s at " return-dir)

          return-dir)
      project-dir)))

(defun projmake-make-generic-prj (project-type project-dir dominating-name cmd)
  "Make a project out of the project dir and the dominating
name. Build the project with the provided command"
  (projmake-log PROJMAKE-DEBUG "Creating project at %s for a %s project to be built with command '%s'"
                project-dir project-type cmd)
  (let* ((filename (expand-file-name
                    (concat (file-name-as-directory project-dir) dominating-name)))
         (shell cmd)
         (prj (projmake-prj filename :shell shell)))
    (projmake-add-to-projects prj)
    prj))

(defun projmake-find-dominating-file-make-project (project-type dominating-name cmd)
  (interactive)
  (let* ((dirname (file-name-directory (buffer-file-name)))
         (project-dir (projmake-find-dominating-top dirname dominating-name)))
    (if project-dir
        (projmake-make-generic-prj project-type project-dir dominating-name cmd)
      nil)))

(defun projmake-find-add-manual-project ()
  "Searches the directory tree from the current active buffer up
through the filesystem root looking for the project file
specified by projmake-project-file-name, usually 'projmake'. When
it finds that file it loades it using projmake-add-project"
  (projmake-log PROJMAKE-DEBUG "Searching for generic %s project to add"
                projmake-project-file-name)
  (interactive)
  (let* ((dirname (file-name-directory (buffer-file-name)))
         (project-dir (locate-dominating-file dirname projmake-project-file-name)))
    (if project-dir
        (projmake-add-project (concat project-dir projmake-project-file-name))
      nil)))

(defun projmake-eval-project-file (file)
  "Load the user supplied project and eval it as a way to create
the project struct. This gives the project definer a lot of
freedom in defining a project though there is some danger
involved."
  (with-temp-buffer
    (buffer-disable-undo)
    (insert-file-contents file)
    (goto-char (point-min))
    (eval `(labels
               ((projmake
                 (&rest args)
                 (apply (function projmake-prj) ,file args)))
             ,(read (current-buffer))))))

(defun projmake-toggle-kill-build-buffer ()
  "Toggle killing of the build buffer after a build"
  (interactive)
  (setf projmake-kill-build-buffer (not projmake-kill-build-buffer))
  (projmake-buildable-event))

(provide 'projmake-extras)
