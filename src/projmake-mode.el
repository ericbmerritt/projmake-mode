;; -*- coding: utf-8; lexical-binding: t; fill-column: 80 -*-
;; Copyright (C) 2007-2008 Vesa Karvonen
;; Copyright (C) 2012 Eric Merritt
;;
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

(with-no-warnings
  (require 'cl))

(require 'compile)
(require 'projmake-util)
(require 'projmake-project)
(require 'projmake-error-parsing)
(require 'projmake-markup)
(require 'projmake-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom vars controlling projmake behaviour
;;;###autoload
(defface projmake-errline
  '((((class color) (background dark))
     (:background "Firebrick4" :italic))
    (((class color) (background light))
     (:background "LightPink" :italic))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'projmake)

;;;###autoload
(defface projmake-warnline
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'projmake)

;;;###autoload
(defface projmake-notify-err
  '((t (:foreground "red"
                    :weight bold
                    :background "white")))
  "Face used for the header line notification on error."
  :group 'projmake)

;;;###autoload
(defface projmake-notify-normal
  '((t (:foreground "black"
                    :weight bold
                    :background "white")))
  "Face used for header line notification on normal notifications."
  :group 'projmake)

;;;###autoload
(defcustom projmake-project-file-name "projmake"
  "The name of the 'projmake' description file that indicates the root
of each project"
  :group 'projmake
  :type 'string)

;;;###autoload
(defcustom projmake-build-now! t
  "Indicates whether the current build (if one exists) is killed and
  restarted when ever a relevant build event occurs. If t the current
  running build is killed and a new build started. If nil a marker is
  set on the project such that when the build terminates naturally a
  new build is started immediately"
  :group 'projmake
  :type 'boolean)

;;;###autoload
(defcustom projmake-log-level -1
  "Logging level, only messages with level lower or equal will be
logged. -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
  :group 'projmake
  :type 'integer)

;;;###autoload
(defcustom projmake-project-descs
  '(("Make" "Makefile" "nice -n5 make")
    ("Rebar" "rebar.config" "nice -n5 rebar skip_deps=true compile"))
  "These are the default names + dominating files + commands needed to
automatically search for the project root and build system style"
  :group 'projmake
  :type '(alist :value-type (string string string)))

;;;###autoload
(defcustom projmake-switch-to-buffer-with-error t
  "Some build systems stop building at the first file that errors. So
it could be that even if there are errors in the project the user
doesn't know it because the buffer that is active has no errors. If
`projmake-switch-to-buffer-with-error' is t then the system checks to
see if there are errors in the current buffer, if so nothing happens,
but if there are no errors then the first buffer with errors is made
active."
  :group 'projmake
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Adjustment
;;;###autoload

;; buffer local variable defined in minor mode declaration
(defvar projmake-toggled)

(define-minor-mode projmake-mode
  "Toggle projmake-mode

   enables or disables the mode"

  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Projmake"
  ;; The minor mode bindings.
  '(([C-backspace] . projmake-toggle))
  :group 'projmake
  (make-local-variable 'projmake-toggled)
  (projmake-on))

;;;###autoload
(defun projmake-toggle ()
  "Togle projmake on or off depending on what projmake-toggle is set
to"
  (interactive)
  (if projmake-toggled
      (projmake-off)
    (projmake-on)))

;;;###autoload
(defun projmake-on ()
  "Turn projmake building on for a buffer"
  (interactive)
  (setq projmake-toggled t)
  (projmake-buildable-event)
  (add-hook 'after-save-hook 'projmake-buildable-event
            t 'local)) ;Only in the current buffer

;;;###autoload
(defun projmake-off ()
  "Turn projmake off "
  (interactive)
  (let ((project (projmake-find-project-by-buffer (current-buffer))))
    (projmake-clear-project-output project))

  (setq projmake-toggled nil)
  (remove-hook 'after-save-hook 'projmake-buildable-event
               'local)) ;Only in the current buffer

(add-hook 'projmake-mode-hook 'projmake-buildable-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Management
(defvar projmake-projects nil
  "This is where all the active projects currently loaded are
stored")

(defun projmake-clear-projects ()
  "Clears projects out of the projects variable. This is useful when
you want to reload a project. After this you need to rerun the
projmake setup for your file. Either restart the mode or open a file
of that mode"
  (interactive)
  (setf projmake-projects nil))

(defun projmake-add-to-projects (prj)
  "Adds a project to the project list only if that project does not
already exist. Where existance is indicated by the full path of the
projmake file."
  (let ((file (projmake-project-file prj)))
    (if (assoc file projmake-projects)
        prj
      (progn
        (push (cons file prj) projmake-projects)
        prj))))

(defun projmake-find-project-by-file (file)
  "This searches the list of projects search for the project
associated with this buffer. The buffer related projet is defined
as anything under the directory where the project configuration
file exists."
  (projmake-log PROJMAKE-DEBUG
                "Looking for project for file: %s" file)
  ;;we do this to ignore cl warnings about find-if. Wish we could turn
  ;;off that globally
  (with-no-warnings
    (cdr (find-if (lambda (prj-kv)
                    (let* ((prj (cdr prj-kv)))
                      (projmake-is-file-part-of-project prj file)))
                  projmake-projects))))

(defun projmake-find-project-by-buffer (buffer)
  "Grab the filename from the buffer and use
projmake-find-project-by-file to find the related project."
  (projmake-find-project-by-file (buffer-file-name buffer)))

(defun projmake-is-file-part-of-project (prj file)
  "Given a project and a file tests to see if the file belongs to the
project"
  (let ((projmake-dir (projmake-project-dir prj)))
    (eql t (compare-strings projmake-dir
                            0 nil
                            file 0 (length projmake-dir)))))

(defun projmake-is-buffer-part-of-project (prj buffer)
  "Given a project and a buffer tests to see if the file belongs to
the project"
  (if (buffer-file-name buffer)
      (projmake-is-file-part-of-project prj (buffer-file-name buffer))
    nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Active Projects

(defun projmake-add-project (&optional file)
  "Adds a project file to bg-build minor mode.  This basically reads
and evaluates the first Emacs Lisp expression from specified file.
The expression should evaluate to a bg-build project object."
  (interactive "fProjmake File:")
  (projmake-log PROJMAKE-DEBUG "loading file %s" file)
  (let ((absfile (expand-file-name file)))
    (cond
     ((not absfile)
      (projmake-search-load-project))
     ((not (and (file-readable-p absfile)
                (file-regular-p absfile)))
      (error "Specified file is not a regular readable file"))
     (t
      (let ((prj (projmake-eval-project-file absfile)))
        (projmake-add-to-projects prj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running Builds
(defvar projmake-processes nil
  "This is a association list of all currently running build
processes where the key is the project file and the value is the
process itself.")

(defun projmake-get-process (project)
  (let ((file (projmake-project-file project)))
    (cdr (assoc file projmake-processes))))

(defun projmake-make-process-name (project)
  (concat "projmake-build["
          (projmake-project-name project) ":"
          (number-to-string
           (projmake-project-build-counter project))
          "]"))

(defun interrupt-existing-and-start-build (process project)
  "If a build process is running interrupt it and start a new
one. If it does not exist then simple start a new one."
  (when (and process
             (eql 'run (process-status process)))
    (setf (projmake-project-inturrupted project) t)
    (interrupt-process process))
  (projmake-start-build-process project))

(defun projmake-build-when (project)
  "Kick off the project in the correct way. If the
projmake-build-now! variable interrupt the current build and kick
off a new build otherwise just mark for rebuild."
  (let ((process (projmake-get-process project)))
    (if projmake-build-now!
        (interrupt-existing-and-start-build process project)
      (if process
          (setf (projmake-project-build-again? project) t)
        (projmake-start-build-process project)))))

(defun projmake-buildable-event ()
  "buildable event occures (this is
almost always just a save). It will do the 'rigth thing' for the
build."
  (interactive)
  (let ((project
         (projmake-find-project-by-buffer (current-buffer))))
    (if project
        (if (projmake-project-build? project)
            (projmake-build-when project)
          (projmake-log PROJMAKE-INFO
                        "No related project for buffer %s"
                        (buffer-file-name (current-buffer))))
      (projmake-log PROJMAKE-INFO "No related project for buffer %s"
                    (buffer-file-name (current-buffer))))))

(defun projmake-start-build-process (project)
  "Start syntax check process."
  (let* ((process nil)
         (default-directory (projmake-project-dir project))
         (shell-cmd (projmake-project-shell project))
         (ctx-proc-filter (lambda (proc output)
                            (projmake-process-filter
                             project proc output)))
         (ctx-proc-sentinel (lambda (proc)
                              (projmake-process-sentinel
                               project proc))))
    (condition-case err
        (progn
          (projmake-clear-project-output project)
          ;; Clean up the project markup up since we are building
          ;; again
          (projmake-notify project "BUILDING ....")
          (projmake-log PROJMAKE-DEBUG
                        "starting projmake process (%s) on dir %s"
                        shell-cmd default-directory)
          (setq process (apply 'start-process-shell-command
                               (projmake-make-process-name project)
                               (projmake-make-process-name project)
                               shell-cmd))
          (set-process-sentinel process ctx-proc-sentinel)
          (set-process-filter process ctx-proc-filter)
          (push (cons (projmake-project-file project) process)
                projmake-processes)

          (incf (projmake-project-build-counter project))
          (setf (projmake-project-is-building? project) t)

          (projmake-log PROJMAKE-INFO
                        "started process %d, command=%s, dir=%s"
                        (process-id process) (process-command process)
                        default-directory)
          process)
      (error
       (let* ((err-str
               (format
                "Failed to launch syntax check process '%s'
with args %s"
                (mapconcat 'identity shell-cmd " ")
                (error-message-string err))))
         (projmake-log PROJMAKE-ERROR err-str))))))

(defun projmake-process-filter (project process output)
  "Parse OUTPUT and highlight error lines.
It's flymake process filter."
  (let ((source-buffer (process-buffer process)))
    (projmake-log PROJMAKE-DEBUG
                  "received %d byte(s) of output from process %d"
                  (length output)
                  (process-id process))
    (when (buffer-live-p source-buffer)
      (projmake-parse-output-and-residual project output))
    (projmake-populate-process-buffer project output)))

(defun projmake-populate-process-buffer (project string)
  (let ((output-buffer (get-buffer-create
                        (projmake-build-buffer-name project))))
    (with-current-buffer output-buffer
      (save-excursion
        (setf buffer-read-only nil)
        ;; Insert the text, advancing the process marker.
        (goto-char (point-max))
        (insert string)
        (setf buffer-read-only t)))))

(defun projmake-process-sentinel (project process)
  "Sentinel for syntax check buffers."
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status  (process-exit-status process))
           (source-buffer (process-buffer process)))

      (projmake-log PROJMAKE-INFO "process %d exited with code %d"
                    (process-id process) exit-status)

      (condition-case err
          (progn
            (delete-process process)
            (setf projmake-processes
                  (assq-delete-all (projmake-project-file project)
                                   projmake-processes))
            (projmake-parse-residual project))
        (error
         (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                                source-buffer (error-message-string err))))
           (projmake-log PROJMAKE-ERROR err-str))))

      (kill-buffer (process-buffer process))
      (projmake-post-build exit-status project))))

(defun projmake-notify-failed (exitcode project)
  ;; Add warning to the top of the file
  (projmake-notify project
                   (format "R:%d E:%d W:%d BUILD FAILED IN THIS PROJECT"
                           exitcode
                           (projmake-project-error-count project)
                           (projmake-project-warning-count project))
                   t))

(defun projmake-notify (project detail &optional error)
  ;; Add warning to the top of the file
  (projmake-do-for-project-buffers project
                                   (if error
                                       (projmake-update-header detail t)
                                     (projmake-update-header detail))))


(defun projmake-unnotify (project)
  (projmake-do-for-project-buffers project
                                   (setf header-line-format nil)))

(defun projmake-clear-project-output (project)
  (projmake-unnotify project)
  (projmake-delete-overlays project)
  (setf (projmake-project-error-info project) nil)
  (projmake-erase-build-buffer project))

(defun projmake-post-build (exitcode project)
  (projmake-delete-overlays project)
  (projmake-log PROJMAKE-ERROR "exit code %d" exitcode)
  (cond
   ((projmake-project-inturrupted project)
    (setf (projmake-project-inturrupted project) nil))
   ((and (not (= 0 exitcode))
         (not (projmake-project-inturrupted project)))
    (progn
      (projmake-notify-failed exitcode project)
      (projmake-highlight-err-lines project)))
   (t
    (progn
      (projmake-notify project nil)
      (projmake-erase-build-buffer project))))

  (projmake-cleanup-transient-project-data project)

  (projmake-log PROJMAKE-ERROR "%s: %d error(s), %d warning(s)"
                (buffer-name)
                (projmake-project-error-count project)
                (projmake-project-warning-count project))

  (when (projmake-project-build-again? project)
    (setf (projmake-project-build-again? project) nil)
    (projmake-build-when project)))

(defun projmake-build-buffer-name (project)
  (let ((project-name (projmake-project-name project)))
    (concat "Build Output [" project-name "]")))

(defun projmake-erase-build-buffer (project)
  (let ((build-buffer (get-buffer (projmake-build-buffer-name project))))
    (when (buffer-live-p build-buffer)
      (with-current-buffer build-buffer
        (save-excursion
          (setf buffer-read-only nil)
          (erase-buffer)
          (setf buffer-read-only t))))))


(provide 'projmake-mode)
