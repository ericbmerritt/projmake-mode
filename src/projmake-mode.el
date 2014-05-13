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
(require 'cl-lib)

(require 'projmake-util)
(require 'projmake-project)
(require 'projmake-build-state)
(require 'projmake-markup)
(require 'projmake-extras)
(require 'projmake-banner)
(require 'projmake-parse-engine)
(require 'projmake-ocaml-parse-engine)

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
(defcustom projmake-log-level -1
  "Logging level, only messages with level lower or equal will be
logged. -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
  :group 'projmake
  :type 'integer)

;;;###autoload
(defcustom projmake-project-descs
  '(("Make" "Makefile" "nice -n5 make")
    ("Ocaml" "myocamlbuild.ml" "nice -n5 ocaml setup.ml -build"
     (projmake-ocaml-parse-engine-make))
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
  (projmake-toggle-off)
  (remove-hook 'after-save-hook 'projmake-buildable-event
               'local)) ;Only in the current buffer

(add-hook 'projmake-mode-hook 'projmake-buildable-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Management
(defvar projmake-projects nil
  "This is where all the active projects currently loaded are
stored")

(defun projmake-toggle-off ()
  "Cleanup building and toggle it off"
  (let ((project (projmake-project--find-by-buffer projmake-projects
                                                   (current-buffer))))
    (projmake-clear-project-output project))
  (setq projmake-toggled nil))

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

(defun projmake-make-process-name (project)
  (concat "Build Output ["
          (projmake-project-name project) ":"
          (number-to-string
           (projmake-project-build-counter project))
          "]"))

(defun projmake-build-buffer (project)
  (process-buffer (projmake-project-process project)))

(defun projmake-interrupt-existing-and-start-build (project)
  "If a build process is running interrupt it and start a new
one. If it does not exist then simple start a new one."
  (let ((process (projmake-project-process project)))
    (if (and process
             (eql 'run (process-status process)))
        (interrupt-process process))
    (projmake-start-build-process project)))

(defun projmake-buildable-event ()
  "buildable event occures (this is
almost always just a save). It will do the 'rigth thing' for the
build."
  (interactive)
  (let ((project
         (projmake-project--find-by-buffer projmake-projects (current-buffer))))
    (if project
        (projmake-interrupt-existing-and-start-build project)
      (projmake-log PROJMAKE-INFO
                    "No related project for buffer %s"
                    (buffer-file-name (current-buffer))))))

(defun projmake-start-build-process (project)
  "Start syntax check process."
  (let* ((build-state (projmake-build-state project))
         (process nil)
         (default-directory (projmake-project-dir project))
         (shell-cmd (projmake-project-shell project))
         (ctx-proc-filter (lambda (proc output)
                            (projmake-process-filter
                             build-state proc output)))
         (ctx-proc-sentinel (lambda (proc event)
                              (projmake-process-sentinel
                               build-state proc event)))
         (parse-state (projmake-project--parse-engine-init project)))
    (setf (projmake-build-state-parse-engine-state build-state) parse-state)
    (condition-case-unless-debug err
        (progn
          ;; Clean up the project markup up since we are building
          ;; again
          (projmake-clear-project-output project)
          (projmake-banner-building build-state)
          (projmake-log PROJMAKE-DEBUG
                        (concat "starting projmake process (%s) on dir %s "
                                "with parse engine %s")
                        shell-cmd default-directory
                        (projmake-project--parse-engine-name project))
          (setq process (apply 'start-process-shell-command
                               (projmake-make-process-name project)
                               (projmake-make-process-name project)
                               shell-cmd))
          (setf (projmake-project-process project) process)
          (set-process-sentinel process ctx-proc-sentinel)
          (set-process-filter process ctx-proc-filter)
          (push (cons (projmake-project-file project) process)
                projmake-processes)
          (incf (projmake-project-build-counter project))
          (projmake-log PROJMAKE-INFO
                        "started process %d, command=%s, dir=%s"
                        (process-id process)
                        (process-command process)
                        default-directory)
          process)
      (error
       (let* ((err-str
               (format
                "Failed to launch build '%s'
with args %s"
                (mapconcat 'identity shell-cmd " ")
                (error-message-string err))))
         (projmake-log PROJMAKE-ERROR err-str))))))

(defun projmake-process-filter (build-state process output)
  "Parse OUTPUT and highlight error lines.
It's flymake process filter."
  (projmake-banner-building build-state)
  (let ((source-buffer (process-buffer process)))
    (projmake-log PROJMAKE-DEBUG
                  "received %d byte(s) of output from process %d"
                  (length output)
                  (process-id process))
    (when (buffer-live-p source-buffer)
      (let ((error-infos
             (projmake-build-state-parse-output build-state output))
            (project-errors (projmake-build-state-error-info build-state)))
        (when error-infos
          (setf (projmake-build-state-error-info build-state)
                (append project-errors error-infos))
          (projmake-markup--highlight-err-lines
           (projmake-build-state-project build-state)
           error-infos)))
      (projmake-populate-process-buffer source-buffer  output))))

(defun projmake-populate-process-buffer (output-buffer string)
  (with-current-buffer output-buffer
      (save-excursion
        (setf buffer-read-only nil)
        ;; Insert the text, advancing the process marker.
        (goto-char (point-max))
        (insert string)
        (setf buffer-read-only t))))

(defun projmake-process-sentinel (build-state process event)
  "Sentinel for syntax check buffers."
  (when (memq (process-status process) '(signal exit))
    (let ((exit-status  (process-exit-status process))
          (source-buffer (process-buffer process)))

      (projmake-log PROJMAKE-INFO "Process received event %s for buffer %s"
                    event
                    (buffer-name))

      (when (string= "interrupt: 2\n" event)
        (setf (projmake-build-state-inturrupted build-state) t))

      (projmake-log PROJMAKE-INFO "process %d exited with code %d"
                    (process-id process) exit-status)

      (condition-case-unless-debug err
          (progn
            (delete-process process)
            (setf projmake-processes
                  (assq-delete-all (projmake-project-file
                                    (projmake-build-state-project build-state))
                                   projmake-processes))
            (projmake-markup--highlight-err-lines
             (projmake-build-state-project build-state)
             (projmake-build-state-parse-engine-stop build-state)))
        (error
         (let ((err-str
                (format "Error in process sentinel for buffer %s: %s"
                        source-buffer (error-message-string err))))
           (projmake-log PROJMAKE-ERROR err-str))))
      (projmake-post-build build-state exit-status))))

(defun projmake-clear-project-output (project)
  (projmake-banner-clear project)
  (projmake-markup--delete-overlays project)
  (projmake-util--do-for-buffers
   (let ((name (buffer-name (current-buffer))))
   (when (string-match "Build Output \\[\\w+:\\([0-9]+\\)\\]"
                       name)
     (let ((build (string-to-number (match-string 1 name))))
       (when (< build (projmake-project-build-counter project))
         (kill-buffer (current-buffer))))))))

(defun projmake-post-build (build-state exitcode)
  (setf (projmake-project-last-exitcode
         (projmake-build-state-project build-state))
        exitcode)
  (setf (projmake-build-state-exitcode build-state) exitcode)
  (projmake-log PROJMAKE-ERROR "exit code %d" exitcode)
  (projmake-banner-show build-state)
  (when (= 0 exitcode)
    (projmake-erase-build-buffer (projmake-build-state-project build-state)))

  (projmake-log PROJMAKE-ERROR "%s: %d error(s), %d warning(s)"
                (buffer-name)
                (projmake-build-state-error-count build-state)
                (projmake-build-state-warning-count build-state)))

(defun projmake-erase-build-buffer (project)
  (kill-buffer (projmake-build-buffer project)))

(provide 'projmake-mode)
