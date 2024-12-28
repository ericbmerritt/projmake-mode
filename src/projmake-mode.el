;;; projmake-mode.el --- run build system for project -*- coding: utf-8; lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2007-2008 Vesa Karvonen
;; Copyright (C) 2012 Eric Merritt

;; Package-Requires: ((dash "20150611.922") (indicators "20130217.1405"))

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

;;; Code:

(require 'cl-lib)

(require 'projmake-util)
(require 'projmake-log)
(require 'projmake-project)
(require 'projmake-build-state)
(require 'projmake-elmm)
(require 'projmake-markup)
(require 'projmake-discover)
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
(defcustom projmake-log/level -1
  "Logging level, only messages with level lower or equal will be
logged. -1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
  :group 'projmake
  :type 'integer)

;;;###autoload
(defcustom projmake-project-descs
  '(("Make" "Makefile" "nice -n5 make")
    ("Ocaml" "myocamlbuild.ml" "nice -n5 ocaml setup.ml -build"
     (projmake-ocaml-parse-engine/make))
    ("Rebar" "rebar.config" "nice -n5 rebar skip_deps=true compile"))
  "These are the default names + dominating files + commands needed to
automatically search for the project root and build system style"
  :group 'projmake
  :type '(alist :value-type (string string string)))

;;;###autoload
(defcustom projmake-show-build-output-buffer nil
  "Usually you don't need to see the build output buffer, but sometimes you need
to . This lets you control displaying that buffer (though it still exists)"
  :group 'projmake
  :type 'boolean)

;;;###autoload
(defcustom projmake-show-error-list-buffer nil
  "Usually its nice to see the error-list buffer, but sometimes it gets
annoying. This lets you disable displaying that buffer (though it still exists)
if you don't want to see it.."
  :group 'projmake
  :type 'boolean)

;;;###autoload
(defcustom projmake-file-name-elements-to-display-in-error-buffer 2
  "The number of file name elements to display in the error buffer. "
  :group 'projmake
  :type 'int)

;;;###autoload
(defcustom projmake-highlight-error-lines t
  "Indicates whether projmake mode should highlight error lines and add them to
the fringe. Sometimes this conflicts with other highlighters like merlin"
  :group 'projmake
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Adjustment
;;;###autoload

;; buffer local variable defined in minor mode declaration
(defvar projmake-mode/toggled)

(define-minor-mode projmake-mode
  "Toggle projmake-mode

   enables or disables the mode"

  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Projmake"
  ;; The minor mode bindings.
  '(([C-backspace] . projmake-mode/toggle))
  :group 'projmake
  (make-local-variable 'projmake-mode/toggled)
  (projmake-mode/on))

;;;###autoload
(defun projmake-mode/toggle ()
  "Togle projmake on or off depending on what projmake-toggle is set
to"
  (interactive)
  (if projmake-mode/toggled
      (projmake-mode/off)
    (projmake-mode/on)))

;;;###autoload
(defun projmake-mode/on ()
  "Turn projmake building on for a buffer"
  (interactive)
  (setq projmake-mode/toggled t)
  (projmake-mode/buildable-event)
  (add-hook 'after-save-hook 'projmake-mode/buildable-event
            t 'local)) ;Only in the current buffer

;;;###autoload
(defun projmake-mode/off ()
  "Turn projmake off "
  (interactive)
  (projmake-mode/toggle-off)
  (remove-hook 'after-save-hook 'projmake-mode/buildable-event
               'local)) ;Only in the current buffer

(add-hook 'projmake-mode-hook 'projmake-mode/buildable-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Management
(defvar projmake-mode/projects nil
  "This is where all the active projects currently loaded are
stored")

(defun projmake-mode/toggle-off ()
  "Cleanup building and toggle it off"
  (let ((project (projmake-project/find-by-buffer projmake-mode/projects
                                                  (current-buffer))))
    (projmake-mode/clear-project-output project))
  (setq projmake-mode/toggled nil))

(defun projmake-mode/clear-projects ()
  "Clears projects out of the projects variable. This is useful when
you want to reload a project. After this you need to rerun the
projmake setup for your file. Either restart the mode or open a file
of that mode"
  (interactive)
  (setf projmake-mode/projects nil))

(defun projmake-mode/add-to-projects (prj)
  "Adds a project to the project list only if that project does not
already exist. Where existance is indicated by the full path of the
projmake file."
  (let ((file (projmake-project-file prj)))
    (if (assoc file projmake-mode/projects)
        prj
      (progn
        (push (cons file prj) projmake-mode/projects)
        prj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build buffer

(defun projmake-mode/make-build-buffer-name (project)
  (format "%s Build Output"
          (projmake-project-name project)))

(defun projmake-mode/build-buffer (project)
  (get-buffer-create (projmake-mode/make-build-buffer-name project)))

(defun projmake-mode/buffer-clear (project)
  (with-current-buffer (projmake-mode/build-buffer project)
    (save-excursion
      (setf buffer-read-only nil)
      ;; Insert the text, advancing the process marker.
      (erase-buffer)
      (setf buffer-read-only t))))

(defun projmake-mode/buffer-write (project output)
  (with-current-buffer (projmake-mode/build-buffer project)
    (save-excursion
      (setf buffer-read-only nil)
      ;; Insert the text, advancing the process marker.
      (goto-char (point-max))
      (insert output)
      (setf buffer-read-only t))))

(defun projmake-mode/buffer-show-latest (project)
  (let* ((build-buffer (projmake-mode/build-buffer project))
        (window-buffer (get-buffer-window build-buffer)))
    (when projmake-show-build-output-buffer build-buffer
          (display-buffer build-buffer)
          (when window-buffer
            (with-selected-window window-buffer)
            (goto-char (point-max))))))

(defun projmake-mode/buffer-kill (project)
  (let ((build-buffer (projmake-mode/build-buffer project)))
    (kill-buffer build-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Active Projects

(defun projmake-mode/add-project (&optional file)
  "Adds a project file to bg-build minor mode.  This basically reads
and evaluates the first Emacs Lisp expression from specified file.
The expression should evaluate to a bg-build project object."
  (interactive "fProjmake File:")
  (projmake-log/debug "loading file %s" file)
  (let ((absfile (expand-file-name file)))
    (cond
     ((not absfile)
      (projmake-discover/search-load-project))
     ((not (and (file-readable-p absfile)
                (file-regular-p absfile)))
      (error "Specified file is not a regular readable file"))
     (t
      (let ((prj (projmake-discover/eval-project-file absfile)))
        (projmake-mode/add-to-projects prj))))))

(defun projmake-mode/make-process-name (project)
  (format "%s Build Process [%d]"
          (projmake-project-name project)
           (projmake-project-build-counter project)))

(defun projmake-mode/interrupt-existing-and-start-build (project)
  "If a build process is running interrupt it and start a new
one. If it does not exist then simple start a new one."
  (let ((process (projmake-project-process project)))
    (if (and process
             (eql 'run (process-status process)))
        (interrupt-process process))
    (projmake-mode/start-build-process project)))

(defun projmake-mode/buildable-event ()
  "buildable event occures (this is
almost always just a save). It will do the 'rigth thing' for the
build."
  (interactive)
  (let ((project
         (projmake-project/find-by-buffer projmake-mode/projects
                                          (current-buffer))))
    (if project
        (projmake-mode/interrupt-existing-and-start-build project)
      (projmake-log/info
       "No related project for buffer %s"
       (buffer-file-name (current-buffer))))))

(defun projmake-mode/start-build-process (project)
  "Start syntax check process."
  (let* ((build-state (projmake-build-state/make project))
         (process nil)
         (default-directory (projmake-project-dir project))
         (shell-cmd (projmake-project-shell project))
         (ctx-proc-filter (lambda (proc output)
                            (projmake-mode/process-filter
                             project build-state proc output)))
         (ctx-proc-sentinel (lambda (proc event)
                              (projmake-mode/process-sentinel
                               build-state proc event)))
         (parse-state (projmake-project/parse-engine-init project)))
    (setf (projmake-build-state-parse-engine-state build-state) parse-state)
    (projmake-elmm/refresh build-state)
    (condition-case-unless-debug err
        (progn
          ;; Clean up the project markup up since we are building
          ;; again
          (projmake-mode/clear-project-output project)
          (projmake-banner/building build-state)
          (projmake-log/debug
           (concat "starting projmake process (%s) on dir %s "
                   "with parse engine %s")
           shell-cmd default-directory
           (projmake-project/parse-engine-name project))

          (let ((process-name (projmake-mode/make-process-name project)))
            (setq process (apply 'start-process-shell-command
                                 process-name
                                 process-name
                                 shell-cmd)))

          (setf (projmake-project-process project) process)
          (set-process-sentinel process ctx-proc-sentinel)
          (set-process-filter process ctx-proc-filter)
          (cl-incf (projmake-project-build-counter project))
          (projmake-log/info "started process %d, command=%s, dir=%s"
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
         (projmake-log/error err-str))))))

(defun projmake-mode/process-filter (project build-state process output)
  "Parse OUTPUT and highlight error lines.
It's flymake process filter."
  (projmake-banner/building build-state)
  (let ((source-buffer (process-buffer process)))

    (projmake-log/debug "received %d byte(s) of output from process %d"
                        (length output)
                        (process-id process))

    (let ((these-errors (projmake-build-state/parse-output build-state output)))
      (when (and these-errors projmake-highlight-error-lines)
        (projmake-markup/highlight-err-lines
         (projmake-build-state-project build-state)
         these-errors)
        (projmake-elmm/refresh build-state)))
    (projmake-mode/populate-process-buffer project output)))

(defun projmake-mode/populate-process-buffer (project output)
  (projmake-mode/buffer-write project output)
  (projmake-mode/buffer-show-latest project))

(defun projmake-mode/process-sentinel (build-state process event)
  "Sentinel for syntax check buffers."
  (when (memq (process-status process) '(signal exit))
    (let ((exit-status  (process-exit-status process))
          (source-buffer (process-buffer process)))

      (projmake-log/info "Process received event %s for buffer %s"
                         event
                         (buffer-name))

      (when (string= "interrupt: 2\n" event)
        (setf (projmake-build-state-inturrupted build-state) t))

      (projmake-log/info "process %d exited with code %d"
                         (process-id process) exit-status)

      (condition-case-unless-debug err
          (progn
            (delete-process process)
            (setf (projmake-project-process
                   (projmake-build-state-project build-state)) nil)
            (when projmake-highlight-error-lines
              (projmake-markup/highlight-err-lines
               (projmake-build-state-project build-state)
               (projmake-build-state/parse-engine-stop build-state)))
            (projmake-elmm/refresh build-state))
        (error
         (let ((err-str
                (format "Error in process sentinel for buffer %s: %s"
                        source-buffer (error-message-string err))))
           (projmake-log/error err-str))))
      (kill-buffer (process-buffer process))
      (projmake-mode/post-build build-state exit-status))))

(defun projmake-mode/clear-project-output (project)
  (projmake-banner/clear project)
  (when projmake-highlight-error-lines (projmake-markup/delete-overlays project))
  (projmake-mode/buffer-clear project))

(defun projmake-mode/post-build (build-state exitcode)
  (setf (projmake-project-last-exitcode
         (projmake-build-state-project build-state))
        exitcode)
  (setf (projmake-build-state-exitcode build-state) exitcode)
  (projmake-log/error "exit code %d" exitcode)
  (projmake-banner/show build-state)
  (when (= 0 exitcode)
    (let ((project (projmake-build-state-project build-state)))
      (projmake-elmm/remove-project-list-buffer build-state)
      (projmake-mode/buffer-kill project))))

(defun projmake-mode/erase-build-buffer (project)
  (let ((project-name (projmake-project-name project)))
    (projmake-util/do-for-buffers
     (let ((name (buffer-name (current-buffer))))
       (when (string-match "Build Output \\[\\([^:]+\\):\\([0-9]+\\)\\]"
                           name)
         (let ((proj-name (match-string 1 name)))
           (when (string= project-name proj-name)
             (kill-buffer (current-buffer)))))))))

(provide 'projmake-mode)
