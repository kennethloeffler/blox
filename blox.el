;;; blox.el --- Interact with Roblox tooling -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kenneth Loeffler

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide functions to use Roblox tooling from Emacs.

;;; Code:

(defgroup blox nil
  "Customization for Roblox tooling."
  :prefix 'blox-
  :group 'tools)

(defgroup blox-executables nil
  "Names of executables to use."
  :group 'blox)

(defcustom blox-test-project "test.project.json"
  "The name of the project to use for `blox-test'."
  :type 'string
  :group 'blox)

(defcustom blox-test-script "TestRunner.server.lua"
  "The name of the script to use for `blox-test'."
  :type 'string
  :group 'blox)

(defcustom blox-rojo-executable "rojo"
  "The name of the executable to use when running Rojo."
  :type 'string
  :group 'blox-executables)

(defcustom blox-run-in-roblox-executable "run-in-roblox.exe"
  "The name of the executable to use when running run-in-roblox."
  :type 'string
  :group 'blox-executables)

(defun blox--save-some-lua-mode-buffers ()
  "Prompt to save any unsaved lua-mode buffers."
  (save-some-buffers nil (lambda ()
                           (eq major-mode 'lua-mode))))

(defun blox--echo-filter (proc string)
  "Write STRING to PROC's buffer and display it in the echo area."
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (setq buffer-read-only nil)
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc)))
      (setq buffer-read-only t))
    (message string)))

(defun blox--display-buffer-sentinel (buffer)
  "Return a process sentinel to call `display-buffer' on BUFFER after every event."
  (lambda (_process _event)
    (with-current-buffer buffer
      (help-mode)
      (display-buffer buffer))))

(defun blox--prompt-kill-if-running (process-name)
  "Kill the buffer named PROCESS-NAME if the process is still running."
  (if (and (get-process process-name)
           (eq (process-status process-name) 'run))
      (kill-buffer process-name)))

(defun blox--get-project-type (project-path)
  "Return the appropriate Roblox file extension for PROJECT-PATH."
  (with-temp-buffer
    (insert-file-contents project-path)
    (if (search-forward "DataModel" nil t)
        ".rbxlx"
      ".rbxmx")))

(defun blox-rojo-serve ()
  "Prompt for a project file for Rojo to start serving."
  (interactive)
  (blox--save-some-lua-mode-buffers)
  (blox--prompt-kill-if-running "*rojo-serve*")
  (let ((directory (or (vc-root-dir) default-directory)))
    (with-current-buffer (get-buffer-create "*rojo-serve*")
      (help-mode)
      (make-process
       :name "*rojo-serve*"
       :buffer (get-buffer "*rojo-serve*")
       :command
       (list blox-rojo-executable "serve"
             (read-file-name "Choose project: " directory ""))
       :filter 'blox--echo-filter))))

(defun blox-rojo-build (&optional force-project-path)
  "Prompt for a project file for Rojo to build and return its filename.
If FORCE-PROJECT-PATH is provided, build that project instead of
prompting."
  (interactive)
  (blox--save-some-lua-mode-buffers)
  (let* ((vc-or-default (or (vc-root-dir) default-directory))
         (previous-directory default-directory)
         (project-path (or force-project-path
                           (read-file-name "Choose project: "
                                           vc-or-default "")))
         (output (concat
                  (file-name-sans-extension (file-name-nondirectory
                                             project-path))
                  (blox--get-project-type project-path))))
    (cd (file-name-directory project-path))
    (make-process
     :name "*rojo-build*"
     :buffer (get-buffer-create "*rojo-build*")
     :command
     (list blox-rojo-executable "build"
           (file-name-nondirectory project-path)
           "--output" output))
    (cd previous-directory)
    output))

(defun blox-rojo-build-default ()
  "Build the first found default.project.json."
  (interactive)
  (blox-rojo-build
   (concat
    (locate-dominating-file default-directory "default.project.json")
    "default.project.json")))

(defun blox-run-in-roblox (script-path place-filename)
  "Run the Lua script at SCRIPT-PATH in PLACE-FILENAME with run-in-roblox.
Both SCRIPT-PATH and PLACE-FILENAME must be under the same directory."
  (blox--prompt-kill-if-running "*run-in-roblox*")
  (let ((previous-directory default-directory))
    (cd (file-name-directory script-path))
    (make-process
     :name "*run-in-roblox*"
     :buffer (get-buffer-create "*run-in-roblox*")
     :command
     (list blox-run-in-roblox-executable
           "--place" place-filename
           "--script" (file-name-nondirectory script-path))
     :sentinel (blox--display-buffer-sentinel
                (get-buffer "*run-in-roblox*")))
    (cd previous-directory)))

(defun blox-rojo-build-and-run ()
  "Prompt for a Rojo place and a script, build the place, then run the script."
  (interactive)
  (blox-run-in-roblox
   (read-file-name "Choose script: "
                   (or (vc-root-dir) default-directory) "")
   (blox-rojo-build)))

(defun blox-test ()
  "Run `blox-test-script' in `blox-test-project' with run-in-roblox.
The files are expected to be under the same directory."
  (interactive)
  (let ((directory (locate-dominating-file
                    default-directory
                    blox-test-script)))
    (if (not directory)
        (message "blox-test: Could not locate test script")
      (if (not (locate-file blox-test-project (list directory)))
          (message "blox-test: Could not locate test project")
        (blox-run-in-roblox
         (concat directory blox-test-script)
         (blox-rojo-build (concat directory
                                  blox-test-project)))))))

(provide 'blox)

;; Local variables:
;; indent-tabs-mode: nil
;; End:

;;; blox.el ends here
