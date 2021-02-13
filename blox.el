;;; blox.el --- Interaction with Roblox tooling -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kenneth Loeffler

;; Author: Kenneth Loeffler <kenneth.loeffler@outlook.com>
;; Version: 0.0.0
;; Keywords: roblox, rojo, tools
;; URL: https://github.com/kennethloeffler/blox
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

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

;; blox.el provides functions to run Roblox tooling (such as Rojo)
;; from Emacs.

;;; Code:

(defgroup blox nil
  "Customization for Roblox tooling."
  :prefix 'blox-
  :group 'tools)

(defgroup blox-executables nil
  "Names of executable files."
  :group 'blox)

(defcustom blox-test-project "test.project.json"
  "The name of the project file to use for `blox-test'."
  :type 'string
  :group 'blox)

(defcustom blox-test-script "TestRunner.server.lua"
  "The name of the Lua script file to use for `blox-test'."
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
  "Prompt to save any unsaved `lua-mode' buffers."
  (save-some-buffers nil (lambda ()
                           (eq major-mode 'lua-mode))))

(defun blox--echo (message-string command-name)
  "Display MESSAGE-STRING formatted with COMMAND-NAME in the echo area."
  (message (format "[%s]: %s" command-name message-string)))

(defun blox--rojo-serve-echo-filter (proc string)
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
    (blox--echo string "blox-rojo-serve")))

(defun blox--display-buffer-sentinel (buffer)
  "Return a process sentinel to call `display-buffer' on BUFFER after every event."
  (lambda (_process _event)
    (with-current-buffer buffer
      (help-mode)
      (display-buffer buffer))))

(defun blox--prompt-kill-p (process-name)
  "Prompt to kill PROCESS-NAME and return nil if the user answers no.
Otherwise, return t."
  (if (and (get-process process-name)
           (eq (process-status process-name) 'run))
      (if (not (yes-or-no-p (format "%s is still running. Kill it?"
                                    process-name)))
          nil
        (kill-process process-name)
        t)
    t))

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
  (if (blox--prompt-kill-p "*rojo-serve*")
      (let ((directory (or (vc-root-dir) default-directory)))
        (with-current-buffer (get-buffer-create "*rojo-serve*")
          (help-mode)
          (make-process
           :name "*rojo-serve*"
           :buffer (get-buffer "*rojo-serve*")
           :filter 'blox--rojo-serve-echo-filter
           :command
           (list blox-rojo-executable "serve"
                 (read-file-name "Choose project: " directory "")))))))

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
    (with-current-buffer (get-buffer-create "*rojo-build*")
      (help-mode)
      (make-process
       :name "*rojo-build*"
       :buffer (get-buffer "*rojo-build*")
       :command
       (list blox-rojo-executable "build"
             (file-name-nondirectory project-path)
             "--output" output)))
    (cd previous-directory)
    (blox--echo (format "Built %s to %s"
                        (file-name-nondirectory project-path)
                        (concat (file-name-directory project-path)
                                output))
                "blox-rojo-build")
    output))

(defun blox-rojo-build-default ()
  "Build the first found default.project.json."
  (interactive)
  (let ((project (locate-dominating-file default-directory
                                         "default.project.json")))
    (if (not project)
        (blox--echo "Could not locate default.project.json"
                    "blox-rojo-build-default")
      (blox-rojo-build (concat project "default.project.json")))))

(defun blox-run-in-roblox (script-path place-filename)
  "Run the Lua script at SCRIPT-PATH in PLACE-FILENAME with run-in-roblox.
Both SCRIPT-PATH and PLACE-FILENAME must be under the same directory."
  (if (blox--prompt-kill-p "*run-in-roblox*")
      (if (not (locate-file place-filename
                            (list (file-name-directory script-path))))
          (blox--echo
           "Script and place files must be under the same directory"
           "blox-run-in-roblox")
        (let ((previous-directory default-directory))
          (cd (file-name-directory script-path))
          (blox--echo "Starting Roblox Studio..." "blox-run-in-roblox")
          (make-process
           :name "*run-in-roblox*"
           :buffer (get-buffer-create "*run-in-roblox*")
           :sentinel (blox--display-buffer-sentinel
                      (get-buffer "*run-in-roblox*"))
           :command
           (list blox-run-in-roblox-executable
                 "--place" place-filename
                 "--script" (file-name-nondirectory script-path)))
          (cd previous-directory)))))

(defun blox-rojo-build-and-run ()
  "Prompt to build a Rojo project and a script to run in it.
The script and project files are expected to be under the same
directory."
  (interactive)
  (blox-run-in-roblox
   (read-file-name "Choose script: "
                   (or (vc-root-dir) default-directory) "")
   (blox-rojo-build)))

(defun blox-test ()
  "Run `blox-test-script' in `blox-test-project' with run-in-roblox.
This function locates the test files by traversing upwards
through the directory hierarchy starting at `default-directory'
until it finds `blox-test-script'.  It then attempts to locate
`blox-test-project' in `blox-test-script''s containing directory.
If either of these steps fails, the function returns early and
displays a message in the echo area.  In a typical project setup,
this means `blox-test-script' and `blox-test-project' should both
be under the project's root directory."
  (interactive)
  (let ((directory (locate-dominating-file default-directory
                                           blox-test-script)))
    (if (not directory)
        (blox--echo "Could not locate test script" "blox-test")
      (if (not (locate-file blox-test-project (list directory)))
          (blox--echo "Could not locate test project" "blox-test")
        (blox-run-in-roblox
         (concat directory blox-test-script)
         (blox-rojo-build (concat directory
                                  blox-test-project)))))))

(provide 'blox)

;; Local variables:
;; indent-tabs-mode: nil
;; End:

;;; blox.el ends here
