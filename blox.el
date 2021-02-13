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

(defcustom blox-test-project-name "test.project.json"
  "The name of the project to use for `blox-test'."
  :type 'string
  :group 'blox)

(defcustom blox-test-script-name "TestRunner.server.lua"
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
  "Higher-order function for returning a process sentinel which displays BUFFER whenever there is an event."
  (lambda (_process _event)
    (with-current-buffer buffer
      (help-mode)
      (display-buffer buffer))))

(defun blox-rojo-serve ()
  "Prompt for a project file for Rojo to start serving."
  (interactive)
  (blox--save-some-lua-mode-buffers)
  (if (and (get-process "*rojo-serve*")
           (eq (process-status "*rojo-serve*") 'run))
      (kill-buffer "*rojo-serve*"))
    (let ((dir (or (vc-root-dir) default-directory)))
      (with-current-buffer (get-buffer-create "*rojo-serve*")
        (help-mode)
        (make-process
         :name "*rojo-serve*"
         :buffer (get-buffer "*rojo-serve*")
         :command
         (list blox-rojo-executable "serve"
               (read-file-name "Choose project: " dir ""))
         :filter 'blox--echo-filter))))

(defun blox-rojo-build (&optional working-dir force-project)
  "Prompt for a project file for Rojo to build.

If WORKING-DIR is provided, output into that directory.  If FORCE-PROJECT is provided, use that project instead of prompting."
  (interactive)
  (blox--save-some-lua-mode-buffers)
  (let* ((dir (or (vc-root-dir) default-directory))
         (project (or force-project
                      (read-file-name "Choose project: " dir "")))
         (output (s-concat (file-name-sans-extension
                            (file-name-nondirectory project))
                           ".rbxlx")))
    (cd (or working-dir
            (file-name-directory project)))
    (make-process
     :name "*rojo-build*"
     :buffer (get-buffer-create "*rojo-build*")
     :command (list blox-rojo-executable "build" (file-name-nondirectory project)
                    "--output" output))
    output))

(defun blox-rojo-build-default ()
  "Build default.project.json in `vc-root-dir'."
  (interactive)
  (blox-rojo-build (vc-root-dir)
                   "default.project.json"))

(defun blox-run-in-roblox (place script working-dir)
  "Run SCRIPT in PLACE with run-in-roblox in WORKING-DIR."
  (cd working-dir)
  (if (and (get-process "*run-in-roblox*")
           (eq (process-status "*run-in-roblox*") 'run))
      (kill-buffer "*run-in-roblox*"))
  (make-process
     :name "*run-in-roblox*"
     :buffer (get-buffer-create "*run-in-roblox*")
     :command (list blox-run-in-roblox-executable
                    "--place" place
                    "--script" (file-name-nondirectory script))
     :sentinel (blox--display-buffer-sentinel
                (get-buffer "*run-in-roblox*"))))

(defun blox-rojo-build-and-run ()
  "Prompt for a Rojo place and a script, build the place, then run the script."
  (interactive)
  (let* ((dir (or (vc-root-dir) default-directory))
         (script (read-file-name "Choose script: " dir ""))
         (place-file (blox-rojo-build (file-name-directory script))))
    (blox-run-in-roblox place-file script (file-name-directory script))))

(defun blox-test ()
  "Run `blox-test-script' in `blox-test-place' with run-in-roblox.

The files are both expected to be under `vc-root-dir'"
  (interactive)
  (blox-run-in-roblox (blox-rojo-build (vc-root-dir) blox-test-project-name)
                      blox-test-script-name
                      (vc-root-dir)))

(provide 'blox)

;; Local variables:
;; indent-tabs-mode: nil
;; End:

;;; blox.el ends here
