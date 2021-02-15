;;; blox.el --- Interaction with Roblox tooling -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kenneth Loeffler

;; Author: Kenneth Loeffler <kenneth.loeffler@outlook.com>
;; Version: 0.2.0
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

;; blox provides functions to run Roblox tooling such as Rojo (see
;; https://github.com/rojo-rbx/rojo) from Emacs.

;; blox doesn't bind any keys by itself; assuming `lua-mode' is
;; installed and loaded, a configuration might look something like
;; this:

;; (require 'blox)

;; (define-key lua-prefix-mode-map (kbd "s")
;;             #'blox-prompt-serve)
;; (define-key lua-prefix-mode-map (kbd "b")
;;             #'blox-prompt-build)
;; (define-key lua-prefix-mode-map (kbd "t")
;;             #'blox-test)

;; Or with `use-package':

;; (use-package blox
;;   :bind (:map lua-prefix-mode-map
;;               ("s" . blox-prompt-serve)
;;               ("b" . blox-prompt-build)
;;               ("t" . blox-test)))

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
  "Display MESSAGE-STRING prefixed with COMMAND-NAME in the echo area.
Removes any trailing newlines from MESSAGE-STRING."
  (message "[%s]: %s"
           command-name
           (replace-regexp-in-string "\n\\'" "" message-string)))

(defun blox--echo-filter (command-name)
  "Process filter to display process output in the echo area.
The filter prefixes any output with COMMAND-NAME and writes the
result to the process buffer, in addition to displaying it in the
echo area."
  (lambda (proc string)
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (setq buffer-read-only nil)
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))
        (setq buffer-read-only t))
      (blox--echo string command-name))))

(defun blox--run-in-roblox-sentinel (buffer)
  "Process sentinel to call `display-buffer' on BUFFER.
Also enable `help-mode' in BUFFER and display a message in the
echo area."
  (lambda (_process _event)
    (with-current-buffer buffer
      (help-mode)
      (display-buffer buffer)
      (blox--echo "Waiting for output from Roblox Studio...done"
                  "blox-run-in-roblox"))))

(defun blox--run-after-build-sentinel (script-path project-path)
  "Process sentinel to run SCRIPT-PATH in PROJECT-PATH once finished."
  (lambda (_process event)
    (if (equal event "finished\n")
        (blox-run-in-roblox
         script-path
         (blox--build-path project-path)))))

(defun blox--kill-if-running-p (process-name)
  "Prompt to kill the process if PROCESS-NAME is running.
Return t if the answer is \"yes\" or if the process is not
running.  Return nil if the process is running and the answer is
\"no\"."
  (or (not (and (get-process process-name)
                (eq (process-status process-name) 'run)))
      (kill-buffer (process-buffer (get-process process-name)))))

(defun blox--roblox-file-extension (project-path)
  "Return the appropriate Roblox file extension for PROJECT-PATH."
  (with-temp-buffer
    (insert-file-contents project-path)
    (if (search-forward "DataModel" nil t)
        ".rbxlx"
      ".rbxmx")))

(defun blox--build-path (project-path)
  "Return the path of the build corresponding to PROJECT-PATH."
  (concat (file-name-sans-extension (file-name-base project-path))
          (blox--roblox-file-extension project-path)))

(defun blox-rojo-serve (project-path)
  "Serve the Rojo project at PROJECT-PATH."
  (blox--save-some-lua-mode-buffers)
  (if (blox--kill-if-running-p "*rojo-serve*")
      (with-current-buffer (get-buffer-create "*rojo-serve*")
        (cd (or (vc-root-dir) default-directory))
        (help-mode)
        (make-process
         :name "*rojo-serve*"
         :buffer (get-buffer "*rojo-serve*")
         :filter (blox--echo-filter "blox-prompt-serve")
         :command
         (list blox-rojo-executable "serve" project-path)))))

(defun blox-rojo-build (project-path &optional sentinel)
  "Build the Rojo project at PROJECT-PATH.
If the function SENTINEL is provided, attach it to the Rojo
process."
  (blox--save-some-lua-mode-buffers)
  (with-current-buffer (get-buffer-create "*rojo-build*")
    (cd (file-name-directory project-path))
    (help-mode)
    (make-process
     :name "*rojo-build*"
     :buffer (get-buffer "*rojo-build*")
     :filter (blox--echo-filter "blox-rojo-build")
     :sentinel sentinel
     :command
     (list
      blox-rojo-executable "build" (file-name-nondirectory
                                    project-path)
      "--output" (blox--build-path project-path)))))

(defun blox-run-in-roblox (script-path build-path)
  "Run the Lua script at SCRIPT-PATH in BUILD-PATH with run-in-roblox.
Both SCRIPT-PATH and BUILD-PATH must have the same penultimate
directory.  If this is not the case, abort and display a message
in the echo area."
  (if (blox--kill-if-running-p "*run-in-roblox*")
      (if (not (locate-file build-path
                            (list (file-name-directory script-path))))
          (blox--echo
           "Script and place files must be under the same directory"
           "blox-run-in-roblox")
        (with-current-buffer (get-buffer-create "*run-in-roblox*")
          (cd (file-name-directory script-path))
          (blox--echo "Waiting for output from Roblox Studio..."
                      "blox-run-in-roblox")
          (make-process
           :name "*run-in-roblox*"
           :buffer (get-buffer-create "*run-in-roblox*")
           :sentinel (blox--run-in-roblox-sentinel
                      (get-buffer "*run-in-roblox*"))
           :command
           (list blox-run-in-roblox-executable
                 "--place" (file-name-nondirectory build-path)
                 "--script" (file-name-nondirectory script-path)))))))

(defun blox-prompt-serve ()
  "Prompt to serve a Rojo project."
  (interactive)
  (blox--save-some-lua-mode-buffers)
  (if (blox--kill-if-running-p "*rojo-serve*")
      (blox-rojo-serve (read-file-name "Choose project: "
                                       (or (vc-root-dir)
                                           default-directory)))))

(defun blox-prompt-build ()
  "Prompt to build a Rojo project."
  (interactive)
  (blox-rojo-build (read-file-name "Choose project: "
                                   (or (vc-root-dir)
                                       default-directory))))

(defun blox-build-default ()
  "Build the first found default.project.json.
Locate the project by traversing upwards through the directory
hierarchy until reaching a directory that contains a file named
default.project.json.  If this fails, abort and display a message
in the echo area."
  (interactive)
  (let ((project (locate-dominating-file default-directory
                                         "default.project.json")))
    (if (not project)
        (blox--echo "Could not locate default.project.json"
                    "blox-build-default")
      (blox-rojo-build (concat project "default.project.json")))))

(defun blox-prompt-build-and-run ()
  "Prompt to build a Rojo project, then for a script to run in it.
The script and project files are expected to be under the same
directory.  If this is not the case, abort and display a message
in the echo area."
  (interactive)
  (let* ((directory (or (vc-root-dir) default-directory))
         (project (read-file-name "Choose project: " directory))
         (script (read-file-name "Choose script " directory)))
    (blox-rojo-build project
                     (blox--run-after-build-sentinel script
                                                     project))))

(defun blox-test ()
  "Run `blox-test-script' in `blox-test-project' with run-in-roblox.
Locate the test files by traversing upwards through the directory
hierarchy starting at `default-directory' until finding
`blox-test-script', then attempt to locate `blox-test-project' in
`blox-test-script''s containing directory.  If either of these
steps fails, abort and display a message in the echo area.

In a typical project setup, this means `blox-test-script' and
`blox-test-project' should both be under the project's root
directory."
  (interactive)
  (if-let ((directory (locate-dominating-file default-directory
                                              blox-test-script))
           (test-project (concat directory blox-test-project))
           (test-script (concat directory blox-test-script)))
      (blox-rojo-build
       test-project
       (blox--run-after-build-sentinel test-script
                                       test-project))
    (blox--echo "Could not locate test script" "blox-test")))

(provide 'blox)

;; Local variables:
;; indent-tabs-mode: nil
;; End:

;;; blox.el ends here
