;; Elisp functions used for compiling/building C++ code.

(defun c++-kill-program ()
  "Kills the compilation programs."
  (interactive)
  (shell-command "taskkill /IM msbuild.exe /F")
  (shell-command "taskkill /IM cl.exe /F")
  (shell-command "taskkill /IM Sandbox.exe /F"
                 (get-buffer "*compilation*") (get-buffer "*compilation*")))

(defun c++-compile-and-run ()
  "Compiles the program and runs it if compiled correctly"
  (interactive)
  (setq compilation-exit-message-function 'c++-run-if-compilation-ok)
  (c++-build-debug))

(defun c++-build-release ()
  "Builds the project in release mode"
  (interactive)
  (let ((default-directory project-directory))
    (compile "BuildProject.bat release")))

(defun c++-build-debug ()
  "Builds the project in debug mode"
  (interactive)
  (let ((default-directory project-directory))
    (compile "BuildProject.bat debug")))

(defun c++-custom-compile () 
  (interactive)
  (setq compilation-exit-message-function nil)
  (compile "make -k new"))

(defun c++-run-if-compilation-ok (status code msg)
  (if (and (eq status 'exit) (zerop code))
      (c++-run-program-debug)
    (cons msg code)))

(defun c++-run-program-release () 
  (interactive)
  (let ((default-directory project-directory))
    (async-shell-command "bin\\release-windows-x86_64\\Sandbox\\Sandbox.exe"
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))

(defun c++-run-program-debug ()
  (interactive)
  (let ((default-directory project-directory))
    (async-shell-command "bin\\debug-windows-x86_64\\Sandbox\\Sandbox.exe"
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))
