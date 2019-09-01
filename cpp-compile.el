;; Elisp functions used for compiling/building C++ code.

;; C++ compilation configurations

(setq cpp-project-directory "C:\\dev\\am-engine")
(setq cpp-premake5-target "vs2019")


;; Compilation functions

(defun cpp-kill-program ()
  "Kills the compilation programs."
  (interactive)
  (shell-command "taskkill /IM msbuild.exe /F")
  (shell-command "taskkill /IM cl.exe /F")
  (shell-command "taskkill /IM Sandbox.exe /F"
                 (get-buffer "*compilation*") (get-buffer "*compilation*")))


(defun cpp-compile-and-run ()
  "Compiles the program and runs it if compiled correctly"
  (interactive)
  (setq compilation-exit-message-function 'cpp-run-if-compilation-ok)
  (cpp-build-debug))


(defun cpp-build-release ()
  "Builds the project in release mode"
  (interactive)
  (let ((default-directory cpp-project-directory))
    (compile "BuildProject.bat release")))


(defun cpp-build-debug ()
  "Builds the project in debug mode"
  (interactive)
  (let ((default-directory cpp-project-directory))
    (compile "BuildProject.bat debug")))


(defun cpp-custom-compile () 
  (interactive)
  (setq compilation-exit-message-function nil)
  (compile "make -k new"))


(defun cpp-run-if-compilation-ok (status code msg)
  (if (and (eq status 'exit) (zerop code))
      (cpp-run-program-debug)
    (cons msg code)))


(defun cpp-run-program-release () 
  (interactive)
  (let ((default-directory cpp-project-directory))
    (async-shell-command "bin\\release-windows-x86_64\\Sandbox\\Sandbox.exe"
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))


(defun cpp-run-program-debug ()
  (interactive)
  (let ((default-directory cpp-project-directory))
    (async-shell-command "bin\\debug-windows-x86_64\\Sandbox\\Sandbox.exe"
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))


(defun cpp-generate-project ()
  (interactive)
  (let ((default-directory cpp-project-directory))
    (async-shell-command (concat "GenerateProjects.bat " cpp-premake5-target)
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))


(provide 'cpp-compile)
