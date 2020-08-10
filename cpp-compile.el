;; Elisp functions used for compiling/building C++ code.

;; Some configurations, override these in .dir-locals.el or config.el!
;; This is configured for windows on mingw g++ compiler with gnumake and premake.
(setq cpp-compiler-bin "g++") ; The compiler to use.
(setq cpp-build-bin "make") ; The build binary.
(setq cpp-build-file "Makefile") ; The build file, directory search is used to find this file.
(setq cpp-build-output-bin "bin\\debug\\") ; The output from building project.
(setq cpp-run-default-file "sqrrlc.exe") ; The default filename for built executable.
(setq cpp-build-directory "") ; This is automatically updated each build.
(setq cpp-generate-project-bin "generate_project.bat gmake") ; Generate build files binary e.g. Makefiles.
(setq cpp-generate-project-file "generate_project.bat") ; Searches for this specific file.
(setq cpp-run-file-command-prefix "") ; The command used to run file prefix, e.g. unix use "./".
(setq cpp-run-file-command-suffix ".exe") ; The command used to run file prefix, e.g. windows has ".exe".

(defun cpp-compile ()
  (interactive)
  (let* ((file (am-current-buffer-file-name))
         (executable (am-file-name-change-extension file cpp-run-file-command-suffix))
         (directory (am-current-buffer-directory))
         (command (concat cpp-compiler-bin " " file " -o " executable)))
    (am-compile-command-directory directory command)))


(defun cpp-run-current-file ()
  (interactive)
  (let* ((fname (am-current-buffer-file-name))
          (dir (am-current-buffer-directory)))
    (am-compile-command-directory dir fname)))
    

(defun cpp-generate-project ()
  (interactive)
  (let* ((file (am-current-buffer-file))
         (genfile (locate-dominating-file file cpp-generate-project-file))
         (gendir (file-name-directory genfile)))
    (am-compile-command-directory gendir cpp-generate-project-bin)))


(defun cpp-build-project (option)
  (interactive)
  (print (locate-dominating-file (am-current-buffer-file) cpp-build-file))
  (let* ((file (am-current-buffer-file))
         (buildfile (locate-dominating-file file cpp-build-file))
         (builddir (file-name-directory buildfile))
         (command (concat cpp-build-bin " " option)))
    (setq cpp-build-directory builddir)
    (am-compile-command-directory builddir command)))


(defun cpp-compile-and-run ()
  (interactive)
  (am-define-compilation-ok-hook '(lambda () (cpp-run-current-file)))
  (cpp-compile))


(defun cpp-build-project-default ()
  (interactive)
  (cpp-build-project ""))

(defun cpp-build-and-run-default ()
  (interactive)
  (am-define-compilation-ok-hook
    '(lambda ()
      (let ((command (concat cpp-build-output-bin cpp-run-default-file)))
        (am-compile-command-directory cpp-build-directory command))))
  (cpp-build-project-default))


(provide 'cpp-compile)
