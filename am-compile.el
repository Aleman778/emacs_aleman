;;; am-compile.el --- helper functions for compiling code -*- lexical-binding: t; -*-


(defun am-compilation-exit (status code msg) 
  (if (and (eq status 'exit) (zerop code))
      (if compilation-exit-callback (funcall compilation-exit-callback) nil) nil)
  (setq compilation-exit-message-function nil))


(setq compilation-exit-callback nil)
(setq compilation-exit-message-function 'am-compilation-exit)
(setq cpp-generate-project-bin "generate_project.bat gmake") ; Generate build files binary e.g. Makefiles.

(defun am-build-command (args)
  (let* ((command ""))
    (while args
      (setq command (concat command (pop args) " ")))
    command))

(defun am-run-command (dir command)
  (let ((default-directory dir))
    (compile command)))


(defun am-run! (&rest args)
  (am-run-command (am-buffer-directory) (am-build-command args)))


(defun am-run-in! (dir &rest args)
  (am-run-command dir (am-build-command args)))


(provide 'am-compile)
