;;; am-compile.el --- helper functions for compiling code -*- lexical-binding: t; -*-


(setq compilation-exit-callback nil)
(defun output-message-sentinel (process msg)
  (when (memq (process-status process) '(exit signal))
    (if compilation-exit-callback (funcall compilation-exit-callback) nil) nil)
  (setq compilation-exit-callback nil))


(defun am-build-command (args)
  (let* ((command ""))
    (while args
      (setq command (concat command (pop args) " ")))
    command))


(defun am-run (&rest args)
  (set-process-sentinel (compile (am-build-command args)) #'output-message-sentinel))


(defun am-run-in (dir &rest args)
  (let ((default-directory dir))
    (set-process-sentinel (compile (am-build-command args)) #'output-message-sentinel)))


(provide 'am-compile)
