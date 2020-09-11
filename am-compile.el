;;; am-compile.el --- helper functions for compiling code -*- lexical-binding: t; -*-


(setq compilation-exit-callback nil)
(defun output-message-sentinel (process msg)
  (when (eq 0 (process-exit-status process))
    (with-current-buffer (process-buffer process)
      (toggle-read-only)
      (ansi-color-apply-on-region (point-min) (point-max))
      (toggle-read-only)))
  (when (memq (process-status process) '(exit signal))
    (if compilation-exit-callback (funcall compilation-exit-callback) nil) nil)
  (setq compilation-exit-callback nil))
(setq compilation-exit-message-function 'am-compilation-exit)


(defun am-build-command (args)
  (let* ((command ""))
    (while args
      (setq command (concat command (pop args) " ")))
    command))


(defun am_clear_buffer (buffer)
  (with-current-buffer buffer
    (toggle-read-only)
    (erase-buffer)
    (toggle-read-only)))


(defun am-run (name program &rest args)
  (am_clear_buffer "*compilation*")
  (apply #'start-process name "*compilation*" program args)
  (with-current-buffer "*compilation*" (compilation-mode)))


(defun am-run-in (dir name program &rest args)
  (am_clear_buffer "*compilation*")
  (let ((default-directory dir))
    (set-process-sentinel (apply #'start-process name "*compilation*" program args)
                          #'output-message-sentinel))
  (with-current-buffer "*compilation*" (compilation-mode)))


(defun am-shell (name &rest args)
  (set-process-sentinel (start-process-shell-command name "*compilation*" (am-build-command args))
                        #'output-message-sentinel)
  (with-current-buffer "*compilation*" (compilation-mode)))


(defun am-shell-in (dir name &rest args)
  (let ((default-directory dir))
    (set-process-sentinel (start-process-shell-command name "*compilation*" (am-build-command args))
                          #'output-message-sentinel))
  (with-current-buffer "*compilation*" (compilation-mode)))

(provide 'am-compile)
