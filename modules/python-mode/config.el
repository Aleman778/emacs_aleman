;;; config.el --- configs for python-mode -*- lexical-binding: t; -*-

(require 'am-compile)


(defun python-run-buffer-file ()
  (interactive)
  (am-run "python-run-buffer-file" "python" (am-buffer-file-name)))


;; Go specific key binds
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-p") 'python-run-buffer-file)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))

