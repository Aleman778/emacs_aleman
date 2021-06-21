;;; config.el --- configs for python-mode -*- lexical-binding: t; -*-

(require 'am-compile)


(defun python-run-buffer-file ()
  (interactive)
  (am-run "python" (am-buffer-file-name)))

(defun python-run-black-buffer-file ()
  (interactive)
  (am-run "black" (am-buffer-file-name) "--line-length=120"))

(defun python-run-flake8-buffer-file ()
  (interactive)
  (am-run "python -m flake8" (am-buffer-file-name) "--max-line-length=120"))

(defun python-run-mypy-buffer-file ()
  (interactive)
  (am-run "mypy" (am-buffer-file-name) "--ignore-missing-imports"))


;; Go specific key binds
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-p") 'python-run-buffer-file)
            (local-set-key (kbd "C-M-u") 'python-run-flake8-buffer-file)
            (local-set-key (kbd "C-M-i") 'python-run-mypy-buffer-file)
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq python-indent 2)))
