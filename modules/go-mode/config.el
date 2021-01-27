;;; config.el --- configs for go-mode -*- lexical-binding: t; -*-

(require 'am-compile)


(defun go-run-buffer-file ()
  (interactive)
  (am-run "go" "run" (am-buffer-file-name)))
  

(defun go-build-buffer-file ()
  (interactive)
  (am-run "go" "build"))
  
(defun go-test-buffer-file ()
  (interactive)
  (setq compilation-exit-callback (lambda () (am-run "go tool cover -html=coverage.out")))
  (am-run "go test -coverprofile=coverage.out"))
  

;; Go specific key binds
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-p") 'go-run-buffer-file)
            (local-set-key (kbd "C-M-p") 'go-build-buffer-file)
            (local-set-key (kbd "C-M-u") 'go-test-buffer-file)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))

