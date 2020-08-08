;;; am-utils.el --- basic utility functions -*- lexical-binding: t; -*-

;; Disable C-x l key bindings
(defun am-text-hook () 
  (define-key text-mode-map "\C-x l" nil))

;; Set focus to previous window
(defun am-back-window () 
  (interactive) 
  (other-window -1))

