;;; config.el -*- lexical-binding: t; -*-

;; Load company
(require 'company)

;; Enable company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
