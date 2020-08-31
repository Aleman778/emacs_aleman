;;; am-core.el --- core settings and setup script -*- lexical-binding: t; -*-

;; Basic filepaths
(defconst am-dir (file-name-directory load-file-name))
(defconst am-init-file   "~/.emacs.d/init.el")
(defconst am-config-file "~/.emacs.d/config.el")


;; Require modules
(require 'am-preferences)
(require 'am-modules)
(require 'am-keybinds)
(require 'am-comment)
(require 'am-theme)
(require 'am-filesystem)
(require 'am-compile)
(require 'am-layout)


;; Create files if they don't exists
(if (file-exists-p am-init-file) nil
  (write-region (am-elisp-template "init.el" "define modules to use") nil am-init-file))
(if (file-exists-p am-config-file) nil
  (write-region (am-elisp-template "config.el" "your own custom tinkering can be done here") nil am-config-file))


;; Load custom packages
(load am-init-file)


;; Initializes the packages and onfigs
(defun am-initialize ()
  ;; Setup each module installed
  (am-enable-modules)
  
  ;; Allow for final customizations before finishing up
  (load am-config-file))


(provide 'am-core)
