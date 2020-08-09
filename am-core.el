;;; am-core.el --- core settings and setup script -*- lexical-binding: t; -*-


;; Basic preferences
(menu-bar-mode -1)			;; Disable menu bar.
(desktop-save-mode -1)			;; Disable desktop save mode.
(tool-bar-mode -1)			;; Disable toolbar.
(scroll-bar-mode -1)			;; Disable scroll bars.
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize the frame on startup
(set-background-color "floral white");; Set background color to reduce blue light
(setq inhibit-startup-screen t)		;; Disable the startup screen
(setq visible-bell t)			;; Emacs stop shouting at me!!!
(setq compilation-read-command nil)	;; Disable user from reading compilation command.
(setq compilation-scroll-output t)	;; Always scroll the the bottom of compilation buffer.
(setq count-lines-page nil)		;; Disable Ctrl-x l command.
(setq next-line-add-newlines nil)	;; Insert new lines useing C-n if end of file.
(setq backup-directory-alist `((".*" . "~/.saves/"))) ;; Set the backup directory

;; Basic filepaths
(setq am-config-file   "~/.emacs.d/config.el")
(setq am-packages-file "~/.emacs.d/packages.el")

;; Setup package manager
(require 'am-package)

;; Setup keybinds
(require 'am-keybinds)

;; Setup basic functions
(require 'am-functions)

;; Create files if they don't exists
(if (file-exists-p am-config-file) nil
  (write-region (am-elisp-template "config.el" "your own custom tinkering can be done here") nil am-config-file))
(if (file-exists-p am-packages-file) nil
  (write-region (am-elisp-template "packages.el" "define packages to use") nil am-packages-file))

;; Load custom packages
(load am-packages-file)

;; Reuse buffer frames when using display buffers
(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

;; Space shows possible files to open
(if (boundp 'minibuffer-local-filename-completion-map)
    (progn
      (define-key minibuffer-local-filename-completion-map " "
	'minibuffer-complete-word)
      (define-key minibuffer-local-must-match-filename-map " "
	'minibuffer-complete-word)))

;; Enable ansi-colors in compilation buffer.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Disable the `C-x l` command
(add-hook 'text-mode-hook 'am-text-hook)

;; Disable auto line breaks
(auto-fill-mode nil)

;; Org mode disable validation link
(setq org-export-html-validation-link nil)

;; Initializes the packages and onfigs
(defun am-initialize ()
  ;; Setup each package installed

  ;; Setup keybindings
  (cond ((eq am-keybinds evil) (evil-mode 1))
	((eq am-keybinds evil) (load "modules/xah-fly-keys/config.el")))

  ;; Allow for final customizations before finishing up
  (load "~/.emacs.d/config.el"))


(provide 'am-core)
