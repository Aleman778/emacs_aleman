;;; am-preferences --- my preferences, feel free to change them -*- lexical-binding: t; -*-


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


;; More settings


;; Disable C-x l key bindings
(defun am-text-hook () 
  (define-key text-mode-map "\C-x l" nil))


;; Set focus to previous window
(defun am-back-window () 
  (interactive) 
  (other-window -1))


;; Returns a simple lisp file template
(defun am-elisp-template (filename desc)
  (concat ";;; " filename " --- " desc " -*- lexical-binding: t; -*-"))


;; Align with spaces only, spaces rules.
(defadvice align-regexp (around align-regexp-with-spaces)
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)


(menu-bar-mode -1)                                           ;; Disable menu bar.
(desktop-save-mode -1)                                       ;; Disable desktop save mode.
(tool-bar-mode -1)                                           ;; Disable toolbar.
(scroll-bar-mode -1)                                         ;; Disable scroll bars.
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize the frame on startup
(set-background-color "floral white")                        ;; Default emacs color hurts my eyes.
(setq-default indent-tabs-mode nil)                          ;; Change all indentation to use spaces, spaces rules.
(setq inhibit-startup-screen t)                              ;; Disable the startup screen
(setq visible-bell t)                                        ;; Emacs stop shouting at me!!!
(setq compilation-read-command nil)                          ;; Disable user from reading compilation command.
(setq compilation-scroll-output t)                           ;; Always scroll the the bottom of compilation buffer.
(setq count-lines-page nil)                                  ;; Disable Ctrl-x l command.
(setq next-line-add-newlines nil)                            ;; Insert new lines useing C-n if end of file.
(setq backup-directory-alist `((".*" . "~/.saves/")))        ;; Set the backup directory
(setq ido-auto-merge-work-directories-length -1)             ;; Disable ido mode from merging files from other directories
(add-hook 'text-mode-hook 'am-text-hook)                     ;; Disable the `C-x l` command
(auto-fill-mode nil)                                         ;; Disable auto line breaks
(show-paren-mode 1)                                          ;; Show matching parenthesis
(setq org-export-html-validation-link nil)                   ;; Org mode disable validation link


(provide 'am-preferences)
