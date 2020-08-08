;;; am-keybinds --- defines keybinds for convenience -*- lexical-binding: t; -*- 

;; Basic leader key variables
(defvar am-leader-key "SPC")		;; Default vim leader key is space
(defvar am-leader-alt-key "M-SPC")	;; Alternative leader key
(defvar am-localleader-key "SPC m")	;; Default local major-mode leader key
(defvar am-localleader-key "M-SPC m")	;; Alternative local major-mode leader key


;; general.el is a required package.
(straight-use-package 'general)

(provide 'am-keybinds)
