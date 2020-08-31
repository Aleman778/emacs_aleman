;;; am-modules.el --- module system for loading packages -*- lexical-binding: t; -*-

;; Setup the straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Module folder
(defconst am-modules-path (file-name-as-directory (concat am-dir "modules")))


;; Selected modules
(defvar am-modules-alist nil)
(defvar am-keybinds nil)
(defvar am-search-engine nil)
(defvar am-completion nil)

;; Ignore configure these packages
(defconst am-ignore-modules-alist `(rg emojify))
(defconst am-ignore-use-modules-alist `(c++-mode))


(defun am-module-put (category module)
  (add-to-list 'am-modules-alist (intern (symbol-name module)))
  (if (member module am-ignore-use-modules-alist) nil (straight-use-package module)))


;; Select packages to use 
(defun am-load-modules (rest)
  (let ((category :utils))
    (while rest
      (let ((module (pop rest)))
        (cond ((keywordp module) 
               (setq category module))
              ((pcase category
                 (:keybinds 
                  (if am-keybinds (error "Please only select one keybinding module")
                    (progn 
                      (setq am-keybinds module)
                      (am-module-put category module))))
                 (:search 
                  (if am-search-engine (error "Please only select one search engine module")
                    (progn 
                      (setq am-search-engine module)
                      (am-module-put category module))))
                 (:utils  (am-module-put category module))
                 (:lang   (am-module-put category module))
                 (:themes (straight-use-package module)))))))))


(defmacro am-modules! (&rest rest)
  (am-load-modules rest))


;; Initialize and configure modules
(defun am-enable-modules ()
  (let ((i 0))
  (while (< i (length am-modules-alist))
    (let* ((module (nth i am-modules-alist))
	   (module-name (symbol-name module))
	   (module-mode (intern (concat module-name "-mode")))
	   (config-path (concat (file-name-as-directory (concat am-modules-path module-name)) "config.el")))
      (if (member module am-ignore-modules-alist) nil
	(if (file-exists-p config-path) (load config-path)
	  (if (boundp module-mode) (funcall module-mode)
	    (warn (concat "Failed to configure module `" module-name "` please configure manually instead"))))))
    (setq i (+ i 1)))))
	
    

(provide 'am-modules)
