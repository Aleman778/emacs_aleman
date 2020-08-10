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
(setq am-modules-alist nil)
(setq am-keybinds nil)
(setq am-search-engine nil)
(setq am-completion nil)


;; Select packages to use 
(defun am-load-modules (rest)
  (while rest
      (let ((key (pop rest)))
        (cond ((keywordp key)
               (pcase key
		 (:keybinds
		  (if am-keybinds (error "Please only select one keybinding module")
		    (setq am-keybinds (car rest))))
		 (:search
		  (if am-search-engine (error "Please only select one search engine module")
		    (setq am-search-engine (car rest))))
		 (:complete
		  (if am-completion (error "Please only select one completion module")
		    (setq am-completion (car rest))))
		 (:utils  nil)
		 (:themes nil)
		 (_
                  (condition-case _
		      (error (concat "Unknown category `" (symbol-name key)
				     "`, use either `keybinds`, `search`, `complete`, `utils` or `themes`"))))))
	      ((add-to-list 'am-modules-alist (intern (symbol-name key)))
	       (straight-use-package key))))))

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
      (print config-path)
      (if (file-exists-p config-path) (load config-path)
	(if (boundp module-mode) (funcall module-mode)
	  (warn (concat "Failed to configure module `" module-name "` please configure manually instead")))))
    (setq i (+ i 1)))))
	
    

(provide 'am-modules)
