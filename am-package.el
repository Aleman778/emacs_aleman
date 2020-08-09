;;; am-package.el --- package management -*- lexical-binding: t; -*-

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

;; Selected packages
(setq am-packages-alist nil)
(setq am-keybinds nil)
(setq am-search-engine nil)
(setq am-completion nil)

(defun am-use-packages (rest)
  (while rest
    (let ((key (pop rest)))
      (cond ((keywordp key)
	     (pcase key
	       (:keybinds
		(if am-keybinds (error "Please only select one keybinding package")
		  (progn (setq am-keybinds (pop rest))
		  (straight-use-package am-keybinds))))
	       (:search
		(if am-search-engine (error "Please only select one search engine package")
		  (progn (setq am-search-engine (pop rest))
		  (straight-use-package am-search-engine))))
	       (:complete
		(if am-completion (error "Please only select one completion package")
		  (progn (setq am-completion (pop rest))
		  (straight-use-package am-completion))))))
	    ((let ((package (pop rest)))
	       (add-to-list 'am-packages-alist package)
	       (straight-use-package package)))))))


(defmacro am-packages! (&rest rest)
  (am-use-packages rest))





(provide 'am-package)
