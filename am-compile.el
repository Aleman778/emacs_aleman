;; am-compile.el --- helper functions for compiling code -*- lexical-binding: t; -*-


(defmacro am-project-setup! ()
  (defconst am-project-dir (file-name-directory load-file-name)))

(defun am-create-compile-commands (rest))


(defun am-compile (command)
  (let ((default-directory am-project-dir))
    (compile command)))


(defmacro am-create-compilation-target! (funname &rest commands)
  (let ((funsymbol (intern funname)))
    `(defun ,funsymbol () (interactive) (am-compile (pop command)))))


(provide 'am-compile)
