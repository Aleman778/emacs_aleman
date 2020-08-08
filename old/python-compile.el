;; Elisp functions for binding keys to compilation functions.


;; Python compiler parameters

(setq python-command "python")

;; Compilation functions

(defun python-execute (directory file)
  "Executes the given file using the compile command and optionally in an environment."
  (let ((default-directory directory))
    (compile (concat python-command " " file))))

(defun python-execute-current ()
  "Executes the current python file using the python interpreter."
  (interactive)
  (setq filepath (python-current-file))
  (let ((directory (file-name-directory filepath))
        (file (file-name-nondirectory filepath)))
    (python-execute directory file)))

(defun python-current-file ()
  "Returns the file name of the current buffer."
  (buffer-file-name (window-buffer (minibuffer-selected-window))))

(provide 'python-compile)
