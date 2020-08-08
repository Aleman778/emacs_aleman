;; Some utility functions


(defun current-buffer-file ()
  "Returns the file name of the current buffer"
  (buffer-file-name (window-buffer (minibuffer-selected-window))))


(defun current-buffer-file-name ()
  "Returns the file name of the current buffer"
  (file-name-nondirectory (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(defun current-buffer-directory ()
  "Returns the file name of the current buffer"
  (file-name-directory (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(defun file-name-change-extension (file ext)
  (concat (file-name-sans-extension file) ext))


(defun compile-command-directory (directory command)
  "Runs the specific compile command in the given directory"
  (let ((default-directory directory))
    (compile command)))


(defun run-async-command-in-compilation-buffer (directory command)
  "Runs the specific command in asynchronously in compilation buffer"
  (let ((buffer (get-buffer "*compilation*")))
    (run-asnyc-command dirctory command buffer)))
  

(defun run-async-command (directory command buffer)
  "Runs the specific command in asynchronously in sepcific buffer buffer"
  (let ((default-directory directory))
    (async-shell-command command buffer buffer)))


(defun define-compilation-ok-hook (callback)
  "Defines a callback function to be called when compilation succedes"
  (setq compilation-exit-hook callback)
  (setq compilation-exit-message-function 
        '(lambda (status code msg) (if (and (eq status 'exit) (zerop code))
          (progn (setq compilation-exit-message-function nil) 
                 (funcall compilation-exit-hook)) (cons msg code)))))


;; From https://github.com/tcrayford/emacs/blob/master/dominating-file.el
(unless (functionp 'locate-dominating-file)
  (defun locate-dominating-file (file name)
    "Look up the directory hierarchy from FILE for a file named NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found."
    (setq file (abbreviate-file-name file))
    (let ((root nil)
          (prev-file file)
          (user nil)
          try)
      (while (not (or root
                      (null file)
                      (string-match locate-dominating-stop-dir-regexp file)))
        (setq try (file-exists-p (expand-file-name name file)))
        (cond (try (setq root file))
              ((equal file (setq prev-file file
                                 file (file-name-directory
                                       (directory-file-name file))))
               (setq file nil))))
      root))

  (defvar locate-dominating-stop-dir-regexp
    "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"))


(provide 'am-utils)
