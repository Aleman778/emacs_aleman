;;; am-functions.el --- basic utility functions -*- lexical-binding: t; -*-


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


;;; Default window layouts


(defun am-layout-two-columns (file1 file2)
  "Creates a layout with two columns the right column has two buffers. Loads two files."
  (delete-other-windows)
  (find-file file1)
  (split-window-right)
  (other-window 1)
  (find-file file2)
  (split-window-below)
  (if (eq (get-buffer "*compilation*") nil) (get-buffer-create "*compilation*") nil)
  (enlarge-window (/ (window-height (next-window)) 2))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (other-window 1))


(defun am-layout-three-columns (file1 file2 file3)
  "Creates a layout with three columns. Loads three files."
  (delete-other-windows)
  (find-file file1)
  (split-window-right)
  (other-window 1)
  (enlarge-window-horizontally (/ (window-width (next-window)) 3))
  (find-file file2)
  (split-window-right)
  (other-window 1)
  (find-file file3)
  (split-window-below)
  (if (eq (get-buffer "*compilation*") nil) (get-buffer-create "*compilation*") nil)
  (balance-windows)
  (enlarge-window (/ (window-height (next-window)) 2))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (other-window 1))


;;; Comment functions

;; Inserting a section comment
(defun am-insert-section-comment ()
  "Inserts a C++ style section comment."
  (interactive)
  (if (am-inside-comment)
      (error "Already inside comment.")
    (beginning-of-line)
    (open-line 1)
    (indent-according-to-mode)
    (insert "/")
    (let* ((indentation (current-column))
           (star-str (make-string (- 76 indentation)?*)))
      (insert star-str "\n")
      (indent-to indentation)
      (insert "* \n")
      (indent-to indentation)
      (insert star-str "/")
      (end-of-line 0))))


;; Inserting a documentation comment
(defun am-insert-doc-comment ()
  "Inserts a C++ style documentation comment."
  (interactive)
  (if (am-inside-comment)
      (error "Already inside comment.")
    (beginning-of-line)
    (open-line 1)
    (indent-according-to-mode)
    (let* ((indentation (current-column)))
      (insert "/**\n")
      (indent-to indentation)
      (insert " * \n")
      (indent-to indentation)
      (insert " */")
      (end-of-line 0))))


;; Check if cusor is already inside a comment
(defun am-inside-comment ()
  "Test if cursor/point in a commented line?"
  (save-excursion
        (if (derived-mode-p 'org-mode)
                (save-match-data (beginning-of-line) (looking-at "^[ \t]*#"))
          (nth 4 (syntax-ppss)))))


;;; Change theme functions


(setq am-theme-alist (list 'default))
(setq *cur-theme* 'default)
(setq *cur-theme-idx* -1)

;; Disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

;; Load the next theme
(defun am-next-theme (theme)
  (if (eq theme 'default)
      (progn (disable-theme *cur-theme*)
             (set-background-color "floral white"))
    (progn
      (load-theme theme t)))
  (message (concat "Loading theme: " (number-to-string *cur-theme-idx*) " (" (symbol-name theme) ")"))
  (setq *cur-theme* theme))

;; Change theme to next theme in the themes
(defun am-change-theme ()
  (interactive)
  (setq *cur-theme-idx* (1+ *cur-theme-idx*))
  (if (< *cur-theme-idx* (length am-theme-alist)) nil (setq *cur-theme-idx* 0))
  (am-next-theme (nth *cur-theme-idx* am-theme-alist)))


;;; Other utilities for compilation

(defun am-current-buffer-file ()
  "Returns the file name of the current buffer"
  (buffer-file-name (window-buffer (minibuffer-selected-window))))


(defun am-current-buffer-file-name ()
  "Returns the file name of the current buffer"
  (file-name-nondirectory (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(defun am-current-buffer-directory ()
  "Returns the file name of the current buffer"
  (file-name-directory (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(defun am-file-name-change-extension (file ext)
  (concat (file-name-sans-extension file) ext))


(defun am-compile-command-directory (directory command)
  "Runs the specific compile command in the given directory"
  (let ((default-directory directory))
    (compile command)))


(defun am-run-async-command-in-compilation-buffer (directory command)
  "Runs the specific command in asynchronously in compilation buffer"
  (let ((buffer (get-buffer "*compilation*")))
    (run-asnyc-command dirctory command buffer)))
  

(defun am-run-async-command (directory command buffer)
  "Runs the specific command in asynchronously in sepcific buffer buffer"
  (let ((default-directory directory))
    (async-shell-command command buffer buffer)))


(defun am-define-compilation-ok-hook (callback)
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


(provide 'am-functions)
