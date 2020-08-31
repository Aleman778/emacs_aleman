;;; am-filesystem.el --- provides filesystem functions -*- lexical-binding: t; -*-


(defun am-buffer-path ()
  (buffer-file-name (window-buffer (minibuffer-selected-window))))


(defun am-buffer-file-name ()
  (file-name-nondirectory (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(defun am-buffer-directory ()
  (file-name-directory (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(defun am-file-extension (file ext)
  (concat (file-name-sans-extension file) ext))


;; From https://github.com/tcrayford/emacs/blob/master/dominating-file.el
(defun am-locate-file (file name)
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        (prev-file file)
        (user nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match am-stop-dir-regexp file)))
      (setq try (file-exists-p (expand-file-name name file)))
      (cond (try (setq root file))
            ((equal file (setq prev-file file
                               file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    root))

(defvar am-stop-dir-regexp
  "\\`\\(?:[\\/][\\/][^\\/]+\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")


(provide 'am-filesystem)
