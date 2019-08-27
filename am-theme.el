;; Handle multiple emacs themes and make it possible to switch between them

(setq am-theme-alist (list 'default))
(setq *current-theme* 'default)
(setq *current* -1)

;; Disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

;; Load the next theme
(defun am-next-theme (theme)
  (if (eq theme 'default)
      (progn (disable-theme *current-theme*)
             (set-background-color "floral white"))
    (progn
      (load-theme theme t)))
  (message (concat "Loading theme: " (number-to-string *current*) " (" (symbol-name theme) ")"))
  (setq *current-theme* theme))

;; Change theme to next theme in the themes
(defun am-change-theme ()
  (interactive)
  (setq *current* (1+ *current*))
  (if (< *current* (length am-theme-alist)) nil (setq *current* 0))
  (am-next-theme (nth *current* am-theme-alist)))

(provide 'am-theme)
