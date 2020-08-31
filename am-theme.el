;; am-theme.el --- switch themes seamlessly using keybinds -*- lexical-binding: t; -*-


(setq am-background-alist (list "floral white"))
(setq am-theme-alist (list 'default))
(setq *cur-theme* 'default)
(setq *cur-theme-idx* -1)


;; Disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))


;; Load the next theme
(defun am-next-theme (theme)
  (if (eq theme 'default) (disable-theme *cur-theme*)
    (load-theme theme t))
  (if (< *cur-theme-idx* (length am-background-alist))
      (let ((background (nth *cur-theme-idx* am-background-alist)))
	(if background (set-background-color background) nil)) nil)
  (message (concat "Loading theme: " (number-to-string *cur-theme-idx*) " (" (symbol-name theme) ")"))
  (setq *cur-theme* theme))


;; Change theme to next theme in the themes
(defun am-change-theme ()
  (interactive)
  (setq *cur-theme-idx* (1+ *cur-theme-idx*))
  (if (< *cur-theme-idx* (length am-theme-alist)) nil (setq *cur-theme-idx* 0))
  (am-next-theme (nth *cur-theme-idx* am-theme-alist)))


(provide 'am-theme)
