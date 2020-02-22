;; Setting up custom keybindings for Xah-fly-keys

(defun my-xah-command-mode-keys () 
  (interactive)
  ;; Go forward to the the right bracket (command mode: /)
  (define-key xah-fly-key-map (kbd "/") 'xah-forward-right-bracket)


  ;; Go to the next buffer (command mode: .)
  (define-key xah-fly-key-map (kbd ".") 'other-window)


  ;; Go to the next buffer (command mode: ,)
  (define-key xah-fly-key-map (kbd ",") 'am-back-window)


  ;; My delete line w/o copy to kill-ring (command mode: g)
  (define-key xah-fly-key-map (kbd "g") 'kill-line)


  ;; Delete entire block using (command mode: SPACE g)
  (define-key xah-fly-key-map (kbd "SPC g") 'xah-delete-current-text-block)


  ;; Electric buffer mode (command mode: SPACE i d)
  (define-key xah-fly-key-map (kbd "SPC i d") 'electric-buffer-list)


  ;; Electric buffer mode (command mode: SPACE i d)
  (define-key xah-fly-key-map (kbd "SPC o t") 'kmacro-end-and-call-macro)


  ;; Next error (command mode: \)
  (define-key xah-fly-key-map (kbd "\\") 'next-error)


  ;; Previous error (command mode: ')
  (define-key xah-fly-key-map (kbd "h") 'previous-error)
  

  ;; Make escape key do Ctrl-g (quit)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))


  (define-key xah-fly-key-map (kbd "k") 'backward-char)
  (define-key xah-fly-key-map (kbd "i") 'backward-word)
  (define-key xah-fly-key-map (kbd "l") 'next-line)
  (define-key xah-fly-key-map (kbd "o") 'previous-line)
  (define-key xah-fly-key-map (kbd ";") 'forward-char)
  (define-key xah-fly-key-map (kbd "p") 'forward-word)
  (define-key xah-fly-key-map (kbd "j") 'xah-beginning-of-line-or-block)
  (define-key xah-fly-key-map (kbd "'") 'xah-end-of-line-or-block)
  (define-key xah-fly-key-map (kbd "[") 'xah-insert-space-before)
  (define-key xah-fly-key-map (kbd "SPC <left>") 'previous-buffer)
  (define-key xah-fly-key-map (kbd "SPC <right>") 'next-buffer))


(add-hook 'xah-fly-command-mode-activate-hook 'my-xah-command-mode-keys)
(my-xah-command-mode-keys)

(provide 'xah-fly-keys-setup)

