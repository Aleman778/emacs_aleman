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


  ;; Kill current buffer (command mode: SPACE k j)
  (define-key xah-fly-key-map (kbd "SPC k j") 'kill-current-buffer)


  ;; Electric buffer mode (command mode: SPACE i d)
  (define-key xah-fly-key-map (kbd "SPC o t") 'kmacro-end-and-call-macro)


  ;; Next error (command mode: \)
  (define-key xah-fly-key-map (kbd "\\") 'next-error)


  ;; Previous error (command mode: ')
  (define-key xah-fly-key-map (kbd "h") 'previous-error)
  

  ;; Make escape key do Ctrl-g (quit)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))


  ;; Copy rectangle as kill (command mode: SPACE o c)
  (define-key xah-fly-key-map (kbd "SPC o c") 'copy-rectangle-as-kill)


  ;; Yank rectangle (command mode: SPACE o v)
  (define-key xah-fly-key-map (kbd "SPC o v") 'yank-rectangle)


  ;; Kill rectangle (command mode: SPACE o x)
  (define-key xah-fly-key-map (kbd "SPC o x") 'kill-rectangle)


  ;; Ido goto a specific symbol (command mode: SPACE i w)
  (define-key xah-fly-key-map (kbd "SPC i w") 'ido-goto-symbol)


  ;; Navigation keys (shifted right by one)
  (define-key xah-fly-key-map (kbd "k") 'backward-char)
  (define-key xah-fly-key-map (kbd "i") 'backward-word)
  (define-key xah-fly-key-map (kbd "l") 'next-line)
  (define-key xah-fly-key-map (kbd "o") 'previous-line)
  (define-key xah-fly-key-map (kbd ";") 'forward-char)
  (define-key xah-fly-key-map (kbd "p") 'forward-word)
  (define-key xah-fly-key-map (kbd "j") 'xah-beginning-of-line-or-block)
  (define-key xah-fly-key-map (kbd "'") 'xah-end-of-line-or-block)

  ;; Moved insert space (shifted right by one)
  (define-key xah-fly-key-map (kbd "[") 'xah-insert-space-before)

  ;; Next/ previous buffer
  (define-key xah-fly-key-map (kbd "SPC <left>") 'previous-buffer)
  (define-key xah-fly-key-map (kbd "SPC <right>") 'next-buffer)
  (define-key xah-fly-key-map (kbd "u") 'recenter-top-bottom)

  ;; Translate compile command to SPC p
  (define-key key-translation-map (kbd "SPC p") (kbd "M-C-p")))



(add-hook 'xah-fly-command-mode-activate-hook 'my-xah-command-mode-keys)
(my-xah-command-mode-keys)

(provide 'xah-fly-keys-setup)

