;;; am-keybinds --- defines keybinds for convenience -*- lexical-binding: t; -*- 


(global-set-key (kbd "M-g") 'goto-line)                    ;; Goto specific line (Alt-g)
(global-set-key (kbd "M-n") 'next-error)                   ;; Find the next error (Alt-n)
(global-set-key (kbd "M-C-n") 'previous-error)             ;; Find previous error (Alt-Ctrl-n)
(global-set-key (kbd "C-c 8") 'am-open-compilation-buffer) ;; Open compilation buffer (Ctrl-c 8)
(global-set-key (kbd "C-M-'") 'goto-line)                  ;; Goto line
(global-set-key (kbd "C-.") 'other-window)                 ;; Move to next buffer (Ctrl-.
(global-set-key (kbd "C-,") 'am-back-window)               ;; Move to previous buffer (Ctrl-,)
(global-set-key (kbd "M-s") 'rg)                           ;; Ripgrep regex search (Alt-s)
(global-set-key (kbd "M-/") 'hippie-expand)                ;; Hippie expand (Alt-/)
(global-set-key (kbd "M-C-f") 'projectile-find-file)       ;; Projectile find file (Alt-Ctrl-f)
(global-set-key (kbd "C-]") 'comment-line)                 ;; Comment or uncomment line (Ctrl-])
(global-set-key (kbd "M-]") 'comment-region)               ;; Insert region comment (Alt-])
(global-set-key (kbd "M-C-k") 'am-insert-doc-comment)      ;; Insert documentation comment (Alt-Ctrl-k)
(global-set-key (kbd "M-C-j") 'am-insert-section-comment)  ;; Insert section comment (Alt-Ctrl-j)
(global-set-key (kbd "C-x l") 'electric-buffer-list)       ;; Display electric buffer list (Ctrl-x l)
(global-set-key (kbd "C-c C-t") 'am-change-theme)          ;; Change the current theme (Ctrl-c Ctrl-t)
(global-set-key (kbd "C-c s") 'magit-status)               ;; Magit Status
(global-unset-key (kbd "C-t"))                             ;; Unbind transpose-chars, accidental presses!
(global-unset-key (kbd "C-x C-c"))                         ;; Unbind kill-emacs, accidental presses kills emacs!!!


(provide 'am-keybinds)
