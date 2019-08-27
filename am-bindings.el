;; Bind interactive elisp functios to keys commands.

;; Require functions that are suppose to be bound to keys
(require 'am-basic)
(require 'am-compile)
(require 'am-comment)
(require 'am-theme)

;; Goto specific line (Alt-g)
(global-set-key (kbd "M-g") 'goto-line)

;; Find the next error (Alt-n)
(global-set-key (kbd "M-n") 'next-error)

;; Find previous error (Alt-Ctrl-n)
(global-set-key (kbd "M-C-n") 'previous-error)

;; Open compilation buffer (Ctrl-c 8)
(global-set-key (kbd "C-c 8") 'am-open-compilation-buffer)

;; Evaluate current buffer (Alt-e)
(global-set-key (kbd "M-e") 'eval-buffer)

;; Move between buffers (Ctrl-./,)
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'am-back-window)

;; Ripgrep regex search (Alt-s)
(global-set-key (kbd "M-s") 'ripgrep-regexp)

;; Hippie expand (Alt-/)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Projectile find file (Alt-Ctrl-f)
(global-set-key (kbd "M-C-f") 'projectile-find-file)

;; Comment or uncomment line (Ctrl-])
(global-set-key (kbd "C-]") 'comment-line)

;; Insert region comment (Alt-])
(global-set-key (kbd "M-]") 'comment-region)

;; Insert documentation comment (Alt-Ctrl-k)
(global-set-key (kbd "M-C-k") 'am-insert-doc-comment)

;; Insert section comment (Alt-Ctrl-j)
(global-set-key (kbd "M-C-j") 'am-insert-section-comment)

;; Custom compile (Alt-i)
(global-set-key (kbd "M-i") 'custom-compile)

;; Build code in release mode (Alt-Ctrl-r)
(global-set-key (kbd "M-C-r") 'build-release)

;; Build code in debug mode (Alt-Ctrl-d)
(global-set-key (kbd "M-C-d") 'build-debug)

;; Kill the currently running program (Ctrl-c Ctrl-m)
(global-set-key (kbd "C-c C-m") 'kill-program)

;; Compile and run the program (Alt-Ctrl-u)
(global-set-key (kbd "M-C-u") 'compile-and-run)

;; Run the program in release mode (Alt-p)
(global-set-key (kbd "M-p") 'run-program-release)

;; Run the program in debug mode (Alt-Ctrl-p)
(global-set-key (kbd "M-C-p") 'run-program-debug)

;; Generate project files (Ctrl-c Ctrl-g)
(global-set-key (kbd "C-c C-g") 'generate-project)

;; Display electric buffer list (Ctrl-x l)
(global-set-key (kbd "C-x l") 'electric-buffer-list)

;; Change the current theme (Ctrl-c Ctrl-t)
(global-set-key (kbd "C-c C-t") 'am-change-theme)

;; Unbind transpose-chars, accidental presses causes problems!!!
(global-unset-key (kbd "C-t"))

(provide 'am-bindings)

