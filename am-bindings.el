;; Bind interactive elisp functios to keys commands.

;; Require functions that are suppose to be bound to keys
(require 'am-basic)
(require 'am-delete)
(require 'am-comment)
(require 'am-theme)
(require 'cpp-compile)
(require 'rust-compile)
(require 'nodejs-compile)
(require 'python-compile)

;; Goto specific line (Alt-g)
(global-set-key (kbd "M-g") 'goto-line)

;; Find the next error (Alt-n)
(global-set-key (kbd "M-n") 'next-error)

;; Find previous error (Alt-Ctrl-n)
(global-set-key (kbd "M-C-n") 'previous-error)

;; Open compilation buffer (Ctrl-c 8)
(global-set-key (kbd "C-c 8") 'am-open-compilation-buffer)

;; Goto line
(global-set-key (kbd "C-M-'") 'goto-line)

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

;; Display electric buffer list (Ctrl-x l)
(global-set-key (kbd "C-x l") 'electric-buffer-list)

;; Change the current theme (Ctrl-c Ctrl-t)
(global-set-key (kbd "C-c C-t") 'am-change-theme)

;; Magit Status
(global-set-key (kbd "C-c s") 'magit-status)

;; Unbind transpose-chars, accidental presses causes problems!!!
(global-unset-key (kbd "C-t"))

;; Custom delete line backwards (Ctrl-Shift-k)
(global-set-key (kbd "C-S-k") 'my-delete-line-backward)

;; Custom delete line (Ctrl-k)
(global-set-key (kbd "C-k") 'my-delete-line)

;; Custom delete word (Alt-d)
(global-set-key (kbd "M-d") 'my-delete-word)

;; Custom backwards delete word (Alt/Ctrl-backspace
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)

;; Unbind kill-emacs, accidental key presses kills emacs!!!
(global-unset-key (kbd "C-x C-c"))

;; Bind major-mode specific elisp functions to local key commands

;; Elisp specific key bindings
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "M-e") 'eval-buffer)))

           
;; C++ specific key bindings
(add-hook 'c++-mode-hook
          (lambda ()
            ;; Custom compile (Alt-i)
            (local-set-key (kbd "M-i") 'cpp-custom-compile)

            ;; Build code in release mode (Alt-Ctrl-r)
            (local-set-key (kbd "M-C-r") 'cpp-build-release)

            ;; Build code in debug mode (Alt-Ctrl-d)
            (local-set-key (kbd "M-C-d") 'cpp-build-debug)

            ;; Kill the currently running program (Ctrl-c Ctrl-m)
            (local-set-key (kbd "C-c C-m") 'cpp-kill-program)

            ;; Compile and run the program (Alt-Ctrl-u)
            (local-set-key (kbd "M-C-u") 'cpp-compile-and-run)

            ;; Run the program in release mode (Alt-p)
            (local-set-key (kbd "M-p") 'cpp-run-program-release)

            ;; Run the program in debug mode (Alt-Ctrl-p)
            (local-set-key (kbd "M-C-p") 'cpp-run-program-debug)

            ;; Generate project files (Ctrl-c Ctrl-g)
            (local-set-key (kbd "C-c C-g") 'cpp-generate-project)))


;; Rust specific key bindings
(add-hook 'rust-mode-hook
          (lambda ()
            ;; Build/compile and run current project or file (Alt-Ctrl-u)
            (local-set-key (kbd "M-C-u") 'rust-compile-and-run)

            ;; Compile and run the current file (Ctrl-c c)
            (local-set-key (kbd "C-c c") 'rust-compile-and-run-file)
            
            ;; Build and run the current project (Ctrl-c b)
            (local-set-key (kbd "C-c b") 'rust-cargo-build-and-run)
            
            ;; Run current project a file (Alt-Ctrl-p)
            (local-set-key (kbd "M-C-p") 'rust-run)

            ;; Compile current file (Alt-p)
            (local-set-key (kbd "M-p") 'rust-run-current-file)

            ;; Run test cases for current project (Ctrl-c v)
            (local-set-key (kbd "C-c v") 'rust-test)))


;; Javascript specific key bindings
(add-hook 'javascript-mode-hook
          (lambda ()
            ;; Start the Node JS server 
            (local-set-key (kbd "M-C-p") 'nodejs-start-server)))


;; Python specific key bindings
(add-hook 'python-mode-hook
          (lambda ()
            ;; Execute the current python script (Alt-Ctrl-p)
            (local-set-key (kbd "M-C-p") 'python-execute-current)))


(provide 'am-bindings)

