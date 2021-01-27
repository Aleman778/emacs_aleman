;; Elisp functions used for compiling rust code

;; Rust compiler parameters

(setq rust-compile-command "rustc")
(setq rust-run-command "./")
(setq rust-cargo-command "cargo")
(setq rust-project-file "cargo.toml")

;; Compilation functions

(defun rust-project-root ()
  (am-locate-file (am-buffer-path) rust-project-file))

(defun rust-compile-and-run ()
  (interactive)
  (am-run-in (rust-project-root) rust-cargo-command "run"))

(add-hook 'rust-mode-hook
          (lambda ()

            ;; Build and run project (Alt-Ctrl-p)
            (local-set-key (kbd "M-C-p") 'rust-compile-and-run)))
