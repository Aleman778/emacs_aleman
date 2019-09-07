;; Elisp functions used for compiling rust code

;; Rust compiler parameters

(setq rust-compile-command "rustc")
(setq rust-run-command "./")
(setq rust-cargo-command "cargo")


;; Compilation functions

(defun rust-compile-and-run ()
  "Compiles the current buffers rust file or project and then runs the comiled executable."
  (interactive)
  (setq rust-cargo-project (rust-find-cargo-folder (rust-current-file)))
  (if (eq rust-cargo-project nil) (rust-compile-and-run-file) (rust-cargo-build-and-run)))


(defun rust-compile-and-run-file ()
  "Compiles and runs the currently active file"
  (interactive)
  (setq compilation-exit-message-function 'rust-run-file-if-compilation-ok)
  (rust-compile-file (rust-current-file)))


(defun rust-cargo-build-and-run ()
  "Builds and runs the currently active project"
  (interactive)
  (if (eq rust-cargo-project nil)
      (setq rust-cargo-project (rust-find-cargo-folder (rust-current-file))))
  (setq compilation-exit-message-function 'rust-cargo-run-if-compilation-ok)
  (rust-cargo-build rust-cargo-project))


(defun rust-run ()
  "Runs the current buffers rust file or project."
  (interactive)
  (let ((project (rust-find-cargo-folder (rust-current-file)))
        (example (rust-find-examples-folder (rust-current-file))))
    (if (eq project nil) (if (eq example nil) (rust-run-file (rust-current-file))
                           (rust-cargo-example example (file-name-sans-extension (file-name-nondirectory (rust-current-file)))))
      (progn (setq rust-cargo-project project)
             (rust-cargo-build-and-run)))))


(defun rust-run-current-file ()
  "Runs the current buffers rust file always even if it belongs to a cargo project."
  (interactive)
  (rust-run-file (rust-current-file)))


(defun rust-test ()
  "Runs the test cases in the current buffer rust project"
  (interactive)
  (setq rust-cargo-project (rust-find-cargo-folder (rust-current-file)))
  (if (eq rust-cargo-project nil)
      (error "Not inside a rust cargo module.")
    (rust-cargo-test rust-cargo-project)))


;; Standard compile file functions

(defun rust-compile-file (file)
  "Compiles a specific rust file"
  (compile (concat rust-compile-command " " (file-name-nondirectory file))))


(defun rust-run-file (file)
  "Run a specific rust file"
  (async-shell-command (concat rust-run-command
                               (file-name-nondirectory (file-name-sans-extension file)))
                       (get-buffer "*compilation*") (get-buffer "*compilation*")))


;; Standard cargo build functions

(defun rust-cargo-build (project)
  "Builds the rust project, using the cargo build system"
  (let ((default-directory project))
    (compile (concat rust-cargo-command " build"))))


(defun rust-cargo-build-release (project)
  "Builds the rust project in release mode, using the cargo build system"
  (let ((default-directory project))
    (compile (concat rust-cargo-command " build --release"))))
  

(defun rust-cargo-run (project)
  "Runs the rust project, using the cargo build system"
  (let ((default-directory project))
    (async-shell-command (concat rust-cargo-command " run")
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))


(defun rust-cargo-check (project)
  "Checks the rust project, using the cargo build system"
  (let ((default-directory project))
    (async-shell-command (concat rust-cargo-command " check")
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))


(defun rust-cargo-test (project)
  "Run test cases in the rust project, using the cargo build system"
  (let ((default-directory project))
    (async-shell-command (concat rust-cargo-command " test")
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))


(defun rust-cargo-example (project example)
  "Run specific example in the rust project, using the cargo build system"
  (let ((default-directory project))
    (async-shell-command (concat rust-cargo-command " run --example " example)
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))

  

;; Utility functions

(defun rust-run-file-if-compilation-ok (status code msg)
  "Checks if the given compilation was successfull"
  (if (and (eq status 'exit) (zerop code))
      (rust-run-file (rust-current-file)))
  (cons msg code))


(defun rust-cargo-run-if-compilation-ok (status code msg)
  "Checks if the given compilation was successfull"
  (if (and (eq status 'exit) (zerop code))
      (rust-cargo-run rust-cargo-project))
  (cons msg code))


(defun rust-current-file ()
  "Returns the file name of the current buffer"
  (buffer-file-name (window-buffer (minibuffer-selected-window))))


(defun rust-find-cargo-folder (file)
  "Tries to find the cargo project file (Cargo.toml)"
  (let ((prj-folder (substring file 0 (string-match-p (regexp-quote "src") file))))
    (if (eq prj-folder nil) nil
      (if (eq (file-exists-p (concat prj-folder "Cargo.toml")) t)
          (substring prj-folder 0 -1) nil))))


(defun rust-find-examples-folder (file)
  (let ((ex-folder (substring file 0 (string-match-p (regexp-quote "examples") file))))
    (if (eq ex-folder nil) nil ex-folder)))

(provide 'rust-compile)
