;;; config.el --- configs for C++ major-mode -*- lexical-binding: t; -*-


;; FIXME(alexander): should not be hardcoded like this, maybe use projectile to setup this.
(setq cpp-compiler "g++") ; The compiler to use.
(setq cpp-build-bin "make") ; The build binary.
(setq cpp-build-file "Makefile") ; The build file, directory search is used to find this file.
(setq cpp-build-output-bin "bin\\debug\\") ; The output from building project.
(setq cpp-run-default-file "sqrrlc.exe") ; The default filename for built executable.
(setq cpp-build-directory "") ; This is automatically updated each build.
(setq cpp-generate-project-bin "generate_project.bat gmake") ; Generate build files binary e.g. Makefiles.
(setq cpp-generate-project-file "generate_project.bat") ; Searches for this specific file.
(setq cpp-executable-ext "") ; The command used to run file prefix, e.g. unix use "./".
(setq cpp-executable-ext ".exe") ; The command used to run file prefix, e.g. windows has ".exe".


(defun cpp-get-exe (file)
  (am-file-extension file cpp-executable-ext))


(defun cpp-build-dir ()
  (am-locate-file (am-buffer-path) cpp-build-file))


(defun cpp-compile-buffer-file ()
  (interactive)
  (am-run "cpp-compile-buffer-file" cpp-compiler (am-buffer-file-name) "-o" (cpp-get-exe (am-buffer-file-name))))


(defun cpp-run-buffer-file ()
  (interactive)
  (am-shell "cpp-run-buffer-file" (cpp-get-exe (am-buffer-file-name))))
  

(defun cpp-compile-run-buffer-file ()
  (interactive)
  (setq compilation-exit-callback (cpp-run-buffer-file))
  (cpp-compile-buffer-file))


(defun cpp-generate-build-files ()
  (interactive)
  (am-run-in (cpp-build-dir) "cpp-generate-build-files" cpp-generate-project-bin))


(defun cpp-build-project (options)
  (interactive)
  (am-run-in (cpp-build-dir) "cpp-build-project" cpp-build-bin options))
  

(defun cpp-run-project ()
  (interactive)
  (am-shell-in (cpp-build-dir) "cpp-run-project" (concat cpp-build-output-bin cpp-run-default-file)))
  

(defun cpp-build-project-default ()
  (interactive)
  (am-run-in (cpp-build-dir) "cpp-build-project" cpp-build-bin))


(defun cpp-build-run-default ()
  (interactive)
  (setq compilation-exit-callback 'cpp-run-project)
  (cpp-build-project-default))
  

;; Check if pos is inside C++ enum class
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ enum class"
  (ignore-errors
    (save-excursion
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

;; Check if pos is inside C++ class
(defun inside-class-p (pos)
  "Checks if POS is within the braces of a C++ class"
  (ignore-errors
    (save-excursion
      (looking-back "class[ \t]+[^}]+"))))

;; C++ linup under anchor used for fixing indentation problems
(defun am-c-lineup-under-anchor (langlem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      'c-lineup-under-anchor
    (if (inside-class-p (c-langelem-pos langlem)) '+ 0)))

;; Setup indentation style C++
(c-add-style "microsoft"
             '("stroustrup"
               (c-offsets-alist
                (innamespace . +)
                (inline-open . 0)
                (access-label . -)
                (inclass . +)
                (inher-cont . c-lineup-multi-inher)
                (arglist-cont-nonempty . +)
                (brace-list-open . +)
                (brace-list-entry . am-c-lineup-under-anchor)
                (template-args-cont . +)
                (comment-intro . 0)
                (member-init-intro . +))))
(setq c-default-style "microsoft")


;; C++ specific keybinds
(add-hook 'c++-mode-hook
          (lambda ()

            ;; Build and run project (Alt-Ctrl-p)
            (local-set-key (kbd "M-C-p") 'cpp-build-run-default)

            ;; Kill the currently running program (Ctrl-c Ctrl-m)
            (local-set-key (kbd "C-c C-m") 'kill-compilation)

            ;; Generate project files (Alt-Ctrl-u)
            (local-set-key (kbd "M-C-u") 'cpp-generate-build-files)

            ;; Run the specific file (Alt-p)
            (local-set-key (kbd "M-p") 'cpp-compile-run-buffer-file)))
