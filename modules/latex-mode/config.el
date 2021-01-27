;;; config.el - Configs for compiling latex documents -*- lexical-binding: t; -*-

(setq latex-main-file "main.tex") ; The main latex file to search for

(defun latex-dir ()
  (am-locate-file (am-buffer-path) latex-main-file))

(defun compile-latex-pdf ()
  (interactive)
  (am-run-in (latex-dir) "pdflatex" "-halt-on-error" "-output-directory=out" 
             "-aux-directory=temp" "main.tex"))

(defun compile-references-biber ()
  (interactive)
  (setq compilation-exit-callback 'compile-latex-pdf)
  (am-run-in (latex-dir) "biber" "temp/main"))

(defun compile-latex-pdf-with-biber ()
  (interactive)
  (setq compilation-exit-callback 'compile-references-biber)
  (compile-latex-pdf))

(add-hook 'latex-mode-hook
          (lambda ()
            (local-set-key (kbd "M-C-p") 'compile-latex-pdf)
            (local-set-key (kbd "M-C-u") 'compile-latex-pdf-with-biber)))
