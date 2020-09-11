;;; config.el - Configs for compiling latex documents -*- lexical-binding: t; -*-

(defun compile-latex-pdf ()
  (interactive)
    (am-run-in (am-buffer-directory) "pdflatex" "-halt-on-error" "-output-directory=out" 
               "-aux-directory=temp" (am-buffer-file-name)))


(add-hook 'latex-mode-hook
          (lambda ()
            (local-set-key (kbd "M-C-p") 'compile-latex-pdf)))
