;; Compile latex files into pdfs.

(defun compile-latex-pdf ()
  (interactive)
  (let ((dir (file-name-directory (latex-current-file)))
        (filename (file-name-nondirectory (latex-current-file))))
    (tex-compile dir (concat "pdflatex -halt-on-error -output-directory=out -aux-directory=temp " filename))))


(defun latex-current-file ()
  "Returns the file name of the current buffer"
  (buffer-file-name (window-buffer (minibuffer-selected-window))))

(provide 'latex-compile)
