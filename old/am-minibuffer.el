

(defun minibuffer-input-provider (inputs)
  (let ((hook (make-symbol "hook")))
    (fset hook (lambda ()
                 (remove-hook 'post-command-hook hook)
                 (when inputs
                   (when (= 0 (minibuffer-depth))
                     (error "Too many inputs"))
                   (when (cdr inputs)
                     (add-hook 'post-command-hook hook))
                   (insert (pop inputs))
                   (exit-minibuffer))))
    (add-hook 'post-command-hook hook)))


(with-minibuffer-input (call-interactively 'find-file)
  "/")

(with-minibuffer-input (call-interactively 'occur)
  "\\(foo\\)\\(bar\\)" "\\1");;C-u C-x C-e

(with-minibuffer-input (call-interactively 'replace-string)
                       "foo" "bar")

(provide am-minibuffer)
