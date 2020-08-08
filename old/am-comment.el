;; Functions for inserting different types of comments


;; Inserting a section comment
(defun am-insert-section-comment ()
  "Inserts a C++ style section comment."
  (interactive)
  (if (am-inside-comment)
      (error "Already inside comment.")
    (beginning-of-line)
    (open-line 1)
    (indent-according-to-mode)
    (insert "/")
    (let* ((indentation (current-column))
           (star-str (make-string (- 76 indentation)?*)))
      (insert star-str "\n")
      (indent-to indentation)
      (insert "* \n")
      (indent-to indentation)
      (insert star-str "/")
      (end-of-line 0))))


;; Inserting a documentation comment
(defun am-insert-doc-comment ()
  "Inserts a C++ style documentation comment."
  (interactive)
  (if (am-inside-comment)
      (error "Already inside comment.")
    (beginning-of-line)
    (open-line 1)
    (indent-according-to-mode)
    (let* ((indentation (current-column)))
      (insert "/**\n")
      (indent-to indentation)
      (insert " * \n")
      (indent-to indentation)
      (insert " */")
      (end-of-line 0))))


;; Check if cusor is already inside a comment
(defun am-inside-comment ()
  "Test if cursor/point in a commented line?"
  (save-excursion
        (if (derived-mode-p 'org-mode)
                (save-match-data (beginning-of-line) (looking-at "^[ \t]*#"))
          (nth 4 (syntax-ppss)))))


(provide 'am-comment)
