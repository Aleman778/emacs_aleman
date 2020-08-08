;; Layout script defines some basic layout options for emacs

(defun am-layout-two-columns (file1 file2)
  "Creates a layout with two columns the right column has two buffers. Loads two files."
  (delete-other-windows)
  (find-file file1)
  (split-window-right)
  (other-window 1)
  (find-file file2)
  (split-window-below)
  (if (eq (get-buffer "*compilation*") nil) (get-buffer-create "*compilation*") nil)
  (enlarge-window (/ (window-height (next-window)) 2))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (other-window 1))


(defun am-layout-three-columns (file1 file2 file3)
  "Creates a layout with three columns. Loads three files."
  (delete-other-windows)
  (find-file file1)
  (split-window-right)
  (other-window 1)
  (enlarge-window-horizontally (/ (window-width (next-window)) 3))
  (find-file file2)
  (split-window-right)
  (other-window 1)
  (find-file file3)
  (split-window-below)
  (if (eq (get-buffer "*compilation*") nil) (get-buffer-create "*compilation*") nil)
  (balance-windows)
  (enlarge-window (/ (window-height (next-window)) 2))
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (other-window 1))


(provide 'am-layout)
