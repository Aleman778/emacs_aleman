;; Generic interface for compiling/building code.

;; USAGE: e.g. (defun <major-mode>-compile-and-run ())
;; <major-mode> is replaced with the major mode in the current buffer.

;; Compilation configurations

(setq am-compile-default-mode "c++")

;; Generic compilation functions

(defun kill-program ()
  (interactive)
  (funcall-major-mode "kill-program"))

(defun custom-compile ()
  (interactive)
  (funcall-major-mode "custom-compile"))

(defun compile-and-run ()
  (interactive)
  (funcall-major-mode "compile-and-run"))

(defun run-program-debug ()
  (interactive)
  (funcall-major-mode "run-program-debug"))

(defun run-program-release ()
  (interactive)
  (funcall-major-mode "run-program-release"))

(defun run-program-dist ()
  (interactive)
  (funcall-major-mode "run-program-dist"))

(defun build-debug ()
  (interactive)
  (funcall-major-mode "build-debug"))

(defun build-release ()
  (interactive)
  (funcall-major-mode "build-release"))

(defun build-dist ()
  (interactive)
  (funcall-major-mode "build-dist"))

;; Utility functions

(defun am-open-compilation-buffer ()
  "Open the compilation buffer"
  (interactive)
  (switch-to-buffer "*compilation*")
  (make-frame))

(defun buffer-mode ()
  "Returns the major mode associated with a buffer."
  (nth 0 (split-string (symbol-name (with-current-buffer (current-buffer) major-mode)) "-")))

(defun funcall-major-mode (funcname)
  (setq func (concat (buffer-mode) "-" funcname))
  (setq def-func (concat default-major-mode "-" funcname))
  (let ((f-int (intern func))
        (f-def-int (intern def-func)))
    (if (fboundp f-int) (funcall f-int) (funcall f-def-int))))

(defun generate-project ()
  (interactive)
  (let ((default-directory project-directory))
    (async-shell-command (concat "GenerateProjects.bat " premake5-target)
                         (get-buffer "*compilation*") (get-buffer "*compilation*"))))

;; C++ compilation 
      
;; Kill program

(provide 'am-compile)
