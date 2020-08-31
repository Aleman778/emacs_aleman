;;; am-compile.el --- helper functions for compiling code -*- lexical-binding: t; -*-


(defun am-compilation-exit (status code msg) 
  (if (and (eq status 'exit) (zerop code))
      (if compilation-exit-callback (funcall compilation-exit-callback) nil) nil)
  (setq compilation-exit-message-function nil))


(setq compilation-exit-callback nil)
(setq compilation-exit-message-function 'am-compilation-exit)
(setq cpp-generate-project-bin "generate_project.bat gmake") ; Generate build files binary e.g. Makefiles.

(defun am-build-command (args)
  (let* ((command ""))
    (while args
      (let ((arg (pop args)))
        (cond ((stringp arg)
               (setq command (concat command arg " ")))
              ((symbolp arg)
               (setq command (concat command (symbol-value arg) " ")))
              ((functionp arg)
               (setq command (concat command (funcall arg) " ")))
              ((keywordp arg)
               (pcase arg
                 (:path  (setq command (concat command (am-buffer-path) " ")))
                 (:file  (setq command (concat command (am-buffer-file-name) " ")))
                 (:dir   (setq command (concat command (am-buffer-directory) " ")))
                 (:after (setq compilation-exit-callback (pop args))))))))
    command))

(defun am-run-command (dir command)
  (let ((default-directory dir))
    (compile command)))


(defun am-run! (&rest args)
  (am-run-command (am-buffer-directory) (am-build-command args)))


(defun am-run-in! (dir &rest args)
  (am-run-command dir (am-build-command args)))


(provide 'am-compile)
