;; Elisp function used for compiling (handling) node js server

;; Node JS compiler parameters

(setq nodejs-command "node")
(setq nodejs-server-fname "server.js")
(setq nodejs-cached-project nil)
(setq nodejs-running nil)


;; Compilation functions

(defun nodejs-start-server ()
  "Starts a node server on the compile buffer"
  (interactive)
  (if (eq nodejs-cached-project nil)
      (setq nodejs-cached-project (nodejs-find-server-folder)))
  (setq compilation-exit-message-function 'nodejs-server-stopped)
  (let ((default-directory nodejs-cached-project))
    (setq command (concat nodejs-command " " nodejs-cached-project nodejs-server-fname))
    (if (eq nodejs-running t) (kill-compilation))
    (async-shell-command command (get-buffer "*compilation*") (get-buffer "*compilation*"))
    (setq nodejs-running t)))


(defun nodejs-server-stopped (status code msg)
  "Notifiy that the server is stopped"
  (setq nodejs-running nil))


(defun nodejs-current-file ()
  "Returns the file name of the current buffer"
  (interactive)
  (print (buffer-file-name (window-buffer (minibuffer-selected-window)))))


(defun nodejs-find-server-folder ()
  "Tries to find where the server is located, asks if not found"
  (let ((prj-folder (file-name-directory (nodejs-current-file))))
    (while (not (eq (file-exists-p (concat prj-folder nodejs-server-fname)) t))
      (setq prj-folder (file-name-directory (substring prj-folder 0 -1))))
    prj-folder))


(provide 'nodejs-compile)
