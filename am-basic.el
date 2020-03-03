;; Basic emacs settings

;; Always use spaces!!!
(setq-default indent-tabs-mode nil)

;; Turn on auto fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Disable menu bar.
(menu-bar-mode -1)

;; Disable desktop save mode.
(desktop-save-mode -1)

;; Disable toolbar.
(tool-bar-mode -1)

;; Disable scroll bars.
(scroll-bar-mode -1)

;; Flash the screen instead of using sound.
(setq visible-bell t)

;; Disable user from reading compilation command.
(setq compilation-read-command nil)

;; Always scroll the the bottom of compilation buffer.
(setq compilation-scroll-output t)

;; Disable Ctrl-x l command.
(setq count-lines-page nil)

;; Insert new lines useing C-n if end of file.
(setq next-line-add-newlines nil)

;; Reuse buffer frames when using display buffers
(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

;; Set the backup directory
(setq backup-directory-alist `((".*" . "~/.saves/")))

;; Maximize the frame on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable ido mode
(ido-mode 1)

;; Enable projectile mode
(projectile-mode 1)

;; Disable ido mode from merging files from other directories
(setq ido-auto-merge-work-directories-length -1)

;; Add .h files to c++ mode instead of c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Add .sq files to rust-mode (temporary)
(add-to-list 'auto-mode-alist '("\\.sq\\'" . rust-mode))

;; Add .ejs files to html-mode
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))

;; Set background color to reduce blue light
(set-background-color "floral white")

;; Space shows possible files to open
(if (boundp 'minibuffer-local-filename-completion-map)
    (progn
      (define-key minibuffer-local-filename-completion-map " "
	'minibuffer-complete-word)
      (define-key minibuffer-local-must-match-filename-map " "
	'minibuffer-complete-word)))

;; Enable ansi-colors in compilation buffer.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Disable C-x l key bindings
(defun am-text-hook () 
  (define-key text-mode-map "\C-x l" nil))
(add-hook 'text-mode-hook 'am-text-hook)

;; Set focus to previous window
(defun am-back-window () 
  (interactive) 
  (other-window -1))


;; Enable multi-web-mode for embedded javascript/ php in html-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
      '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
        (js-mode  "<script[^>]*>" "</script>")
        (js-mode  "<%[-_=#%]?" "[-_]?%>")
        (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; Disable auto line breaks
(auto-fill-mode nil)

(provide 'am-basic)
