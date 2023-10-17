;;; config.el --- your own custom tinkering can be done here -*- lexical-binding: t; -*-

;; Add dracula as custom theme
(setq am-theme-alist (list 'doom-one 'doom-one-light))
(setq am-background-alist (list nil 'floralwhite))
(am-change-theme)

;; Use Source Code Pro font
(set-frame-font "Source Code Pro 9")

;; Add .h files to c++ mode instead of c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Add .sq files to c++-mode (temporary)
(add-to-list 'auto-mode-alist '("\\.sq\\'" . c++-mode)) 

;; Setup default layout
(am-layout-three-columns "c:/dev/sqrrl/code/sqrrl_interp.cpp" "c:/dev/sqrrl/code/sqrrl_parser.h" "c:/dev/sqrrl/code/sqrrl_ast.h")


(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
