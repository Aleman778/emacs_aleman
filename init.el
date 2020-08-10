;;; init.el --- define modules to use -*- lexical-binding: t; -*-


(am-modules!
    :keybinds
    ;; evil                ; the extensible vi layer for emacs
    xah-fly-keys        ; the most efficient (vim like) keybinding

    :search	        
    ido                 ; search engine included now in emacs
    ;; ivy	                ; another search engine

    :complete	        
    company             ; modular in-buffer completion framework

    :utils
    hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
    rg	                ; searches directories for a regex pattern
    which-key           ; lists all keybinds available
    minions             ; hide minor-modes in the modeline
    doom-modeline       ; modeline from doom emacs, very pretty
    smartparens         ; smart minor mode that deals with parens pairs
    ;; dired               ; making dired pretty (functional)
    ;; electric            ; smarter, keyword-based electric-indent
    ;; ibuffer             ; interactive buffer management
    ;; undo                ; persistent, smarter undo for your inevitable mistakes
    ;; vc                  ; version-control and Emacs, sitting in a tree
    ;; syntax              ; marks any incorrect easy syntax errors

    :themes	        
    dracula-theme       ; dracula theme
)
