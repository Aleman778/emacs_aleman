;;; init.el --- define modules to use -*- lexical-binding: t; -*-

(am-modules!
    :keybinds
    ;; evil                ; the extensible vi layer for emacs (not configured)
    xah-fly-keys        ; the most efficient (vim like) keybinding

    :search	        
    ido                 ; search engine included now in emacs
    ;; ivy	                ; another search engine

    :complete	        
    company             ; modular in-buffer completion framework (not working atm.)

    :utils
    hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
    rg	                ; searches directories for a regex pattern
    which-key           ; lists all keybinds available
    minions             ; hide minor-modes in the modeline
    doom-modeline       ; modeline from doom emacs, very pretty
    ;; smartparens         ; smart minor mode that deals with parens pairs (not configured)
    ;; pdf-tools           ; adds support for viewing pdfs inside of emacs

    :lang
    c++-mode            ; major-mode for C++ programming language
    rust-mode           ; major-mode for Rust programming language
    go-mode             ; major-mode for Go programming language
    python-mode         ; major-mode for python programming language
    glsl-mode           ; major-mode for opengl shading language
    latex-mode          ; major-mode for writing latex documents
    sass-mode           ; major-mode for SASS styling preprocessor

    :themes	        
    doom-themes         ; doom emacs themes
)
