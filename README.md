# My Emacs Configurations
These are my emacs lisp configuration files, please feel free to use them.
At the moment I have only configured my emacs for C++ programming.
More major-modes will be added later when I require them my self.

## Installation
Clone the repository into either your `~/.emacs.d/` directory or anywhere else that fits.
Note: cloning outside of the default emacs directory requires that the `config.el` and `packages.el`
are moved into the `~/.emacs.d/` folder.

Now in your `~/.emacs` file put the following code:
```elisp
(add-to-list 'load-path "path/to/clone") ; only needed if clone is outside ~/.emacs.d/

(require 'am-core)
(am-initialize)
```
Before restarting emacs please take a look inside the `packages.el` file and turn on and off
packages that you wish to use. Note that the first time you start Emacs it will take a few minutes
but the next time it will go very fast!
And that's it just start emacs and happy hacking!

## Custom Configurations
Inside the `~/.emacs.d/config.el` file you can tinker and further customize. This file is loaded
at the end of `am-initialize` function. Here is what my config looks like:
```elisp
;;; config.el --- your own custom tinkering can be done here -*- lexical-binding: t; -*-

;; Add dracula as custom theme
(add-to-list 'am-theme-alist 'dracula)

;; Use Source Code Pro font
(set-frame-font "Source Code Pro 9")

;; Add .h files to c++ mode instead of c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Add .sq files to c++-mode (temporary)
(add-to-list 'auto-mode-alist '("\\.sq\\'" . c++-mode)) 

;; Setup default layout
(am-layout-two-columns "c:/dev/sqrrl/src/parser.cpp" "c:/dev/sqrrl/src/main.cpp")
```

### Initialization of modules
In `~/.emacs.d/init.el` you can specify this code which will load the modules you want to use.
Below you find an example of how this can be performed, just comment out anything you don't want.
```
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

    :themes	        
    doom-themes         ; doom emacs themes
)
```