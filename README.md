# am-emacs-config
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

## Custom configurations
Inside the `config.el` file you can tinker and further customize. This file is loaded
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
