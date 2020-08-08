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
And that's it just start emacs and happy hacking!