# am-emacs-config
These are my emacs lisp configuration files, please feel free to use them.
At the moment I have only configured my emacs for C++ programming.
More major-modes will be added later when I require them my self.

## How to setup
It is very easy to setup simply clone this repository anywhere on your desktop.
In order to run the lisp code simply add the following code to your `~/.emacs` file
```elisp
(add-to-list 'load-path "<path-to-cloned-git-repo>")
(require 'am-bindings)
```
where `<path-to-cloned-git-repo>` should be the path to the directory where you cloned this repository (i.e. the directory where the .el files are located). Take a look in the `am-bindings.el` for all the key bindings I have choosen.

## Further customizations
There are some more customizations that can be done to further improve the emacs experiance.

### Set the layout
By default emacs starts with one empty buffer but if you have a larger monitor it is useful to split the frame into multiple frames which allows you to view multiple buffers at the same time. Emacs does not save the edited layout be default so in order to avoid having to customize the layout manually I have included different layout options that can be set in your init file. There are two different layout options `am-layout-three-columns` and `am-layout-two-columns`. These functions also takes in locations to files that are opened when calling the function. Here is an example
```elisp
(require 'am-layout)
(am-layout-three-columns "c:/dev/am-engine/CoreEngine/src/core/Application.cpp"
                         "c:/dev/am-engine/CoreEngine/src/core/Application.h"
                         "c:/dev/am-engine/CoreEngine/src/core/Core.h")

```
note that this code needs to be loaded by calling `(require 'am-layout)`.

### Change themes
With the press of `Ctrl-c Ctrl-t` you can change the theme. But no themes are included in this repository. You can install any available emacs theme. If you want to be able to swap themes using the keyboard shortcut then simply add this code after the initialization code.
```elisp
(add-to-list 'am-theme-alist 'your-theme)
```
where 'your-theme is the reference to the theme.

### My current ~/.emacs file
```elisp
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (clang-format spacemacs-theme magit wgrep-ag wgrep ripgrep))))

;; Set to smaller font than the original.
(set-frame-font "-outline-Courier New-normal-normal-normal-mono-12-*-*-*-c-*-iso8859-1" nil t)

;; Setup my custom emacs scripts.
(add-to-list 'load-path "c:/dev/am-emacs-config")
(require 'am-bindings)

;; Use three sepearte buffers as default layout.
(require 'am-layout)
(am-layout-three-columns "c:/dev/am-engine/CoreEngine/src/core/Application.cpp"
                         "c:/dev/am-engine/CoreEngine/src/core/Application.h"
                         "c:/dev/am-engine/CoreEngine/src/core/Core.h")

;; Add spacemacs dark as a custom theme.
(require 'spacemacs-common)
(add-to-list 'am-theme-alist 'spacemacs-dark)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
```
