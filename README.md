# blox

`blox` is a package that provides functions to interact with Roblox tooling such as [Rojo](https://github.com/rojo-rbx/rojo) from the comfort of Emacs.

### Installation

To install, put `blox.el` somewhere on your `load-path` and `require` it. `blox.el` doesn't bind any keys by itself; assuming [lua-mode](https://github.com/immerrr/lua-mode) is installed and loaded, a configuration might look something like this:

```elisp
(require 'blox)

(define-key lua-prefix-mode-map (kbd "s")
            #'blox-rojo-serve)
(define-key lua-prefix-mode-map (kbd "b")
            #'blox-rojo-build)
(define-key lua-prefix-mode-map (kbd "t")
            #'blox-test)
```

Or with [use-package](https://github.com/jwiegley/use-package):

```elisp
(use-package blox
  :bind (:map lua-prefix-mode-map
              ("s" . blox-rojo-serve)
              ("b" . blox-rojo-build)
              ("t" . blox-test)))
```
