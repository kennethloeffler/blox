# blox.el

`blox.el` is a package that provides functions to interact with Roblox tooling from the comfort of Emacs.

### Installation

To install, put `blox.el` somewhere on your `load-path` and `require` it (or equivalent). `blox.el` doesn't bind any keys by itself - a configuration might look something like this (with `use-package`):

```elisp
(use-package blox
  :bind (:map lua-prefix-mode-map
              ("s" . blox-rojo-serve)
              ("b" . blox-rojo-build)
              ("t" . blox-test)))
```
