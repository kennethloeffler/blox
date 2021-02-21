# blox [![MELPA](https://melpa.org/packages/blox-badge.svg)](https://melpa.org/#/blox)

Blox is a package that provides functions to interact with Roblox tooling such as [Rojo](https://github.com/rojo-rbx/rojo) from the comfort of Emacs.

### Installation

Blox is on [melpa](https://melpa.org) and can be installed with your package manager of choice.

Blox doesn't bind any keys by itself. Assuming that [lua-mode](https://github.com/immerrr/lua-mode) is installed, a configuration might look something like this:

```elisp
(require 'blox)

(define-key lua-prefix-mode-map (kbd "s") #'blox-prompt-serve)
(define-key lua-prefix-mode-map (kbd "b") #'blox-prompt-build)
(define-key lua-prefix-mode-map (kbd "t") #'blox-test)
```

Or with [use-package](https://github.com/jwiegley/use-package):

```elisp
(use-package blox
  :bind (:map lua-prefix-mode-map
              ("s" . blox-prompt-serve)
              ("b" . blox-prompt-build)
              ("t" . blox-test)))
```
