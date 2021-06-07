# flycheck-quicklintjs Emacs plugin

This directory contains a plugin for the [Emacs text editor][Emacs].

This plugin integrates with other Emacs plugins to show lint rules when editing
files in Emacs.

**Important**: Installing this Emacs plugin will not also install quick-lint-js
itself. You must separately install quick-lint-js in order for this plugin to
work.

Flycheck plugin must be installed and configured in order for this plugin to
work:

* [Flycheck] - On the fly syntax checking, version 31 or newer

## Installation

### Manually

You need to copy flycheck-quicklintjs.el inside a folder present in Emacs
load-path, a default one that is looked up by Emacs is
`/usr/local/share/emacs/site-lisp` but requires root privileges, alternatively
you may append a custom folder of your preference containing the
`flycheck-quicklintjs.el` file to Emacs load-path variable, for example:

```lisp
(add-to-list 'load-path "~/.emacs/quicklintjs")
```

A less convenient way also possible is appending to `load-path` with Emacs
command line argument `-L folder`

### Initialization

When Emacs is able to find the quick-lint-js library, you need to load it,
your [Emacs Initialization] file is a good place:

```lisp
(require 'flycheck)
(require 'flycheck-quicklintjs)
```

At this point quick-lint-js should be registered as a checker, which you can
verify with `M-x flycheck-verify-setup`, optionally we suggest that you add
this hook to `js-mode`, which will remove the delay after you type that
Flycheck waits before running the checker, also registers `quick-lint-js`
as the selected checker by default, instead of `eslint`.

```lisp
(defun my-flycheck-quicklintjs-setup ()
  "Configure flycheck-quicklintjs for better experience."

  ;; Enable Flycheck
  (unless (bound-and-true-p flycheck-mode)
    (flycheck-mode))

  ;; Use quick-lint-js by default when in 'js-mode`
  (flycheck-select-checker 'javascript-quicklintjs)

  ;; Remove any delay after a change in buffer to run checkers.
  ;; The default is 0.5 (500ms)
  (setq-local flycheck-idle-change-delay 0)

  ;; Run quick-lint-js program when the buffer is changed and when 'js-mode` is
  ;; loaded
  (setq-local flycheck-check-syntax-automatically '(mode-enabled idle-change)))
(add-hook 'js-mode-hook #'my-flycheck-quicklintjs-setup)
```

[Flycheck]: https://www.flycheck.org/
[Emacs]: https://www.gnu.org/software/emacs/
[Emacs Initialization]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
