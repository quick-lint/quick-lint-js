# Emacs plugin

This directory contains quick-lint-js integration for [GNU Emacs][Emacs] plugins
ecosystem including [Flymake], [Flycheck], [Eglot] and [LSP Mode].

## Installation

**Important**: The following steps will not also install quick-lint-js itself.
You must separately install [quick-lint-js](https://quick-lint-js.com/install)
in order for this plugin to work.

Copy which plugin integration you like inside a folder present in Emacs
`load-path`, the plugin integration files are:
- eglot-quicklintjs.el
- flycheck-quicklintjs.el
- flymake-quicklintjs.el
- lsp-quicklintjs.el

A default folder that is looked up by Emacs is
`/usr/local/share/emacs/site-lisp` but requires root privileges, alternatively
you may append a custom folder of your preference containing the plugin
integration files to [Emacs load-path][load-path] variable, for example:

```lisp
(add-to-list 'load-path "~/.emacs/quicklintjs")
```

It's also possible to append `load-path` with Emacs command line argument
`-L folder`

### Eglot

Make sure you have Eglot installed, it's available on [ELPA] or [MELPA]

`M-x package-install RET eglot RET`

Now load `eglot-quicklintjs` and quick-lint-js should be registered as a LSP
server, starting Eglot with `M-x eglot RET` in a js-mode buffer should get you
started.

Usage example in your [Emacs initialization] file.

<details>
<summary>init.el</summary>

```lisp
(require 'eglot-quicklintjs)

(defun my-eglot-quicklintjs-setup ()
  "Configure eglot-quicklintjs for better experience."

  ;; Remove the time to wait after last change before automatically checking
  ;; buffer.  The default is 0.5 (500ms)
  (setq-local eglot-send-changes-idle-time 0))
(add-hook 'js-mode-hook #'my-eglot-quicklintjs-setup)
```

</details>

### Flycheck

First install the Flycheck, it's available on MELPA.

`M-x package-install RET flycheck RET`

Now load flycheck-quicklintjs and quick-lint-js should be registered as a
Flycheck checker, you may inspect your setup with
`M-x flycheck-verify-setup RET`.

Usage example in your [Emacs initialization] file.
<details>
  <summary>init.el</summary>

```lisp
(require 'flycheck-quicklintjs)

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

</details>

### Flymake

Flymake is bultin on Emacs but it's supported only on major version 26 or
higher.

After loading flymake-quicklintjs, you can use `flymake-quicklintjs` function as
a backend for Flymake.

Usage example in your [Emacs initialization] file.
<details>
<summary>init.el</summary>

```lisp
(require 'flymake-quicklintjs)

(defun my-flymake-quicklintjs-setup ()
  "Configure flymake-quicklintjs for better experience."

  ;; Enable Flymake
  (unless (bound-and-true-p flymake-mode)
    (flymake-mode))
  (add-hook 'flymake-diagnostic-functions #'flymake-quicklintjs nil t)

  ;; Remove the time to wait after last change before automatically checking
  ;; buffer.  The default is 0.5 (500ms)
  (setq-local flymake-no-changes-timeout 0))
(add-hook 'js-mode-hook #'my-flymake-quicklintjs-setup)
```

</details>

### LSP Mode

First install the LSP Mode, if you have MELPA configured simply:

`M-x package-install RET lsp-mode RET`

Now require lsp-quicklintjs and quick-lint-js should be registered as a LSP
server, starting LSP Mode with `M-x lsp RET` in a js-mode buffer should get you
started.

Usage example in your [Emacs initialization] file.
<details>
<summary>init.el</summary>

```lisp
(require 'lsp-quicklintjs)

(defun my-lsp-quicklintjs-setup ()
  "Configure lsp-quicklintjs for better experience."
  ;; Remove the time to wait after last change before automatically checking
  ;; buffer.  The default is 0.5 (500ms)
  (setq-local lsp-idle-delay 0))
(add-hook 'js-mode-hook #'my-lsp-quicklintjs-setup)
```

</details>


[Flymake]:
https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html
[Flycheck]: https://www.flycheck.org
[Eglot]: https://github.com/joaotavora/eglot
[LSP Mode]: https://emacs-lsp.github.io/lsp-mode
[Emacs]: https://www.gnu.org/software/emacs
[Emacs initialization]:
https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
[load-path]:
https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
[ELPA]: https://elpa.gnu.org
[MELPA]: https://melpa.org/#/getting-started
