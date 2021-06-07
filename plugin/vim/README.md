# quick-lint-js Vim plugin

This directory contains a plugin for the [Vim text editor][Vim].

This plugin integrates with other Vim plugins to show lint rules when editing
files in Vim.

**Important**: Installing this Vim plugin will not also install quick-lint-js
itself. You must separately install quick-lint-js in order for this plugin to
work.

One of the following Vim plugins must be installed and configured in order for
this plugin to work:

* [ALE - Asynchronous Lint Engine][ALE] v2.1.1 or newer, or version v3.0.0 or
  newer

## Installation

Install this plugin with your favorite Vim packaging system:

* [Vim package](#install-as-a-vim-package) (built into Vim 8.0 and newer)
* [Pathogen](#install-with-pathogen)
* [Vim-Plug](#install-with-vim-plug)
* [Vundle](#install-with-vundle)

### Install as a Vim package

This option requires Vim 8.0 or newer.

First, add `packloadall` to your `vimrc` file if not already present
(`%USERPROFILE%\_vimrc` on Windows; `~/.vimrc` on UNIXy platforms).

**UNIXy platforms**: Create the directory `~/.vim/pack/external/start/`. Copy
the `quick-lint-js.vim` directory into that `start` directory.

**Windows**: Create a directory at
`%USERPROFILE%\vimfiles\pack\external\start\`. Copy the `quick-lint-js.vim`
directory into that `start` directory.

Then, restart Vim.

### Install with Pathogen

This option requires [Pathogen][].

**UNIXy platforms**: Copy the `quick-lint-js.vim` directory into
`~/.vim/bundle/` directory.

Then, restart Vim.

### Install with Vim-Plug

This option requires [Vim-Plug][].

Add the following line to your `vimrc` file between `call plug#begin(...)` and
`call plug#end()`:

    Plug 'quick-lint/quick-lint-js', {'rtp': 'plugin/vim/quick-lint-js.vim'}

Then, restart Vim, then run `:PlugInstall` in Vim.

### Install with Vundle

This option requires [Vundle][].

Add the following line to your `vimrc` file between `call vundle#begin(...)` and
`call vundle#end()`:

    Plugin 'quick-lint/quick-lint-js', {'rtp': 'plugin/vim/quick-lint-js.vim'}

Then, restart Vim, then run `:PluginInstall` in Vim.

[ALE]: https://github.com/dense-analysis/ale
[Pathogen]: https://github.com/tpope/vim-pathogen
[Vim-Plug]: https://github.com/junegunn/vim-plug
[Vim]: https://www.vim.org/
[Vundle]: https://github.com/VundleVim/Vundle.vim
