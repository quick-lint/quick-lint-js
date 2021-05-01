#/usr/bin/env bash

_quick-lint-js ()
{
	local cur prev opts
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"
	opts="-h --help -v --version --lsp --lsp-server --exit-fail-on --output-format --vim-file-bufnr"

	if [[ $cur == -* ]]; then
		COMPREPLY=($(compgen -W '${opts}' -- "$cur"))
		[[ ${COMPREPLY-} == *= ]] && compopt -o nospace
		return 0
	fi
} &&
	complete -F _quick-lint-js quick-lint-js

# vim: ft=bash:
