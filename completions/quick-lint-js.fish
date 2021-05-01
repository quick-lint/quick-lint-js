complete -c quick-lint-js -s h -l help -d 'Print help message' -f
complete -c quick-lint-js -s v -l version -d 'Print version information' -f
complete -c quick-lint-js -l lsp -l lsp-server -d 'Run quick-lint-js in LSP server mode' -r
complete -c quick-lint-js -l exit-fail-on -d 'Fail with a non-zero exit code if any of these errors are found (default: "all")' -r
complete -c quick-lint-js -l output-format -d 'Format to print feedback where FORMAT is one of: gnu-like (default if omitted), vim-qflist-json' -r
complete -c quick-lint-js -l vim-file-bufnr -d 'Select a vim buffer for outputting feedback' -r

# vim: ft=sh:
