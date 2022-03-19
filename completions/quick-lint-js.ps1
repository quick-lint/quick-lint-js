Register-ArgumentCompleter -CommandName quick-lint-js -Native -ScriptBlock {
    param ($wordToComplete)

    $opts = @('--help',
              '--version',
              '--lsp-server',
              '--config-file=',
              '--stdin',
              '--exit-fail-on=',
              '--output-format=',
              '--diagnostic-hyperlinks=',
              '--vim-file-bufnr='
            )

    $opts.Where({$_ | Select-String $wordToComplete})
}
