### Shell completions

To get automatic completions for quick-lint-js's options you can install the provided shell completions.

#### Zsh

To install the completions for zsh, you can place the `completions/_quick-lint-js` file in any
directory referenced by `$fpath`.

If you do not already have such a directory registered through your `~/.zshrc`, you can add one like this:

```sh
mkdir -p ${ZDOTDIR:-~}/.zsh_functions
echo 'fpath+=${ZDOTDIR:-~}/.zsh_functions' >> ${ZDOTDIR:-~}/.zshrc
```

Then copy the completion file to this directory:

```sh
cp completions/_quick-lint-js ${ZDOTDIR:-~}/.zsh_functions/_quick-lint-js
```

#### Bash

To install the completions for bash, you can `source` the `completions/quick-lint-js.bash` file
in your `~/.bashrc` file.

If you do not plan to delete the source folder of quick-lint-js, you can run

```sh
echo "source $(pwd)/completions/quick-lint-js.bash" >> ~/.bashrc
```

Otherwise you can copy it to the `~/.bash_completion` folder and source it from there:

```sh
mkdir -p ~/.bash_completion
cp completions/quick-lint-js.bash ~/.bash_completion/quick-lint-js
echo "source ~/.bash_completion/quick-lint-js" >> ~/.bashrc
```

#### Fish

To install the completions for fish, run

```sh
mkdir -p $fish_complete_path[1]
cp completions/quick-lint-js.fish $fish_complete_path[1]/quick-lint-js.fish
```
