# <img src="dist/artwork/dusty-right.svg" alt="" title="Dusty, the quick-lint-js mascot" width="46.4" height="36" align="top"> quick-lint-js

quick-lint-js finds bugs in JavaScript programs.

quick-lint-js finds many bugs, including:

* Using a variable which does not exist
* Assigning to a `const` variable
* Using `await` in a non-`async` function
* Syntax errors

![Demonstration of quick-lint-js in Visual Studio Code](plugin/vscode/demo.webp)

## Installing

There are many ways to install `quick-lint-js`, depending on how you prefer to work! Here are the environments we support:

- CLI (Command-Line Interface)
- LSP (Language Server Protocol)
- Editor Plugins for:
  - Visual Studio Code (VSCode)
  - VSCodium
  - Vim/Neovim
  - Sublime Text
  - Emacs
  - Kate

For detailed installation instructions, visit the [quick-lint-js installation guide](https://quick-lint-js.com/install/).

## Examples
Here are some common bugs that `quick-lint-js` can catch:

### Example 1: Undefined Variable
```js
const occupation = "Engineer";
console.log("Welcome, " + ocupation); // Typo: 'ocupation' should be 'occupation'
```
Output: `use of undeclared variable: ocupation`

### Example 2: Reassigning a const variable
```js
const name = "John";
name = "Doe"; // Error: Cannot reassign a constant
```
Output: `assignment to const variable: name`

### Example 3: Using await in a non-async function
```js
function getData() {
  await fetch("/data");
}
```
Output: `await used in non-async function`

These are just a few examples of how `quick-lint-js` can help identify common issues in your JavaScript code.

## Contributing

We’re always happy to welcome new contributors! If you’d like to contribute to `quick-lint-js`, please refer to the contributing guide on our website for all the details.

Check out the [contributing guide here](https://quick-lint-js.com/contribute/).

## Contact
If you have any questions or feedback, feel free to reach out:

- **Bugs and feature requests**: File an issue on GitHub.
- **IRC**: Ask questions in the quick-lint-js channel on Libera.Chat.
- **Security bug reports** (private disclosure): Email us at strager.nds@gmail.com
