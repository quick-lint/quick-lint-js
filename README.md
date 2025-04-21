# <img src="dist/artwork/dusty-right.svg" alt="" title="Dusty, the quick-lint-js mascot" width="46.4" height="36" align="top"> quick-lint-js

quick-lint-js finds bugs in JavaScript programs.

quick-lint-js finds many bugs, including:

* Using a variable which does not exist
* Assigning to a `const` variable
* Using `await` in a non-`async` function
* Syntax errors

![Demonstration of quick-lint-js in Visual Studio Code](plugin/vscode/demo.webp)

## Installing

There are many ways to install quick-lint-js, depending on how you prefer to
work! Here are the environments we support:

- CLI (Command-Line Interface)
- LSP (Language Server Protocol)
- Editor Plugins for:
  - Visual Studio Code (VSCode)
  - VSCodium
  - Vim/Neovim
  - Sublime Text
  - Emacs
  - Kate

For detailed installation instructions, visit the [quick-lint-js installation
guide](https://quick-lint-js.com/install/).

## Examples

Here are some common bugs that quick-lint-js can catch:

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

These are just a few examples of how quick-lint-js can help identify common
issues in your JavaScript code.

## Contributing

We’re always happy to welcome new contributors! If you’d like to contribute to
quick-lint-js, please refer to the contributing guide on our website for all the
details.

Check out the [contributing guide here](https://quick-lint-js.com/contribute/).


   ```markdown
   ## License
   quick-lint-js is licensed under the [MIT License](LICENSE).
   ```

7. **Add Contribution Guidelines**:
   - Provide clear steps on how users can contribute beyond linking to the contributing guide.
   ```markdown
   ## How to Contribute
   1. Fork the repository.
   2. Create a new branch for your changes.
   3. Submit a pull request with a detailed description of your changes.
   ```

8. **Add Links to Related Projects**:
   - If there are other tools or plugins that integrate well with `quick-lint-js`, include them.

### Example Update with Some of These Suggestions:
```markdown
# <img src="dist/artwork/dusty-right.svg" alt="" title="Dusty, the quick-lint-js mascot" width="46.4" height="36" align="top"> quick-lint-js

[![Version](https://img.shields.io/npm/v/quick-lint-js.svg)](https://www.npmjs.com/package/quick-lint-js)
[![Build Status](https://github.com/coder-raju-004/quick-lint-js/actions/workflows/ci.yml/badge.svg)](https://github.com/coder-raju-004/quick-lint-js/actions)
[![License](https://img.shields.io/github/license/coder-raju-004/quick-lint-js)](LICENSE)

quick-lint-js finds bugs in JavaScript programs.

## Table of Contents
- [About](#about)
- [Installing](#installing)
- [Examples](#examples)
- [Contributing](#contributing)
- [Contact](#contact)

## About
quick-lint-js finds many bugs, including:
- Using a variable which does not exist.
- Assigning to a `const` variable.
- Using `await` in a non-`async` function.
- Syntax errors.

![Demonstration of quick-lint-js in Visual Studio Code](plugin/vscode/demo.webp)

## Installing
To install quick-lint-js, follow the [installation guide](https://quick-lint-js.com/install/).

### CLI
```bash
npm install -g quick-lint-js
```

### Visual Studio Code
Download the quick-lint-js extension from the [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=quick-lint-js).

## Examples
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

## Contributing
We welcome contributions! Here's how to get started:
1. Fork the repository.
2. Create a new branch for your changes.
3. Submit a pull request with a detailed description.

Check out the [contributing guide here](https://quick-lint-js.com/contribute/).

## License

quick-lint-js is licensed under the [MIT License](LICENSE).

## Contact

If you have any questions or feedback, feel free to reach out:

- **Bugs and feature requests**: File an issue on GitHub.
- **IRC**: Ask questions in the [#quick-lint-js][quick-lint-js-irc-web] channel
  on Libera.Chat.
- **Security bug reports** (private disclosure): Email us at
  [strager.nds@gmail.com](mailto:strager.nds@gmail.com).

[quick-lint-js-irc-web]: https://kiwiirc.com/nextclient/irc.libera.chat/#quick-lint-js

