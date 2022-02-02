This extension finds bugs in your JavaScript code.

quick-lint-js detects [over 200 different JavaScript mistakes][error-docs],
including:

* Using a variable which does not exist
* `await` in a non-`async` function
* `=` instead of `===` in an `if` statement
* Syntax errors

quick-lint-js is [over 110× faster than ESLint][benchmarks] and works out of the
box with no configuration.

![Demonstration of quick-lint-js in Visual Studio Code](demo.webp)

This plugin bundles quick-lint-js. You do not need to install the quick-lint-js
program separately.

---

quick-lint-js lints your code as you type. It continuously finds JavaScript
mistakes before you run your code. Improve the quality of your JavaScript code
and reduce debugging time with this easy-to-use Visual Studio Code extension.

Supported JavaScript language variants:

* Node.js (`require`)
* browser JavaScript (`window`)
* JavaScript modules (`import`) and JavaScript scripts
* ES3, ES5, ES2015 (ES6) through ES2020
* React (JSX)

[benchmarks]: https://quick-lint-js.com/benchmarks/
[error-docs]: https://quick-lint-js.com/errors/
