# quick-lint-js

**Warning**: This is pre-release software. quick-lint-js is under active
development and is unstable. Use at your own risk.

quick-lint-js finds bugs in your JavaScript code.

quick-lint-js finds many bugs, including:

* Using a variable which does not exist
* Assigning to a `const` variable
* Using `await` in a non-`async` function
* Syntax errors

## Installing

### Install within project

Install quick-lint-js so anyone can use it when hacking on your project:

    # Linux and macOS with npm:
    $ npm install quick-lint-js --save-dev --save-exact
    $ node_modules/.bin/quick-lint-js --version

    # Linux and macOS with yarn:
    $ yarn add quick-lint-js --dev --exact
    $ node_modules/.bin/quick-lint-js --version

    # Windows with npm:
    $ npm install quick-lint-js --save-dev --save-exact
    $ node_modules\.bin\quick-lint-js.cmd --version

    # Windows with yarn:
    $ yarn add quick-lint-js --dev --exact
    $ node_modules\.bin\quick-lint-js.cmd --version

### Install globally

Install quick-lint-js on your machine for use anywhere:

    # Linux and macOS:
    $ sudo npm install --global --unsafe-perm quick-lint-js
    $ quick-lint-js --version

    # Windows:
    $ npm install --global quick-lint-js
    $ quick-lint-js.cmd --version
