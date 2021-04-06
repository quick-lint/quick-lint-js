# Building quick-lint-js npm package

To build this quick-lint-js npm package:

1. Build quick-lint-js for macOS, Linux, and Windows. The build-static.yml GitHub
   Actions workflow does this and uploads the .zip files.
2. Move files where they need to go:
   * `dist/npm/linux/bin/quick-lint-js`: Linux executable
   * `dist/npm/macos/bin/quick-lint-js`: macOS executable
   * `dist/npm/windows/bin/quick-lint-js.exe`: Windows executable
3. Run `npm pack .` to create `quick-lint-js-0.2.0.tgz`.

To install system-wide, run
`npm install --global --unsafe-perm ./quick-lint-js-0.2.0.tgz`.

To install within an existing Node.js project, run
`npm install /path/to/quick-lint-js-0.2.0.tgz`.

To publish to npm's registry, run `npm publish ./quick-lint-js-0.2.0.tgz`.
