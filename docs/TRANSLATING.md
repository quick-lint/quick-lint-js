# Translating quick-lint-js

## Updating messages.pot after changing quick-lint-js

After editing quick-lint-js' source code, build the `quick-lint-js-i18n` CMake
target. (This is built for you if you build the main `quick-lint-js` executable,
any tests or benchmarks, or any plugin.) This will update po/messages.pot and
also quick-lint-js/i18n/translation-table-generated.cpp,
quick-lint-js/i18n/translation-table-generated.h, and
quick-lint-js/i18n/translation-table-test-generated.h. Be sure to commit changes
to the updated files.

## Updating .po files after changing quick-lint-js or messages.pot

Update the po/\*.po files by running the `tools/update-translator-sources`
script.

You'll need to install the following third-party dependencies:

* [GNU gettext][]

## Updating translation-data.cpp after changing .po files

After editing a translation file (\*.po), update the generated C++ source files
by building the `quick-lint-js-i18n` CMake
target. This will change quick-lint-js/i18n/translation-table-generated.cpp and
quick-lint-js/i18n/translation-table-test-generated.h. Be sure to commit changes
to the updated files.

## Adding a new language 

To add a translation for a new language, run
`tools/create-translation --locale=fr_FR` (changing the locale as appropriate).
This script creates a file in the po directory for your language. See
documentation for [GNU gettext msginit][] for details.

Then, list the .po file in the `QLJS_TRANSLATION_FILES` list in
src/CMakeLists.txt.

[GNU gettext]: https://www.gnu.org/software/gettext/
[GNU gettext msginit]: https://www.gnu.org/software/gettext/manual/html_node/Creating.html#Creating
