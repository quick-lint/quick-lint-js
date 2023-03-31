# Translating quick-lint-js

## Updating .po files after changing quick-lint-js

After editing quick-lint-js' source code, update the po/messages.pot, po/\*.po,
and po/compiled/\*.mo files by running the `tools/update-translator-sources`
script. Be sure to commit changes to the updated \*.pot, \*.po, and \*.mo files.

You'll need to install the following third-party dependencies:

* [Go][] version 1.16 or newer
* [GNU gettext][]

## Updating translation-data.cpp after changing .po files

After editing a translation file (\*.po), update the compiled .mo files
(embedded within src/quick-lint-js/translation-data-generated.cpp) by running the
`tools/compile-translations.go` script. Be sure to commit changes to the updated
translation-data-generated.cpp file

## Adding a new language (.po file)

To add a translation for a new language, run
`tools/create-translation --locale=fr_FR` (changing the locale as appropriate).
This script creates a file in the po directory for your language. See
documentation for [GNU gettext msginit][] for details.

[Go]: https://go.dev/
[GNU gettext]: https://www.gnu.org/software/gettext/
[GNU gettext msginit]: https://www.gnu.org/software/gettext/manual/html_node/Creating.html#Creating
