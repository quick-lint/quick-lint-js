
# MD4C Change Log


## Version 0.4.7

Changes:

 * Add `MD_TABLE_DETAIL` structure into the API. The structure describes column
   count and row count of the table, and pointer to it is passed into the
   application-provided block callback with the `MD_BLOCK_TABLE` block type.

Fixes:

 * [#131](https://github.com/mity/md4c/issues/131):
   Fix handling of a reference image nested in a reference link.

 * [#135](https://github.com/mity/md4c/issues/135):
   Handle unmatched parenthesis pairs inside a permissive URL and WWW auto-links
   in a way more compatible with the GFM.

 * [#138](https://github.com/mity/md4c/issues/138):
   The tag `<tbody></tbody>` is now suppressed whenever the table has zero body
   rows.

 * [#139](https://github.com/mity/md4c/issues/139):
   Recognize a list item mark even when EOF follows it.

 * [#142](https://github.com/mity/md4c/issues/142):
   Fix reference link definition label matching in a case when the label ends
   with a Unicode character with non-trivial case folding mapping.


## Version 0.4.6

Fixes:

 * [#130](https://github.com/mity/md4c/issues/130):
   Fix `ISANYOF` macro, which could provide unexpected results when encountering
   zero byte in the input text; in some cases leading to broken internal state
   of the parser.

   The bug could result in denial of service and possibly also to other security
   implications. Applications are advised to update to 0.4.6.


## Version 0.4.5

Fixes:

 * [#118](https://github.com/mity/md4c/issues/118):
   Fix HTML renderer's `MD_HTML_FLAG_VERBATIM_ENTITIES` flag, exposed in the
   `md2html` utility via `--fverbatim-entities`.

 * [#124](https://github.com/mity/md4c/issues/124):
   Fix handling of indentation of 16 or more spaces in the fenced code blocks.


## Version 0.4.4

Changes:

 * Make Unicode-specific code compliant to Unicode 13.0.

New features:

 * The HTML renderer, developed originally as the heart of the `md2html`
   utility, is now built as a standalone library, in order to simplify its
   reuse in applications.

 * With `MD_HTML_FLAG_SKIP_UTF8_BOM`, the HTML renderer now skips UTF-8 byte
   order mark (BOM) if the input begins with it, before passing to the Markdown
   parser.

   `md2html` utility automatically enables the flag (unless it is custom-built
   with `-DMD4C_USE_ASCII`).

 * With `MD_HTML_FLAG_XHTML`, The HTML renderer generates XHTML instead of
   HTML.

   This effectively means `<br />` instead of `<br>`, `<hr />` instead of
   `<hr>`, and `<img ... />` instead of `<img ...>`.

   `md2html` utility now understands the command line option `-x` or `--xhtml`
   enabling the XHTML mode.

Fixes:

 * [#113](https://github.com/mity/md4c/issues/113):
   Add missing folding info data for the following Unicode characters:
   `U+0184`, `U+018a`, `U+01b2`, `U+01b5`, `U+01f4`, `U+0372`, `U+038f`,
   `U+1c84`, `U+1fb9`, `U+1fbb`, `U+1fd9`, `U+1fdb`, `U+1fe9`, `U+1feb`,
   `U+1ff9`, `U+1ffb`, `U+2c7f`, `U+2ced`, `U+a77b`, `U+a792`, `U+a7c9`.

   Due the bug, the link definition label matching did not work in the case
   insensitive way for these characters.


## Version 0.4.3

New features:

 * With `MD_FLAG_UNDERLINE`, spans enclosed in underscore (`_foo_`) are seen
   as underline (`MD_SPAN_UNDERLINE`) rather than an ordinary emphasis or
   strong emphasis.

Changes:

 * The implementation of wiki-links extension (with `MD_FLAG_WIKILINKS`) has
   been simplified.

    - A noticeable increase of MD4C's memory footprint introduced by the
      extension implementation in 0.4.0 has been removed.
    - The priority handling towards other inline elements have been unified.
      (This affects an obscure case where syntax of an image was in place of
      wiki-link destination made the wiki-link invalid. Now *all* inline spans
      in the wiki-link destination, including the images, is suppressed.)
    - The length limitation of 100 characters now always applies to wiki-link
      destination.

 * Recognition of strike-through spans (with the flag `MD_FLAG_STRIKETHROUGH`)
   has become much stricter and, arguably, reasonable.

    - Only single tildes (`~`) and double tildes (`~~`) are recognized as
      strike-through marks. Longer ones are not anymore.
    - The length of the opener and closer marks have to be the same.
    - The tildes cannot open a strike-through span if a whitespace follows.
    - The tildes cannot close a strike-through span if a whitespace precedes.

   This change follows the changes of behavior in cmark-gfm some time ago, so
   it is also beneficial from compatibility point of view.

 * When building MD4C by hand instead of using its CMake-based build, the UTF-8
   support was by default disabled, unless explicitly asked for by defining
   a preprocessor macro `MD4C_USE_UTF8`.

   This has been changed and the UTF-8 mode now becomes the default, no matter
   how `md4c.c` is compiled. If you need to disable it and use the ASCII-only
   mode, you have explicitly define macro `MD4C_USE_ASCII` when compiling it.

   (The CMake-based build as provided in our repository explicitly asked for
   the UTF-8 support with `-DMD4C_USE_UTF8`. I.e. if you are using MD4C library
   built with our vanilla `CMakeLists.txt` files, this change should not affect
   you.)

Fixes:

 * Fixed some string length handling in the special `MD4C_USE_UTF16` build.

   (This does not affect you unless you are on Windows and explicitly define
   the macro when building MD4C.)

 * [#100](https://github.com/mity/md4c/issues/100):
   Fixed an off-by-one error in the maximal length limit of some segments
   of e-mail addresses used in autolinks.

 * [#107](https://github.com/mity/md4c/issues/107):
   Fix mis-detection of asterisk-encoded emphasis in some corner cases when
   length of the opener and closer differs, as in `***foo *bar baz***`.


## Version 0.4.2

Fixes:

 * [#98](https://github.com/mity/md4c/issues/98):
   Fix mis-detection of asterisk-encoded emphasis in some corner cases when
   length of the opener and closer differs, as in `**a *b c** d*`.


## Version 0.4.1

Unfortunately, 0.4.0 has been released with badly updated ChangeLog. Fixing
this is the only change on 0.4.1.


## Version 0.4.0

New features:

 * With `MD_FLAG_LATEXMATHSPANS`, LaTeX math spans (`$...$`) and LaTeX display
   math spans (`$$...$$`) are now recognized. (Note though that the HTML
   renderer outputs them verbatim in a custom `<x-equation>` tag.)

   Contributed by [Tilman Roeder](https://github.com/dyedgreen).

 * With `MD_FLAG_WIKILINKS`, Wiki-style links (`[[...]]`) are now recognized.
   (Note though that the HTML renderer renders them as a custom `<x-wikilink>`
   tag.)

   Contributed by [Nils Blomqvist](https://github.com/niblo).

Changes:

 * Parsing of tables (with `MD_FLAG_TABLES`) is now closer to the way how
   cmark-gfm parses tables as we do not require every row of the table to
   contain a pipe `|` anymore.

   As a consequence, paragraphs now cannot interrupt tables. A paragraph which
   follows the table has to be delimited with a blank line.

Fixes:

 * [#94](https://github.com/mity/md4c/issues/94):
   `md_build_ref_def_hashtable()`: Do not allocate more memory than strictly
   needed.

 * [#95](https://github.com/mity/md4c/issues/95):
   `md_is_container_mark()`: Ordered list mark requires at least one digit.

 * [#96](https://github.com/mity/md4c/issues/96):
   Some fixes for link label comparison.


## Version 0.3.4

Changes:

 * Make Unicode-specific code compliant to Unicode 12.1.

 * Structure `MD_BLOCK_CODE_DETAIL` got new member `fenced_char`. Application
   can use it to detect character used to form the block fences (`` ` `` or
   `~`). In the case of indented code block, it is set to zero.

Fixes:

 * [#77](https://github.com/mity/md4c/issues/77):
   Fix maximal count of digits for numerical character references, as requested
   by CommonMark specification 0.29.

 * [#78](https://github.com/mity/md4c/issues/78):
   Fix link reference definition label matching for Unicode characters where
   the folding mapping leads to multiple codepoints, as e.g. in `ẞ` -> `SS`.

 * [#83](https://github.com/mity/md4c/issues/83):
   Fix recognition of an empty blockquote which interrupts a paragraph.


## Version 0.3.3

Changes:

 * Make permissive URL autolink and permissive WWW autolink extensions stricter.

   This brings the behavior closer to GFM and mitigates risk of false positives.
   In particular, the domain has to contain at least one dot and parenthesis
   can be part of the link destination only if `(` and `)` are balanced.

Fixes:

 * [#73](https://github.com/mity/md4c/issues/73):
   Some raw HTML inputs could lead to quadratic parsing times.

 * [#74](https://github.com/mity/md4c/issues/74):
   Fix input leading to a crash. Found by fuzzing.

 * [#76](https://github.com/mity/md4c/issues/76):
   Fix handling of parenthesis in some corner cases of permissive URL autolink
   and permissive WWW autolink extensions.


## Version 0.3.2

Changes:

 * Changes mandated by CommonMark specification 0.29.

   Most importantly, the white-space trimming rules for code spans have changed.
   At most one space/newline is trimmed from beginning/end of the code span
   (if the code span contains some non-space contents, and if it begins and
   ends with space at the same time). In all other cases the spaces in the code
   span are now left intact.

   Other changes in behavior are in corner cases only. Refer to [CommonMark
   0.29 notes](https://github.com/commonmark/commonmark-spec/releases/tag/0.29)
   for more info.

Fixes:

 * [#68](https://github.com/mity/md4c/issues/68):
   Some specific HTML blocks were not recognized when EOF follows without any
   end-of-line character.

 * [#69](https://github.com/mity/md4c/issues/69):
   Strike-through span not working correctly when its opener mark is directly
   followed by other opener mark; or when other closer mark directly precedes
   its closer mark.


## Version 0.3.1

Fixes:

 * [#58](https://github.com/mity/md4c/issues/58),
   [#59](https://github.com/mity/md4c/issues/59),
   [#60](https://github.com/mity/md4c/issues/60),
   [#63](https://github.com/mity/md4c/issues/63),
   [#66](https://github.com/mity/md4c/issues/66):
   Some inputs could lead to quadratic parsing times. Thanks to Anders Kaseorg
   for finding all those issues.

 * [#61](https://github.com/mity/md4c/issues/59):
   Flag `MD_FLAG_NOHTMLSPANS` erroneously affected also recognition of
   CommonMark autolinks.


## Version 0.3.0

New features:

 * Add extension for GitHub-style task lists:

   ```
    * [x] foo
    * [x] bar
    * [ ] baz
   ```

   (It has to be explicitly enabled with `MD_FLAG_TASKLISTS`.)

 * Added support for building as a shared library. On non-Windows platforms,
   this is now default behavior; on Windows static library is still the default.
   The CMake option `BUILD_SHARED_LIBS` can be used to request one or the other
   explicitly.

   Contributed by Lisandro Damián Nicanor Pérez Meyer.

 * Renamed structure `MD_RENDERER` to `MD_PARSER` and refactorize its contents
   a little bit. Note this is source-level incompatible and initialization code
   in apps may need to be updated.

   The aim of the change is to be more friendly for long-term ABI compatibility
   we shall maintain, starting with this release.

 * Added `CHANGELOG.md` (this file).

 * Make sure `md_process_table_row()` reports the same count of table cells for
   all table rows, no matter how broken the input is. The cell count is derived
   from table underline line. Bogus cells in other rows are silently ignored.
   Missing cells in other rows are reported as empty ones.

Fixes:

 * CID 1475544:
   Calling `md_free_attribute()` on uninitialized data.

 * [#47](https://github.com/mity/md4c/issues/47):
   Using bad offsets in `md_is_entity_str()`, in some cases leading to buffer
   overflow.

 * [#51](https://github.com/mity/md4c/issues/51):
   Segfault in `md_process_table_cell()`.

 * [#53](https://github.com/mity/md4c/issues/53):
   With `MD_FLAG_PERMISSIVEURLAUTOLINKS` or `MD_FLAG_PERMISSIVEWWWAUTOLINKS`
   we could generate bad output for ordinary Markdown links, if a non-space
   character immediately follows like e.g. in `[link](http://github.com)X`.


## Version 0.2.7

This was the last version before the changelog has been added.
