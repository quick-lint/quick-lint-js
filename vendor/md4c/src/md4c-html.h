/*
 * MD4C: Markdown parser for C
 * (http://github.com/mity/md4c)
 *
 * Copyright (c) 2016-2017 Martin Mitas
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifndef MD4C_HTML_H
#define MD4C_HTML_H

#include "md4c.h"

#ifdef __cplusplus
    extern "C" {
#endif


/* If set, debug output from md_parse() is sent to stderr. */
#define MD_HTML_FLAG_DEBUG                  0x0001
#define MD_HTML_FLAG_VERBATIM_ENTITIES      0x0002
#define MD_HTML_FLAG_SKIP_UTF8_BOM          0x0004
#define MD_HTML_FLAG_XHTML                  0x0008


/* Render Markdown into HTML.
 *
 * Note only contents of <body> tag is generated. Caller must generate
 * HTML header/footer manually before/after calling md_html().
 *
 * Params input and input_size specify the Markdown input.
 * Callback process_output() gets called with chunks of HTML output.
 * (Typical implementation may just output the bytes to a file or append to
 * some buffer).
 * Param userdata is just propgated back to process_output() callback.
 * Param parser_flags are flags from md4c.h propagated to md_parse().
 * Param render_flags is bitmask of MD_HTML_FLAG_xxxx.
 *
 * Returns -1 on error (if md_parse() fails.)
 * Returns 0 on success.
 */
int md_html(const MD_CHAR* input, MD_SIZE input_size,
            void (*process_output)(const MD_CHAR*, MD_SIZE, void*),
            void* userdata, unsigned parser_flags, unsigned renderer_flags);

/* Create an MD_PARSER which renders Markdown into HTML.
 *
 * The caller is responsible for allocating enough memory to store the
 * returned MD_PARSER. This size might be greater than sizeof(md_parser), so
 * callers must ensure enough memory is allocated. To determine the size,
 * first call md_html_create with *out_renderer_size equal to 0.
 *
 * Before deallcoating memory used for the returned MD_PARSER, call
 * md_html_destroy.
 *
 * See md4c-html-example.c for example usage.
 *
 * Note only contents of <body> tag is generated. Caller must generate
 * HTML header/footer manually before/after using the parser
 * returned by md_html_create.
 *
 * Param out_renderer is initialized if *out_renderer_size is large enough.
 * Param out_renderer_size points to the size of the allocation pointed to
 * by out_renderer (in chars). If *out_renderer_size is too small,
 * md_html_create modifies *out_renderer_size with the expected size then
 * returns -1.
 * Callback process_output() gets called with chunks of HTML output.
 * (Typical implementation may just output the bytes to a file or append to
 * some buffer) as parsing occurs.
 * Param userdata is just propgated back to process_output() callback.
 * Param parser_flags are flags from md4c.h propagated to md_parse().
 * Param render_flags is bitmask of MD_HTML_FLAG_xxxx.
 *
 * Do not specify MD_HTML_FLAG_SKIP_UTF8_BOM in render_flag. Currently,
 * md_html_create ignores this flag, but this behavior might change in the
 * future.
 *
 * Returns -1 and modifies *out_renderer_size if *out_renderer_size is too
 * small.
 * Returns -1 and does not modify *out_renderer_size if another error
 * occurs.
 * Returns 0 and modifies *out_renderer_size on success.
 */
int md_html_create(MD_PARSER* out_renderer, MD_SIZE* out_renderer_size,
                   void (*process_output)(const MD_CHAR*, MD_SIZE, void*),
                   void* userdata,
                   unsigned parser_flags, unsigned renderer_flags);

/* Clean up resources allocated by md_html_create.
 *
 * Returns -1 on error.
 * Returns 0 on success.
 */
int md_html_destroy(MD_PARSER* renderer, MD_SIZE renderer_size);


#ifdef __cplusplus
    }  /* extern "C" { */
#endif

#endif  /* MD4C_HTML_H */
