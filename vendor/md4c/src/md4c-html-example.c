/*
 * MD4C: Markdown parser for C
 * (http://github.com/mity/md4c)
 *
 * Copyright (c) 2016-2021 Martin Mitas
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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "md4c-html.h"

static const char example_markdown[] =
    "# HTML example\n"
    "\n"
    "This example program shows how to use the `md_html_create` API.\n"
    "\n"
    "This program uses `<tt>` instead of `<code>` for inline code blocks.\n";

typedef struct {
    MD_PARSER parser;
    MD_PARSER* html_renderer;
    MD_SIZE html_renderer_size;
    FILE* output_file;
} EXAMPLE_PARSER;

static int example_enter_block_callback(MD_BLOCKTYPE type, void* detail, void* userdata)
{
    EXAMPLE_PARSER* p = (EXAMPLE_PARSER*) userdata;
    p->html_renderer->enter_block(type, detail, p->html_renderer);
    return 0;
}

static int example_leave_block_callback(MD_BLOCKTYPE type, void* detail, void* userdata)
{
    EXAMPLE_PARSER* p = (EXAMPLE_PARSER*) userdata;
    p->html_renderer->leave_block(type, detail, p->html_renderer);
    return 0;
}

static int example_enter_span_callback(MD_SPANTYPE type, void* detail, void* userdata)
{
    EXAMPLE_PARSER* p = (EXAMPLE_PARSER*) userdata;
    if (type == MD_SPAN_CODE) {
        fprintf(p->output_file, "<tt>");
    } else {
        p->html_renderer->enter_span(type, detail, p->html_renderer);
    }
    return 0;
}

static int example_leave_span_callback(MD_SPANTYPE type, void* detail, void* userdata)
{
    EXAMPLE_PARSER* p = (EXAMPLE_PARSER*) userdata;
    if (type == MD_SPAN_CODE) {
        fprintf(p->output_file, "</tt>");
    } else {
        p->html_renderer->leave_span(type, detail, p->html_renderer);
    }
    return 0;
}

static int example_text_callback(MD_TEXTTYPE type, const MD_CHAR* text, MD_SIZE size, void* userdata)
{
    EXAMPLE_PARSER* p = (EXAMPLE_PARSER*) userdata;
    p->html_renderer->text(type, text, size, p->html_renderer);
    return 0;
}

static void example_process_output(const MD_CHAR* buffer, MD_SIZE buffer_size, void* userdata)
{
    EXAMPLE_PARSER* p = (EXAMPLE_PARSER*) userdata;
    fwrite(buffer, sizeof(MD_CHAR), buffer_size, p->output_file);
}

int main()
{
    int rc;

    unsigned parser_flags = 0;
    unsigned renderer_flags = 0;
    EXAMPLE_PARSER p = {
        {
            0,
            parser_flags,
            example_enter_block_callback,
            example_leave_block_callback,
            example_enter_span_callback,
            example_leave_span_callback,
            example_text_callback,
            NULL,
            NULL,
        },
        NULL,
        0,
        stdout,
    };

    md_html_create(NULL, &p.html_renderer_size,
                   example_process_output, &p,
                   parser_flags, renderer_flags);
    p.html_renderer = malloc(p.html_renderer_size);
    rc = md_html_create(p.html_renderer, &p.html_renderer_size,
                        example_process_output, &p,
                        parser_flags, renderer_flags);
    if (rc != 0) {
        fprintf(stderr, "error: failed to create HTML renderer\n");
        return 1;
    }

    rc = md_parse(example_markdown, strlen(example_markdown), &p.parser, &p);
    if (rc != 0) {
        fprintf(stderr, "error: failed to parse Markdown\n");
        return 1;
    }

    rc = md_html_destroy(p.html_renderer, p.html_renderer_size);
    if (rc != 0) {
        fprintf(stderr, "error: failed to destroy HTML renderer\n");
        return 1;
    }
    free(p.html_renderer);

    return 0;
}
