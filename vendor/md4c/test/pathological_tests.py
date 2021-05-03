#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import argparse
import sys
import platform
from cmark import CMark
from timeit import default_timer as timer

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Run cmark tests.')
    parser.add_argument('-p', '--program', dest='program', nargs='?', default=None,
            help='program to test')
    parser.add_argument('--library-dir', dest='library_dir', nargs='?',
            default=None, help='directory containing dynamic library')
    args = parser.parse_args(sys.argv[1:])

cmark = CMark(prog=args.program, library_dir=args.library_dir)

# list of pairs consisting of input and a regex that must match the output.
pathological = {
    # note - some pythons have limit of 65535 for {num-matches} in re.
    "U+0000":
                 ("abc\u0000de\u0000",
                  re.compile("abc\ufffd?de\ufffd?")),
    "U+FEFF (Unicode BOM)":
                 ("\ufefffoo",
                  re.compile("<p>foo</p>")),
    "nested strong emph":
                (("*a **a " * 65000) + "b" + (" a** a*" * 65000),
                 re.compile("(<em>a <strong>a ){65000}b( a</strong> a</em>){65000}")),
    "many emph closers with no openers":
                 (("a_ " * 65000),
                  re.compile("(a[_] ){64999}a_")),
    "many emph openers with no closers":
                 (("_a " * 65000),
                  re.compile("(_a ){64999}_a")),
    "many 3-emph openers with no closers":
                 (("a***" * 65000),
                  re.compile("(a<em><strong>a</strong></em>){32500}")),
    "many link closers with no openers":
                 (("a]" * 65000),
                  re.compile("(a\]){65000}")),
    "many link openers with no closers":
                 (("[a" * 65000),
                  re.compile("(\[a){65000}")),
    "mismatched openers and closers":
                 (("*a_ " * 50000),
                  re.compile("([*]a[_] ){49999}[*]a_")),
    "openers and closers multiple of 3":
                 (("a**b" + ("c* " * 50000)),
                  re.compile("a[*][*]b(c[*] ){49999}c[*]")),
    "link openers and emph closers":
                 (("[ a_" * 50000),
                  re.compile("(\[ a_){50000}")),
    "hard link/emph case":
                 ("**x [a*b**c*](d)",
                  re.compile("\\*\\*x <a href=\"d\">a<em>b\\*\\*c</em></a>")),
    "nested brackets":
                 (("[" * 50000) + "a" + ("]" * 50000),
                  re.compile("\[{50000}a\]{50000}")),
    "nested block quotes":
                 ((("> " * 50000) + "a"),
                  re.compile("(<blockquote>\r?\n){50000}")),
    "backticks":
                 ("".join(map(lambda x: ("e" + "`" * x), range(1,1000))),
                  re.compile("^<p>[e`]*</p>\r?\n$")),
    "many links":
                 ("[t](/u) " * 50000,
                  re.compile("(<a href=\"/u\">t</a> ?){50000}")),
    "many references":
                 ("".join(map(lambda x: ("[" + str(x) + "]: u\n"), range(1,20000 * 16))) + "[0] " * 20000,
                  re.compile("(\[0\] ){19999}")),
    "deeply nested lists":
                 ("".join(map(lambda x: ("  " * x + "* a\n"), range(0,1000))),
                  re.compile("<ul>\r?\n(<li>a<ul>\r?\n){999}<li>a</li>\r?\n</ul>\r?\n(</li>\r?\n</ul>\r?\n){999}")),
    "many html openers and closers":
                 (("<>" * 50000),
                  re.compile("(&lt;&gt;){50000}")),
    "many html proc. inst. openers":
                 (("x" + "<?" * 50000),
                  re.compile("x(&lt;\\?){50000}")),
    "many html CDATA openers":
                 (("x" + "<![CDATA[" * 50000),
                  re.compile("x(&lt;!\\[CDATA\\[){50000}")),
    "many backticks and escapes":
                 (("\\``" * 50000),
                  re.compile("(``){50000}")),
    "many broken link titles":
                 (("[ (](" * 50000),
                  re.compile("(\[ \(\]\(){50000}")),
    "broken thematic break":
                 (("* " * 50000 + "a"),
                  re.compile("<ul>\r?\n(<li><ul>\r?\n){49999}<li>a</li>\r?\n</ul>\r?\n(</li>\r?\n</ul>\r?\n){49999}"))
    }

whitespace_re = re.compile('/s+/')
passed = 0
errored = 0
failed = 0

#print("Testing pathological cases:")
for description in pathological:
    (inp, regex) = pathological[description]
    start = timer()
    [rc, actual, err] = cmark.to_html(inp)
    end = timer()
    if rc != 0:
        errored += 1
        print('{:35} [ERRORED (return code %d)]'.format(description, rc))
        print(err)
    elif regex.search(actual):
        print('{:35} [PASSED] {:.3f} secs'.format(description, end-start))
        passed += 1
    else:
        print('{:35} [FAILED]'.format(description))
        print(repr(actual))
        failed += 1

print("%d passed, %d failed, %d errored" % (passed, failed, errored))
if (failed == 0 and errored == 0):
    exit(0)
else:
    exit(1)
