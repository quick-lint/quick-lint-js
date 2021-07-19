# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

"""Creates communication between plugin and C++ code."""

import ctypes
import os

import sublime


class SeverityEnumeration:
    ERROR = 1
    WARNING = 2


class DiagnosticStructure(ctypes.Structure):
    """Diagnostic layer used to communicate with the C++ code."""

    # struct qljs_sublime_text_4_diagnostic {
    #   const char* message;
    #   const char* code;
    #   qljs_severity severity;
    #   int start_line;
    #   int start_character;
    #   int end_line;
    #   int end_character;
    # };
    _fields_ = [
        ("message", ctypes.c_char_p),
        ("code", ctypes.c_char_p),
        ("severity", ctypes.c_int),
        ("start_line", ctypes.c_int),
        ("start_character", ctypes.c_int),
        ("end_line", ctypes.c_int),
        ("end_character", ctypes.c_int),
    ]


DiagnosticStructurePointer = ctypes.POINTER(DiagnosticStructure)


class ParserStructure(ctypes.Structure):
    """Parser layer used to communicate with the C++ code."""

    _fields_ = []


ParserStructurePointer = ctypes.POINTER(ParserStructure)


def get_script_path_directory():
    return os.path.dirname(os.path.realpath(__file__))


def load_library():
    script_path_directory = get_script_path_directory()
    lib_path = script_path_directory + "/libquick-lint-js-lib.so"
    return ctypes.CDLL(lib_path)


def create_library():
    lib = load_library()

    lib.qljs_sublime_text_4_create_parser.argtypes = []
    lib.qljs_sublime_text_4_create_parser.restype = ParserStructurePointer

    lib.qljs_sublime_text_4_destroy_parser.argtypes = [ParserStructurePointer]
    lib.qljs_sublime_text_4_destroy_parser.restype = None

    lib.qljs_sublime_text_4_replace_text.argtypes = [
        ParserStructurePointer,
        ctypes.c_int,
        ctypes.c_int,
        ctypes.c_int,
        ctypes.c_int,
        ctypes.c_void_p,
        ctypes.c_size_t,
    ]
    lib.qljs_sublime_text_4_replace_text.restype = None

    lib.qljs_sublime_text_4_lint.argtypes = [ParserStructurePointer]
    lib.qljs_sublime_text_4_lint.restype = DiagnosticStructurePointer

    return lib


class Diagnostic:
    """Diagnostic layer used to communicate with the plugin."""

    def __init__(self, ctypes_diagnostic, view):
        self.message = ctypes_diagnostic.message.decode(encoding="utf-8")
        self.code = ctypes_diagnostic.code.decode(encoding="utf-8")
        self.severity = ctypes_diagnostic.severity
        start_point = view.text_point_utf16(
            ctypes_diagnostic.start_line, ctypes_diagnostic.start_character
        )
        end_point = view.text_point_utf16(
            ctypes_diagnostic.end_line, ctypes_diagnostic.end_character
        )
        self.region = sublime.Region(start_point, end_point)


class Parser:
    """Parser layer used to communicate with the plugin."""

    lib = create_library()

    def __init__(self, view):
        self.view = view
        self.diagnostics = []
        self._ctypes_parser = Parser.lib.qljs_sublime_text_4_create_parser()
        if self._ctypes_parser is None:
            raise MemoryError()

    def __del__(self):
        if self._ctypes_parser is not None:
            Parser.lib.qljs_sublime_text_4_destroy_parser(self._ctypes_parser)
            self._ctypes_parser = None

    def set_text(self):
        viewsize = self.view.size()
        allregion = sublime.Region(0, viewsize)
        allcontent = self.view.substr(allregion)
        text_utf8 = allcontent.encode(encoding="utf-8")
        text_len_utf8 = len(text_utf8)
        Parser.lib.qljs_sublime_text_4_replace_text(
            self._ctypes_parser, 0, 0, 0, 0, text_utf8, text_len_utf8
        )

    def replace_text(self, change):
        replacement_text_utf8 = change.str.encode(encoding="utf-8")
        replacement_text_len_utf8 = len(replacement_text_utf8)
        Parser.lib.qljs_sublime_text_4_replace_text(
            self._ctypes_parser,
            change.a.row,
            change.a.col_utf16,
            change.b.row,
            change.b.col_utf16,
            replacement_text_utf8,
            replacement_text_len_utf8,
        )

    def lint(self):
        ctypes_diagnostics = Parser.lib.qljs_sublime_text_4_lint(self._ctypes_parser)
        diagnostics = []
        for ctypes_diagnostic in ctypes_diagnostics:
            if ctypes_diagnostic.message is None:
                break
            diagnostics.append(Diagnostic(ctypes_diagnostic, self.view))
        self.diagnostics = diagnostics


# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew Glazar
#
# This file is part of quick-lint-js.
#
# quick-lint-js is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# quick-lint-js is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
