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

    # struct qljs_sublime_text_3_diagnostic {
    #   const char* message;
    #   const char* code;
    #   qljs_severity severity;
    #   int begin_offset;
    #   int end_offset;
    # };
    _fields_ = [
        ("message", ctypes.c_char_p),
        ("code", ctypes.c_char_p),
        ("severity", ctypes.c_int),
        ("begin_offset", ctypes.c_int),
        ("end_offset", ctypes.c_int),
    ]


DiagnosticStructurePointer = ctypes.POINTER(DiagnosticStructure)


class ParserStructure(ctypes.Structure):
    """Parser layer used to communicate with the C++ code."""

    _fields_ = []


ParserStructurePointer = ctypes.POINTER(ParserStructure)


def get_script_directory_path():
    return os.path.dirname(os.path.realpath(__file__))


def load_library():
    script_directory_path = get_script_directory_path()
    lib_path = script_directory_path + "/libquick-lint-js-lib.so"
    return ctypes.CDLL(lib_path)


def create_library():
    lib = load_library()

    lib.qljs_sublime_text_3_create_parser.argtypes = []
    lib.qljs_sublime_text_3_create_parser.restype = ParserStructurePointer

    lib.qljs_sublime_text_3_destroy_parser.argtypes = [ParserStructurePointer]
    lib.qljs_sublime_text_3_destroy_parser.restype = None

    lib.qljs_sublime_text_3_set_text.argtypes = [
        ParserStructurePointer,
        ctypes.c_void_p,
        ctypes.c_size_t,
    ]
    lib.qljs_sublime_text_3_set_text.restype = None

    lib.qljs_sublime_text_3_lint.argtypes = [ParserStructurePointer]
    lib.qljs_sublime_text_3_lint.restype = DiagnosticStructurePointer

    return lib


class Diagnostic:
    """Diagnostic layer used to communicate with the plugin."""

    def __init__(self, ctypes_diagnostic):
        self.message = ctypes_diagnostic.message.decode(encoding="utf-8")
        self.code = ctypes_diagnostic.code.decode(encoding="utf-8")
        self.severity = ctypes_diagnostic.severity
        self.region = sublime.Region(
            ctypes_diagnostic.begin_offset, ctypes_diagnostic.end_offset
        )


class Parser:
    """Parser layer used to communicate with the plugin."""

    try:
        lib = create_library()
    except OSError as error:
        sublime.error_message("quick-lint-js: " + repr(error))
        lib = None
        err = error

    @classmethod
    def is_working(cls):
        """Tests if Parser is working."""
        try:
            cls(None)
        except (OSError, MemoryError):
            return False
        return True

    def __init__(self, view):
        if Parser.lib is None:
            self._ctypes_parser_pointer = None
            raise OSError from Parser.err
        self.view = view
        self.diagnostics = []
        self._ctypes_parser_pointer = Parser.lib.qljs_sublime_text_3_create_parser()
        if self._ctypes_parser_pointer is None:
            raise MemoryError()

    def __del__(self):
        if self._ctypes_parser_pointer is not None:
            Parser.lib.qljs_sublime_text_3_destroy_parser(self._ctypes_parser_pointer)
            self._ctypes_parser_pointer = None

    def set_text(self):
        view_size = self.view.size()
        all_region = sublime.Region(0, view_size)
        all_content = self.view.substr(all_region)
        text_utf8 = all_content.encode(encoding="utf-8")
        text_len_utf8 = len(text_utf8)
        Parser.lib.qljs_sublime_text_3_set_text(
            self._ctypes_parser_pointer,
            text_utf8,
            text_len_utf8,
        )

    def lint(self):
        ctypes_diagnostics_pointer = Parser.lib.qljs_sublime_text_3_lint(
            self._ctypes_parser_pointer
        )
        diagnostics = []
        for ctypes_diagnostic in ctypes_diagnostics_pointer:
            if ctypes_diagnostic.message is None:
                break
            diagnostics.append(Diagnostic(ctypes_diagnostic))
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
