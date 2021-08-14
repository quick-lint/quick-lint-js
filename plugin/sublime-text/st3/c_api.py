# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

"""Creates communication between plugin and C++ code."""

import ctypes
import os
import platform

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


class ErrorStructure(ctypes.Structure):
    """Error layer used to communicate with the C++ code."""

    # struct qljs_sublime_text_3_error {
    #   const char* message;
    # };
    _fields_ = [
        ("message", ctypes.c_char_p),
    ]


class ResultStructure(ctypes.Structure):
    """Result layer used to communicate with the C++ code."""

    # struct qljs_sublime_text_3_result {
    #   union {
    #     qljs_sublime_text_4_diagnostic* diagnostics;
    #     qljs_sublime_text_4_error error;
    #   } value;
    #   bool is_diagnostics;
    # };
    class ValueUnion(ctypes.Union):
        _fields_ = [
            ("diagnostics", DiagnosticStructurePointer),
            ("error", ErrorStructure),
        ]

    _fields_ = [
        ("value", ValueUnion),
        ("is_diagnostics", ctypes.c_bool),
    ]


ResultStructurePointer = ctypes.POINTER(ResultStructure)


class ParserStructure(ctypes.Structure):
    """Parser layer used to communicate with the C++ code."""

    _fields_ = []


ParserStructurePointer = ctypes.POINTER(ParserStructure)


def get_script_directory_path():
    return os.path.dirname(os.path.realpath(__file__))


class set_directory:
    """Sets the cwd (current working directory) within the context."""

    def __init__(self, path):
        self.path = path
        self.origin = os.getcwd()

    def __enter__(self):
        os.chdir(self.path)

    def __exit__(self, exception_type, exception_value, traceback):
        os.chdir(self.origin)
        return False


def load_library():
    if platform.system() == "Windows":
        lib_path_file = "quick-lint-js-lib.dll"
        directory_separator = "\\"
    elif platform.system() == "Linux":
        lib_path_file = "libquick-lint-js-lib.so"
        directory_separator = "/"

    lib_directory_path = get_script_directory_path()
    lib_path = lib_directory_path + directory_separator + lib_path_file
    # For the Python loading the library (through ctypes) on Windows,
    # it needs to load the DLLs (other libraries) that this library
    # depends on, to Python do it, we need to change the current
    # working directory (temporarily) to the directory containing
    # these dependencies. This directory, in this case, is the same
    # as the library we are trying to load.
    with set_directory(lib_directory_path):
        lib = ctypes.CDLL(lib_path)
    return lib


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
    lib.qljs_sublime_text_3_set_text.restype = ErrorStructure

    lib.qljs_sublime_text_3_lint.argtypes = [ParserStructurePointer]
    lib.qljs_sublime_text_3_lint.restype = ResultStructurePointer

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


def display_error_message(message):
    sublime.error_message("error: quick-lint-js:\n" + message)


class Error(Exception):
    """Error layer used to communicate with the plugin."""

    def __init__(self, ctypes_error):
        self.message = ctypes_error.message.decode(encoding="utf-8")
        super().__init__(self.message)

    def has_message(self):
        return bool(self.message)

    def display_message(self):
        display_error_message(self.message)


class Parser:
    """Parser layer used to communicate with the plugin."""

    try:
        lib = create_library()
    except OSError as error:
        display_error_message(repr(error))
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
        ctypes_error = Parser.lib.qljs_sublime_text_3_set_text(
            self._ctypes_parser_pointer,
            text_utf8,
            text_len_utf8,
        )
        if ctypes_error.message is not None:
            raise Error(ctypes_error)

    def lint(self):
        ctypes_result_pointer = Parser.lib.qljs_sublime_text_3_lint(
            self._ctypes_parser_pointer
        )
        ctypes_result = ctypes_result_pointer.contents
        if ctypes_result.is_diagnostics:
            ctypes_diagnostics_pointer = ctypes_result.value.diagnostics
            diagnostics = []
            for ctypes_diagnostic in ctypes_diagnostics_pointer:
                if ctypes_diagnostic.message is None:
                    break
                diagnostics.append(Diagnostic(ctypes_diagnostic))
            self.diagnostics = diagnostics
        else:
            self.diagnostics = []
            raise Error(ctypes_result.value.error)


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
