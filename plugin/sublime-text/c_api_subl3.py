# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

"""Creates communication between plugin and C++ code."""

import ctypes
import os
import zipfile


class SeverityEnumeration:
    ERROR = 1
    WARNING = 2


class DiagnosticStructure(ctypes.Structure):
    """Diagnostic layer used to communicate with the C++ code."""

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


def get_script_path_directory():
    return os.path.dirname(os.path.realpath(__file__))


def string_remove_suffix(string, suffix):
    return string[: -len(suffix)]


def load_library():
    # If the plugin is inside a package you will need to unzip the
    # shared library from inside the package before continuing.
    #
    # From:
    # |-- quick-lint-js-v020-linux_x86_64.sublime-package
    #
    # To:
    # |-- quick-lint-js-v020-linux_x86_64.sublime-package
    # |-- quick-lint-js-v020-linux_x86_64
    #     |-- libquick-lint-js-lib.so
    #
    script_path_directory = get_script_path_directory()
    if script_path_directory.endswith(".sublime-package"):
        with zipfile.ZipFile(script_path_directory, mode="r") as package_file:
            lib_path_directory = string_remove_suffix(
                script_path_directory, ".sublime-package"
            )
            package_file.extract(
                "libquick-lint-js-lib.so",
                path=lib_path_directory,
            )
    else:
        lib_path_directory = script_path_directory

    # Load the shared library.
    lib_path = lib_path_directory + "/libquick-lint-js-lib.so"
    lib = ctypes.CDLL(lib_path)
    return lib


def set_argtypes_and_restype(func, argtypes, restype):
    func.argtypes = argtypes
    func.restype = restype


def create_library():
    lib = load_library()
    set_argtypes_and_restype(
        lib.qljs_sublime_text_3_create_parser,
        argtypes=[],
        restype=ParserStructurePointer,
    )
    set_argtypes_and_restype(
        lib.qljs_sublime_text_3_destroy_parser,
        argtypes=[ParserStructurePointer],
        restype=None,
    )
    set_argtypes_and_restype(
        lib.qljs_sublime_text_3_set_text,
        argtypes=[ParserStructurePointer, ctypes.c_void_p, ctypes.c_size_t],
        restype=None,
    )
    set_argtypes_and_restype(
        lib.qljs_sublime_text_3_lint,
        argtypes=[ParserStructurePointer],
        restype=DiagnosticStructurePointer,
    )
    return lib


LIB = create_library()


class Diagnostic:
    """Diagnostic layer used to communicate with the plugin."""

    def __init__(self, ctypes_diagnostic):
        self.message = ctypes_diagnostic.message.decode(encoding="utf-8")
        self.code = ctypes_diagnostic.code.decode(encoding="utf-8")
        self.severity = ctypes_diagnostic.severity
        self.begin_offset = ctypes_diagnostic.begin_offset
        self.end_offset = ctypes_diagnostic.end_offset


class Parser:
    """Parser layer used to communicate with the plugin."""

    def __init__(self):
        self._ctypes_parser = LIB.qljs_sublime_text_3_create_parser()
        if self._ctypes_parser is None:
            raise MemoryError()

    def __del__(self):
        if self._ctypes_parser is not None:
            LIB.qljs_sublime_text_3_destroy_parser(self._ctypes_parser)
            self._ctypes_parser = None

    def set_text(self, text):
        text_utf8 = text.encode(encoding="utf-8")
        text_len_utf8 = len(text_utf8)
        LIB.qljs_sublime_text_3_set_text(
            self._ctypes_parser,
            text_utf8,
            text_len_utf8,
        )

    def lint(self):
        ctypes_diagnostics = LIB.qljs_sublime_text_3_lint(self._ctypes_parser)
        diagnostics = []
        for ctypes_diagnostic in ctypes_diagnostics:
            if ctypes_diagnostic.message is None:
                break
            diagnostics.append(Diagnostic(ctypes_diagnostic))
        return diagnostics


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
