# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import ctypes
import os
import zipfile


class SeverityEnumeration:
    ERROR = 1
    WARNING = 2


class DiagnosticStructure(ctypes.Structure):
    _fields_ = [
        ("message", ctypes.c_char_p),
        ("code", ctypes.c_char_p),
        ("severity", ctypes.c_int),
        ("begin_offset", ctypes.c_int),
        ("end_offset", ctypes.c_int),
    ]


DiagnosticStructurePointer = ctypes.POINTER(DiagnosticStructure)


class ParserStructure(ctypes.Structure):
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


def set_argtypes_and_restype(func, argtypes=[], restype=None):
    func.argtypes = argtypes
    func.restype = restype


def create_library():
    lib = load_library()
    set_argtypes_and_restype(
        lib.qljs_sublime_text_create_parser, restype=ParserStructurePointer
    )
    set_argtypes_and_restype(
        lib.qljs_sublime_text_destroy_parser, argtypes=[ParserStructurePointer]
    )
    set_argtypes_and_restype(
        lib.qljs_sublime_text_set_text,
        argtypes=[ParserStructurePointer, ctypes.c_void_p, ctypes.c_size_t],
    )
    set_argtypes_and_restype(
        lib.qljs_sublime_text_lint,
        argtypes=[ParserStructurePointer],
        restype=DiagnosticStructurePointer,
    )
    return lib


LIB = create_library()


class Diagnostic:
    def __init__(self, _c_diag):
        self.message = _c_diag.message.decode(encoding="utf-8")
        self.code = _c_diag.code.decode(encoding="utf-8")
        self.severity = _c_diag.severity
        self.begin_offset = _c_diag.begin_offset
        self.end_offset = _c_diag.end_offset


class Parser:
    def __init__(self):
        self._c_parser = LIB.qljs_sublime_text_create_parser()
        if self._c_parser is None:
            raise MemoryError()

    def __del__(self):
        if self._c_parser is not None:
            LIB.qljs_sublime_text_destroy_parser(self._c_parser)

    def set_text(self, text):
        text_utf_8 = text.encode(encoding="utf-8")
        text_utf_8_byte_count = len(text_utf_8)
        LIB.qljs_sublime_text_set_text(
            self._c_parser, text_utf_8, text_utf_8_byte_count
        )

    def lint(self):
        _c_diagnostics = LIB.qljs_sublime_text_lint(self._c_parser)
        diagnostics = []
        for _c_diag in _c_diagnostics:
            if _c_diag.message is None:
                break
            diagnostics.append(Diagnostic(_c_diag))
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
