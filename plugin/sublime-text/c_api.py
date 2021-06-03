# Copyright (C) 2020  Matthew Glazar
# See end of file for extended copyright information.

import os
import zipfile
from ctypes import *


class SeverityEnumeration:
    ERROR = 1
    WARNING = 2


class DiagnosticStructure(Structure):
    _fields_ = [
        ("message", c_char_p),
        ("code", c_char_p),
        ("severity", c_int),
        ("begin_offset", c_int),
        ("end_offset", c_int),
    ]


def iterdiags(diags):
    count = 0
    while diags[count].message is not None:
        yield diags[count]
        count += 1


DiagnosticStructurePointer = POINTER(DiagnosticStructure)


class ParserStructure(Structure):
    _fields_ = []


ParserStructurePointer = POINTER(ParserStructure)


def get_script_path_directory():
    return os.path.dirname(os.path.realpath(__file__))


def string_remove_suffix(string, suffix):
    return string[: -len(suffix)]


def load_library():
    # If the plugin is inside a package you will need to unzip the
    # shared library from inside the package before continuing.
    # From:
    # |-- quick-lint-js-v020-linux_x86_64.sublime-package
    #
    # To:
    # |-- quick-lint-js-v020-linux_x86_64.sublime-package
    # |-- quick-lint-js-v020-linux_x86_64
    #     |-- libquick-lint-js-lib.so
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
    lib = CDLL(lib_path)
    return lib


def set_argtypes_and_restype(func, argtypes=[], restype=None):
    func.argtypes = argtypes
    func.restype = restype


def create_library():
    lib = load_library()
    set_argtypes_and_restype(
        lib.qljs_web_demo_create_parser, restype=ParserStructurePointer
    )
    set_argtypes_and_restype(
        lib.qljs_web_demo_destroy_parser, argtypes=[ParserStructurePointer]
    )
    set_argtypes_and_restype(
        lib.qljs_web_demo_set_text,
        argtypes=[ParserStructurePointer, c_void_p, c_size_t],
    )
    set_argtypes_and_restype(
        lib.qljs_web_demo_lint,
        argtypes=[ParserStructurePointer],
        restype=DiagnosticStructurePointer,
    )
    return lib


lib = create_library()


class Parser:
    def init(self):
        self._c_parser = lib.qljs_web_demo_create_parser()
        if self._c_parser is None:
            raise MemoryError()

    def dealloc(self):
        if self._c_parser is not None:
            lib.qljs_web_demo_destroy_parser(self._c_parser)

    def set_text(self, text, count):
        lib.qljs_web_demo_set_text(self._c_parser, text, count)

    def lint(self):
        return lib.qljs_web_demo_lint(self._c_parser)


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
