# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import os
import platform
from contextlib import contextmanager
from ctypes import CDLL, POINTER, Structure, c_char_p, c_int, c_size_t, c_uint

from sublime import version

from . import utils


def _have_incremental_changes():
    major_version = version()[0]
    return int(major_version) > 3


_Offset = c_uint
_OffsetPointer = POINTER(_Offset)


class _Text(Structure):
    _fields_ = [
        ("content", c_char_p),
        ("length", c_size_t),
    ]


_TextPointer = POINTER(_Text)


class _Region(Structure):
    _fields_ = [
        ("start", _Offset),
        ("end", _Offset),
    ]


_RegionPointer = POINTER(_Region)


if _have_incremental_changes():

    class _Position(Structure):
        _fields_ = [
            ("line", _Offset),
            ("character", _Offset),
        ]

    _PositionPointer = POINTER(_Position)

else:

    _Position = _Offset
    _PositionPointer = _OffsetPointer


if _have_incremental_changes():

    class _Range(Structure):
        _fields_ = [
            ("start", _Position),
            ("end", _Position),
        ]

    _RangePointer = POINTER(_Range)

else:

    _Range = _Region
    _RangePointer = _RegionPointer


class _Diagnostic(Structure):
    _fields_ = [
        ("range", _RangePointer),
        ("severity", c_int),
        ("code", c_char_p),
        ("message", c_char_p),
    ]


class _Document:
    _fields_ = []


class Exception(Exception):
    pass


def _get_module_path():
    return os.path.realpath(__file__)


@contextmanager
def changed_directory(path):
    previous_path = os.getcwd()
    try:
        yield os.chdir(path)
    finally:
        os.chdir(previous_path)


def _create_library():
    directory = os.path.dirname(_get_module_path())
    if platform.system() == "Windows":
        filename = "quick-lint-js-lib.dll"
    elif platform.system() == "Darwin":
        filename = "libquick-lint-js-lib.dylib"
    else platform.system() == "Linux":
        filename = "libquick-lint-js-lib.so"
    # It's need multiple DLLs for load the library object on Windows,
    # these DLLs are all in the same folder, for find these DLLs
    # we need to change the current working directory to that folder.
    with _changed_directory(directory):
        library = CDLL(filename)

    def libget(name):
        return getattr(library, "qljs_sublime_text" + name)

    def libset(name, argtypes, restype):
        libget(name).argtypes = argtypes
        libget(name).restype = restype

    def getargtypes(structure):
        return structure._fields_

    libset('text_new', )


class Library:
    @staticmethod
    def get_file_extension():
    def __init__(self):
        directory = os.path.dirname(utils.get_module_path(__name__))
        filename = "quick-lint-js-lib" + self.get_file_extension()
            except OSError as err:
                raise Exception("") from err  # TODO: add message

        version = utils.sublime.major_version()
        self.Document = cdll.qljs_sublime_text_document_new
        self.Document.argtypes = []
        self.Document.restype = Document_p
        self.Document = cdll.qljs_sublime_text_document_delete
        self.Document.argtypes = [Document_p]
        self.Document.restype = None
        self.Set_text = cdll.qljs_sublime_text_set_text
        self.Set_text.argtypes = [Document_p, Text_p]
        self.Set_text.restype = Error_p
        self.Replace_text = cdll.qljs_sublime_text_replace_text
        self.Replace_text.argtypes = [Document_p, Range_p, Text_p]
        self.Replace_text.restype = Error_p
        self.Lint = cdll.qljs_sublime_text_lint
        self.Lint.argtypes = [Document_p]
        self.Lint.restype = DiagnostiP

    def create_parser(self):
        Document_p = self.Create_parser()
        if utils.is_pointer_null(Document_p):
            raise Exception("Parser unavailable.")
        return Document_p

    def destroy_parser(self, Document_p):
        if utils.is_pointer_null(Document_p):
            raise Exception("Cannot free nonexistent pointer.")
        self.Destroy_parser(Document_p)

    def set_text(self, Document_p, Text_p):
        return self.Set_text(Document_p, Text_p)

    if _have_incremental_changes():

        def replace_text(self, Document_p, Range_p, Text_p):
            return self.Replace_text(Document_p, Range_p, Text_p)

    def lint(self, Document_p):
        return self.Lint(Document_p)


################################################################################
## python interface
################################################################################


class Severity:
    def __init__(self, value):
        self.value = value

    def is_error():
        return self.value == 1

    def is_warning():
        return self.value == 2


class Diagnostic:
    @classmethod
    def from_pointer(cls, Diags_p):
        diags = []
        for Diag in Diags_p:
            if utils.is_pointer_null(Diag.message):
                break
            diags.append(Diagnostic(Diag))
        return diags

    def __init__(self, Diag, view):
        self.message = Diag.message.decode()
        self.code = Diag.code.decode()
        self.severity = Severity(Diag.severity)
        if utils.sublime.is_version_three():
            start = Diag.region.start
            end = Diag.region.end
        elif utils.sublime.is_version_four():
            start = view.text_point_utf16(Diag.start_line, Diag.start_character)
            end = view.text_point_utf16(Diag.end_line, Diag.end_character)
        self.region = sublime.Region(start, end)


class Parser:
    try:
        Lib = CLibrary()
    except CException as ex:
        sublime.error_message("quick-lint-js: " + str(ex))
    finally:
        Lib = None

    def __init__(self, view):
        self.view = view
        self.diags = []
        try:
            self.Document_p = Parser.Lib.object.create_parser()
        except AttributeError:
            raise ParserError("Library unavailable.")
        except CException:
            raise ParserError("Internal parser unavailable.")
        finally:
            self.Document_p = None

    def delete():
        try:
            Parser.Lib.object.destroy_parser(self.Document_p)
        except CException:
            raise ParserError("Cannot delete pointer.")

    def set_text(self):
        content = utils.sublime.view_content(self.view).encode()
        Text_p = CText(content, len(content)).lightweight_pointer()
        Parser.Lib.object.set_text(self.Document_p, Text)

    def replace_text(self, change):
        Start = CPosition(change.a.row, change.a.col_utf16)
        End = CPosition(change.b.row, change.b.col_utf16)
        Range_p = CRange(Start, End).lightweight_pointer()
        content = change.str.encode()
        Text_p = CText(content, len(content)).lightweight_pointer()
        Parser.Lib.object.replace_text(self.Document_p, Range_p, Text_p)

    def lint(self):
        Diags_p = Parser.Lib.lint(self.Document_p)
        self.diags = Diagnostic.from_pointer(Diags_p, self.view)


# class Severity:
#     ERROR = 1
#     WARNING = 2


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
