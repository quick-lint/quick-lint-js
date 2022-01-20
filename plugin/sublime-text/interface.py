# Copyright(C) 2020 Matthew "strager" Glazar
# See end of file for extended copyright information.

from contextlib import contextmanager
from ctypes import CDLL as CLoadLibrary
from ctypes import POINTER as CPointer
from ctypes import Structure as CStructure
from ctypes import c_char_p, c_int, c_size_t, c_uint
from os import chdir, getcwd, path
from platform import system

from sublime import version


def have_incremental_changes():
    major_version = version()[0]
    return int(major_version) > 3


COffset = c_uint


class CText(CStructure):
    _fields_ = [
        ("content", c_char_p),
        ("length", c_size_t),
    ]


class CRegion(CStructure):
    _fields_ = [
        ("start", COffset),
        ("end", COffset),
    ]


if have_incremental_changes():

    class CPosition(CStructure):
        _fields_ = [
            ("line", COffset),
            ("character", COffset),
        ]

else:

    CPosition = COffset


if have_incremental_changes():

    class CRange(CStructure):
        _fields_ = [
            ("start", CPosition),
            ("end", CPosition),
        ]

else:

    CRange = CRegion
    CRangeP = CPointer(CRegion)


class CDiagnostic(CStructure):
    _fields_ = [
        ("range", CRangeP),
        ("severity", c_int),
        ("code", c_char_p),
        ("message", c_char_p),
    ]


CDiagnosticP = CPointer(CDiagnostic)


class CDocument(CStructure):
    _fields_ = []


CDocument = CPointer(CDocument)


def module_path():
    return path.realpath(__file__)


@contextmanager
def changed_directory(path):
    previous_path = getcwd()
    try:
        yield chdir(path)
    finally:
        chdir(previous_path)


def library_pathname():
    return path.dirname(module_path())


def library_filename():
    if system() == "Windows":
        return "quick-lint-js-lib.dll"
    elif system() == "Darwin":
        return "libquick-lint-js-lib.dylib"
    elif system() == "Linux":
        return "libquick-lint-js-lib.so"
    else:
        raise OSError("Operating System not supported")


def library_new():
    pathname = library_pathname()
    filename = library_filename()

    # Multiple .dll files are needed to load the quick-lint-js library
    # on Windows. These .dll files are all in the same folder.
    # For ctypes to find these .dll files, we need to change the current
    # working directory to the folder where these .dll files are.
    with changed_directory(pathname):
        library = CLoadLibrary(filename)

    library.document_new = library.qljs_sublime_text_document_new
    library.document_new.argtypes = []
    library.document_new.restype = CDocumentP
    library.document_delete = library.qljs_sublime_text_document_delete
    library.document_delete.argtypes = [CDocumentP]
    library.document_delete.restype = None
    library.document_set_text = library.qljs_sublime_text_document_set_text
    library.document_set_text.argtypes = [CDocumentP, CText]
    library.document_set_text.restype = None
    library.document_replace_text = library.qljs_sublime_text_document_replace_text
    library.document_replace_text.argtypes = [CDocumentP, CRange, CText]
    library.document_replace_text.restype = None
    library.document_lint = library.qljs_sublime_text_document_lint
    library.document_lint.argtypes = [CDocumentP]
    library.document_lint.restype = CDiagnosticP

    return library


def error_message(message):
    sublime.error_message("quick-lint-js: " + message)


def is_pointer_null(pointer):
    return not bool(ptr)


def view_entire_content(view):
    region = sublime.Region(0, view.size())
    return view.substr(region)


class DocumentError(Exception):
    pass


class Document:
    try:
        library = library_new()
    except OSError as err:
        error_message(str(err))
    finally:
        library = None

    def __init__(self, view):
        self.view = view
        self.diagnostics = []
        try:
            self.c_document_p = library.document_new()
            if is_pointer_null(self.c_document_p):
                raise OSError("Document unavailable")
        except AttributeError:
            raise DocumentError("Library unavailable")
        except OSError:
            raise DocumentError("Internal document unavailable")
        finally:
            self.c_document_p = None

    def delete():
        try:
            parser.Lib.object.destroy_parser(self.Document_p)
        except CException:
            raise ParserError("Cannot delete pointer.")

    def set_text(self):
        content = _view_content(self.view).encode()
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
        self.diagnostics = Diagnostic.from_pointer(Diags_p, self.view)


class Library:
    @staticmethod
    def get_file_extension():
        pass

    def __init__(self):
        directory = os.path.dirname(utils.get_module_path(__name__))
        filename = "quick-lint-js-lib" + self.get_file_extension()
        #    except OSError as err:
        #        raise Exception("") from err  # TODO: add message

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


# class Severity:
#     def __init__(self, value):
#         self.value = value
#
#     def is_error():
#         return self.value == 1
#
#     def is_warning():
#         return self.value == 2


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


# class Severity:
# ERROR = 1
# WARNING = 2
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
import contextlib
import os
import sys

import sublime


def is_pointer_null(ptr):
    return not bool(ptr)


def get_first_character(str):
    return str[0]


def remove_prefix(str, prefix):
    if str.startswith(prefix):
        return str[len(prefix) :]
    return str


def get_module_path(module_name):
    return os.path.realpath(sys.modules[module_name].__file__)


@contextlib.contextmanager
def changed_directory(path):
    previous_path = os.getcwd()
    try:
        yield os.chdir(path)
    finally:
        os.chdir(previous_path)


def sublime_get_major_version():
    return get_first_character(sublime.version())


def sublime_have_incremental_changes():
    return int(sublime_get_major_version()) > 3


def plugin_error_message(message):
    sublime.error_message("quick-lint-js: " + message)


def view_entire_content(view):
    region = sublime.Region(0, view.size())
    return view.substr(region)


# < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < < <


# quick - lint - js finds bugs in JavaScript programs.
# Copyright(C) 2020 Matthew Glazar
#
# This file is part of quick - lint - js.
#
# quick - lint - js is free software : you can redistribute it and / or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# quick - lint - js is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with quick - lint - js.If not, see < https:  // www.gnu.org/licenses/>.
