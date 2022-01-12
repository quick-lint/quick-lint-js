# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import ctypes
import os
import platform

from . import utils


c_offset = ctypes.c_uint


class CStruct(ctypes.Structure):
    def __init_subclass__(cls, /, **kwargs):
        try:
            cls.CPointer = ctypes.POINTER(cls)
            cls._fields_ = list(cls.c_fields.items())
        except AttributeError:
            pass
        super().__init_subclass__(**kwargs)

    def lightweight_pointer(self):
        return ctypes.byref(self)

    def pointer(self):
        return ctypes.pointer(self)


class CText(CStruct):
    c_fields = {
        "content": ctypes.c_char_p,
        "length": ctypes.c_size_t,
    }


class CRegion(CStruct):
    c_fields = {
        "start": ctypes.c_uint,
        "end": ctypes.c_uint,
    }


class CPosition(CStruct):
    c_fields = {
        "line": ctypes.c_uint,
        "character": ctypes.c_uint,
    }


class CRange(CStruct):
    c_fields = {
        "start": CPosition,
        "end": CPosition,
    }


class CDiagnostic(CStruct):
    c_fields = {
        "severity": ctypes.c_int,
        "code": ctypes.c_char_p,
        "message": ctypes.c_char_p,
    }
    if utils.sublime_is_version_three():
        c_fields["region"] = CRegion.CPointer
    elif utils.sublime_is_version_four():
        c_fields["range"] = CRange.CPointer


class CDocument(Cstruct):
    c_fields = {}


class CException(Exception):
    pass


class CLibrary:
    @staticmethod
    def get_file_extension():
        if platform.system() == "Windows":
            return ".dll"
        elif platform.system() == "Darwin":
            return ".dylib"
        else:
            return ".so"

    def __init__(self):
        directory = os.path.dirname(utils.os.get_module_path())
        filename = "quick-lint-js-lib" + self.get_file_extension()
        # It's need multiple DLLs for load the library object on Windows,
        # these DLLs are all in the same folder, for ctypes find these DLLs
        # we need to change the current working directory to that folder.
        with changed_directory(directory):
            try:
                cdll = ctypes.CDLL(filename)
            except OSError as err:
                raise CException("") from err  # TODO: add message

        version = utils.sublime.major_version()
        self.c_create_parser = getattr(cdll, "qljs_st%d_create_parser" % (version))
        self.c_create_parser.argtypes = []
        self.c_create_parser.restype = CDocument.CPointer
        self.c_destroy_parser = getattr(cdll, "qljs_st%d_destroy_parser" % (version))
        self.c_destroy_parser.argtypes = [CDocument.CPointer]
        self.c_destroy_parser.restype = None
        if utils.sublime.is_version_three():
            self.c_set_text = cdll.qljs_st_3_set_text
            self.c_set_text.argtypes = [
                CDocument.CPointer, CText.CPointer,  # fmt: skip
            ]
            self.c_set_text.restype = CError.CPointer
        elif utils.sublime.is_version_four():
            self.c_replace_text = cdll.qljs_st_4_replace_text
            self.c_replace_text.argtypes = [
                CDocument.CPointer, CRange.CPointer, CText.CPointer,  # fmt: skip
            ]
            self.c_replace_text.restype = CError.CPointer
        self.c_lint = getattr(cdll, "qljs_st%d_lint" % (version))
        self.c_lint.argtypes = [CDocument.CPointer]
        self.c_lint.restype = CDiagnostic.CPointer

    def create_parser(self):
        c_parser_p = self.c_create_parser()
        if utils.ctypes.is_pointer_null(c_parser_p):
            raise CException("Parser unavailable.")
        return c_parser_p

    def destroy_parser(self, c_parser_p):
        if utils.ctypes.is_pointer_null(c_parser_p):
            raise CException("Cannot free nonexistent pointer.")
        self.c_destroy_parser(c_parser_p)

    if utils.sublime.is_version_three():

        def set_text(self, c_parser_p, c_text_p):
            return self.c_set_text(c_parser_p, c_text_p)

    elif utils.sublime.is_version_four():

        def replace_text(self, c_parser_p, c_range_p, c_text_p):
            return self.c_replace_text(c_parser_p, c_range_p, c_text_p)

    def lint(self, c_parser_p):
        return self.c_lint(c_parser_p)


class Severity:
    def __init__(self, value):
        self.value = value

    def is_error():
        return self.value == 1

    def is_warning():
        return self.value == 2


class Diagnostic:
    @classmethod
    def from_pointer(cls, c_diags_p):
        diags = []
        for c_diag in c_diags_p:
            if utils.ctypes.is_pointer_null(c_diag.message):
                break
            diags.append(Diagnostic(c_diag))
        return diags

    def __init__(self, c_diag, view):
        self.message = c_diag.message.decode()
        self.code = c_diag.code.decode()
        self.severity = Severity(c_diag.severity)
        if utils.sublime.is_version_three():
            start = c_diag.region.start
            end = c_diag.region.end
        elif utils.sublime.is_version_four():
            start = view.text_point_utf16(c_diag.start_line, c_diag.start_character)
            end = view.text_point_utf16(c_diag.end_line, c_diag.end_character)
        self.region = sublime.Region(start, end)


class Parser:
    try:
        c_lib = CLibrary()
    except CException as ex:
        sublime.error_message("quick-lint-js: " + str(ex))
    finally:
        c_lib = None

    def __init__(self, view):
        self.view = view
        self.diags = []
        try:
            self.c_parser_p = Parser.c_lib.object.create_parser()
        except AttributeError:
            raise ParserError("Library unavailable.")
        except CException:
            raise ParserError("Internal parser unavailable.")
        finally:
            self.c_parser_p = None

    def delete():
        try:
            Parser.c_lib.object.destroy_parser(self.c_parser_p)
        except CException:
            raise ParserError("Cannot delete pointer.")

    def set_text(self):
        content = utils.sublime.view_content(self.view).encode()
        c_text_p = CText(content, len(content)).lightweight_pointer()
        Parser.c_lib.object.set_text(self.c_parser_p, c_text)

    def replace_text(self, change):
        c_start = CPosition(change.a.row, change.a.col_utf16)
        c_end = CPosition(change.b.row, change.b.col_utf16)
        c_range_p = CRange(c_start, c_end).lightweight_pointer()
        content = change.str.encode()
        c_text_p = CText(content, len(content)).lightweight_pointer()
        Parser.c_lib.object.replace_text(self.c_parser_p, c_range_p, c_text_p)

    def lint(self):
        c_diags_p = Parser.c_lib.lint(self.c_parser_p)
        self.diags = Diagnostic.from_pointer(c_diags_p, self.view)


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
