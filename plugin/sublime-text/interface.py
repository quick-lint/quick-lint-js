# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import ctypes
import platform
import os


class CStruct:
    def __init_subclass__(cls, /, **kwargs):
        super().__init_subclass__(**kwargs)
        try:
            cls._fields_ = list(cls.fields.items())
        except AttributeError:
            pass

    @cached_classmethod
    def ptrtype(cls):
        return ctypes.POINTER(cls)

    @cache
    def ptr(self):
        return CTypesUtils.ptr(self)


class CText(CStruct):
    fields = {
        "content": ctypes.c_char_p,
        "length": ctypes.c_size_t,
    }


class CPosition(CStruct):
    fields = {
        "line": ctypes.c_uint,
        "character": ctypes.c_uint,
    }


class CRegion(CStruct):
    fields = {
        "start": ctypes.c_uint,
        "end": ctypes.c_uint,
    }


class CRange(CStruct):
    fields = {
        "start": CPosition,
        "end": CPosition,
    }


class CDiagnostic(CStruct):
    fields = {
        "message": ctypes.c_char_p,
        "code": ctypes.c_char_p,
        "severity": ctypes.c_int,
    }
    if SublimeUtils.is_three():
        fields["region"] = CRegion.ptrtype()
    elif SublimeUtils.is_four():
        fields["range"] = CRange.ptrtype()


class CParser(Cstruct):
    fields = {}


class CException(Exception):  # TODO: CException or CError
    pass


class CObject:
    @staticmethod
    def cdll(path):  # TODO: `cdll` is a good name?
        # It's need multiple DLLs for load the library object on Windows,
        # these DLLs are all in the same folder, for ctypes find these DLLs
        # we need to change the current working directory to that folder.
        with changed_directory(os.path.dirname(directory)):
            return ctypes.CDLL(path)

    def __init__(self, path):
        cdll = CObject.cdll(path)
        version = SublimeUtils.major_version()
        self.c_create_parser = getattr(cdll, "qljs_st%d_create_parser" % (version))
        self.c_create_parser.argtypes = []
        self.c_create_parser.restype = CParser.ptrtype()
        self.c_destroy_parser = getattr(cdll, "qljs_st%d_destroy_parser" % (version))
        self.c_destroy_parser.argtypes = [CParser.ptrtype()]
        self.c_destroy_parser.restype = None
        self.lint = getattr(cdll, "qljs_st%d_lint" % (version))
        self.lint.argtypes = [CParser.ptrtype()]
        self.lint.restype = CDiagnostic.ptrtype()
        if version == "3":
            self.set_text = cdll.qljs_st_3_set_text
            self.set_text.argtypes = [CParser.ptrtype(), CText]
            self.set_text.restype = CError.ptrtype()
        elif version == "4":
            self.replace_text = cdll.qljs_st_4_replace_text
            self.replace_text.argtypes = [CParser.ptrtype(), CRange, CText]
            self.replace_text.restype = CError.ptrtype()

    def create_parser(self):
        c_parser_p = self.c_create_parser()
        if CTypesUtils.is_ptr_null(c_parser_p):
            raise CException("Parser unavailable.")
        return c_parser_p

    def destroy_parser(self, c_parser_p):
        if CTypesUtils.is_ptr_null(c_parser_p):
            raise CException("Cannot free nonexistent ptr.")
        self.c_destroy_parser(c_parser_p)


class CLibrary:
    def __init__(self):
        self.directory = os.path.dirname(get_module_path())
        self.path = os.path.join(self.directory, self.filename)
        try:
            self.object = CObject(self.path)
        except OSError:
            raise CException("Failed to load library object.")

    @cached_property
    def filename(self):  # TODO: Remove lib prefix with CMake
        if platform.system() == "Windows":
            return "quick-lint-js-lib.dll"
        elif platform.system() == "Darwin":
            return "quick-lint-js-lib.dylib"
        elif platform.system() == "Linux":
            return "quick-lint-js-lib.so"
        else:
            raise CException("Operating system not supported.")


class Severity:
    def __init__(self, value):
        self.value

    def is_error():
        return self.value == 1

    def is_warning():
        return self.value == 2


class Diagnostic:
    @classmethod
    def from_ptr(cls, c_diags_p):
        diags = []
        for c_diag in c_diags_p:
            if CTypesUtils.is_ptr_null(c_diag.message):
                break
            diags.append(Diagnostic(c_diag))
        return diags

    def __init__(self, c_diag, view):
        self.message = c_diag.message.decode()
        self.code = c_diag.code.decode()
        self.severity = Severity(c_diag.severity)
        if SublimeUtils.is_three():
            start = c_diag.region.start
            end = c_diag.region.end
        elif SublimeUtils.is_four():
            start = view.text_point_utf16(c_diag.start_line, c_diag.start_character)
            end = view.text_point_utf16(c_diag.end_line, c_diag.end_character)
        self.region = sublime.Region(start, end)


class Parser:
    try:
        c_lib = CLibrary()
    except CException as ex:
        SublimeUtils.error_message(str(ex))
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
        content = SublimeUtils.view_content(self.view).encode()
        c_text_p = CText(content, len(content)).ptr()
        Parser.c_lib.object.set_text(self.c_parser_p, c_text)

    def replace_text(self, change):
        c_start = CPosition(change.a.row, change.a.col_utf16)
        c_end = CPosition(change.b.row, change.b.col_utf16)
        c_range_p = CRange(c_start, c_end).ptr()
        content = change.str.encode()
        c_text_p = CText(content, len(content)).ptr()
        Parser.c_lib.object.replace_text(self.c_parser_p, c_range_p, c_text_p)

    def lint(self):
        c_diags_p = Parser.c_lib.lint(self.c_parser_p)
        self.diags = Diagnostic.from_ptr(c_diags_p, self.view)


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
