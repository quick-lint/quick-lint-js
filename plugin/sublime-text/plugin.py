# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import ctypes
import html
import os
from contextlib import contextmanager
from functools import lru_cache
from platform import system

import sublime
import sublime_plugin


# TODO: Very importart that you test if has async method and if not use normal method


def cache(func):
    return lru_cache(maxsize=None)(func)


def cached_property(func):
    return property(cache(func))


def composed(*decs):
    def decorator(func):
        for dec in reversed(decs):
            func = dec(func)
        return func

    return decorator


class SublimeUtils:
    @composed(staticmethod, cache)
    def major_version():
        return sublime.version()[0]

    @composed(classmethod, cache)
    def is_three(cls):
        return cls.major_version() == "3"

    @composed(staticmethod, cache)
    def is_four(cls):
        return cls.major_version() == "4"

    @staticmethod
    def error_message(msg):
        sublime.error_message("quick-lint-js: " + msg)

    @staticmethod
    def view_content(view):
        region = sublime.Region(0, view.size())
        return view.substr(region)


def remove_prefix(text, prefix):
    if text.startswith(prefix):
        return text[len(prefix) :]
    return text


class CTypesUtils:
    @staticmethod
    def ptr(obj):
        return ctypes.byref(obj)

    @staticmethod
    def is_ptr_null(ptr):
        return not bool(ptr)


class CTypesMetaclass(type):
    def __new__(cls, clsname, clsbases, clsattrs):
        for attr in dir(ctypes):
            if attr.startswith("c_"):
                name = remove_prefix(attr, "c_")
                value = getattr(ctypes, attr)
                clsattrs[name] = value
        return super().__new__(cls, clsname, clsbases, clsattrs)


class CTypes(metaclass=CTypesMetaclass):
    pass


class CStruct:
    def __init_subclass__(cls, /, **kwargs):
        super().__init_subclass__(**kwargs)
        try:
            cls._fields_ = list(cls.fields.items())
        except AttributeError:
            pass

    @composed(classmethod, cache)
    def ptrtype(cls):
        return ctypes.POINTER(cls)

    @cache
    def ptr(self):
        return CTypesUtils.ptr(self)


class CText(CStruct):
    fields = {
        "content": CTypes.char_p,
        "length": CTypes.size_t,
    }


class CPosition(CStruct):
    fields = {
        "line": CTypes.uint,
        "character": CTypes.uint,
    }


class CRegion(CStruct):
    fields = {
        "start": CTypes.uint,
        "end": CTypes.uint,
    }


class CRange(CStruct):
    fields = {
        "start": CPosition,
        "end": CPosition,
    }


class CDiagnostic(CStruct):
    fields = {
        "message": CTypes.char_p,
        "code": CTypes.char_p,
        "severity": CTypes.int,
    }
    if SublimeUtils.is_three():
        fields["region"] = CRegion.ptrtype()
    elif SublimeUtils.is_four():
        fields["range"] = CRange.ptrtype()


class CParser(Cstruct):
    fields = {}


def get_module_path():
    return os.path.realpath(__file__)


@contextmanager
def changed_directory(path):
    previous = os.getcwd()
    try:
        yield os.chdir(path)
    finally:
        os.chdir(previous)


class CException(Exception):
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
        if system() == "Windows":
            return "quick-lint-js-lib.dll"
        elif system() == "Darwin":
            return "quick-lint-js-lib.dylib"
        elif system() == "Linux":
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


# TODO: self.diags or return?
# TODO: views = set or list?


class Buffer:
    def __init__(self, view):
        self.views = [view]
        self.parser = Parser(view)


class Buffer:
    def __init__(self, view):
        self.views = {view}
        self.parser = Parser(view)


class BuffersManager:
    def __init__(self):
        self.buffers = {}

    def add_view(self, view):
        id_ = view.buffer_id()
        if id_ not in self.buffers:
            self.buffers[id_] = Buffer(view)
        else:
            self.buffers[id_].views.append(view)
        return self.buffers[id_]

    def remove_view(self, view):
        id_ = view.buffer_id()
        self.buffers[id_].views.remove(view)
        if not self.buffers[id_].views:
            del self.buffers[id_]


class BuffersManager:
    def __init__(self):
        self.buffers = {}

    def add_view(self, view):
        id_ = view.buffer_id()
        if id_ not in self.buffers:
            self.buffers[id_] = Buffer(view)
        else:
            self.buffers[id_].views.add(view)
        return self.buffers[id_]

    def remove_view(self, view):
        id_ = view.buffer_id()
        if id_ not in self.buffers:
            return
        self.buffers[id_].views.discard(view)
        if not self.buffers[id_].views:
            del self.buffers[id_]

    def get_buffer(self, id_):
        return self.buffers[id_]



# Just for the sake of clarity, you can think of a buffer as a block of
# memory that contains the file's text and a view as a tab in the
# sublime text.

if SUBLIME_TEXT_MAJOR_VERSION == "3":



    class QuickLintJsListener(sublime_plugin.ViewEventListener):
        """Listens for events bound to a specific view."""

        # The internal strategy used is to share information between
        # all views that belong to the same buffer. Because that way,
        # if there are multiple views (tabs) of the same buffer (file),
        # they will all apply the same changes (have squiggly underlines
        # and pop-ups available).

        buffers_manager = BuffersManager()

        @classmethod
        def is_applicable(cls, settings):
            if not Parser.is_working():
                return False
            syntax = settings.get("syntax", "")
            return "JavaScript.sublime-syntax" in syntax

        @classmethod
        def applies_to_primary_view_only(cls):
            return False

        # Won't on_load get called anyway? Why do we need to explicitly lint
        # in __init__?
        #
        # Answer:
        #     Inside the quick-lint-js/docs/SUBLIME_TEXT.md or
        #     https://github.com/quick-lint/quick-lint-js/pull/328#discussion_r670077226

        def __init__(self, view):
            """Called when the view is finished loading."""
            super().__init__(view)
            self.buffer = QuickLintJsListener.buffers_manager.add_view(self.view)
            self.on_modified()

        def __del__(self):
            QuickLintJsListener.buffers_manager.remove_view(self.view)

        def on_load(self):
            """Called when the file is finished loading."""
            self.on_modified()

        def on_modified(self):
            try:
                self.buffer.parser.set_text()
                self.buffer.parser.lint()
                self.add_squiggly_underlines()
            except Error as error:
                self.remove_squiggly_underlines()
                if error.has_message():
                    error.display_message()

        def on_hover(self, point, hover_zone):
            if hover_zone == sublime.HOVER_TEXT:
                for diagnostic in self.buffer.parser.diagnostics:
                    # If the user hovers over the diagnostic region
                    # (region with squiggly underlines).
                    if diagnostic.region.contains(point):
                        self.add_popup(diagnostic)

        def add_squiggly_underlines(self):
            warning_regions, error_regions = self.get_regions_by_severity()
            flags = (
                sublime.DRAW_SQUIGGLY_UNDERLINE
                | sublime.DRAW_NO_FILL
                | sublime.DRAW_NO_OUTLINE
            )
            for view in self.buffer.views:
                view.add_regions("2", warning_regions, "region.orangish", "", flags)
                view.add_regions("1", error_regions, "region.redish", "", flags)

        def remove_squiggly_underlines(self):
            for view in self.buffer.views:
                view.erase_regions("2")
                view.erase_regions("1")

        def get_regions_by_severity(self):
            warning_regions = []
            error_regions = []
            for diagnostic in self.buffer.parser.diagnostics:
                if Severity.warning == diagnostic.severity:
                    warning_regions.append(diagnostic.region)
                elif Severity.error == diagnostic.severity:
                    error_regions.append(diagnostic.region)
            return warning_regions, error_regions

        def add_popup(self, diagnostic):
            minihtml = """
            <body style="margin: 0.8rem;">
                <div>%s</div>
                <div style="color: %s;">quick-lint-js(%s)</div>
            </body>
            """
            color = self.view.style_for_scope("comment.line")["foreground"]

            # Sublime Text 3 parser cannot interpret escaped quotes.
            # >
            # > Parse Error: quot; code: Unknown entity
            # >
            content = minihtml % (
                html.escape(diagnostic.message, quote=False),
                html.escape(color, quote=False),
                html.escape(diagnostic.code, quote=False),
            )

            flags = sublime.HIDE_ON_MOUSE_MOVE_AWAY
            location = diagnostic.region.begin()
            max_width, max_height = (1280, 720)  # 1280x720 Screen Resolution
            self.view.show_popup(content, flags, location, max_width, max_height)

elif SUBLIME_TEXT_MAJOR_VERSION == "4":



    class QuickLintJsListener:
        """Base for the listeners present in this plugin."""

        # The internal strategy used is to share information between
        # all views that belong to the same buffer. Because that way,
        # if there are multiple views (tabs) of the same buffer (file),
        # they will all apply the same changes (have squiggly underlines
        # and pop-ups available).
        #
        # More details:
        #     Inside the quick-lint-js/docs/SUBLIME_TEXT.md or
        #     https://github.com/quick-lint/quick-lint-js/pull/328#issuecomment-869038036

        plugin_buffers_manager = PluginBuffersManager()

        def __init__(self, view):
            if view is None:
                return
            self.view = view
            self.plugin_buffer = QuickLintJsListener.plugin_buffers_manager.add_view(
                self.view
            )

        def __del__(self):
            if hasattr(self, "view"):
                QuickLintJsListener.plugin_buffers_manager.remove_view(self.view)

        def add_squiggly_underlines(self):
            warning_regions, error_regions = self.get_regions_by_severity()
            flags = (
                sublime.DRAW_SQUIGGLY_UNDERLINE
                | sublime.DRAW_NO_FILL
                | sublime.DRAW_NO_OUTLINE
            )
            for view in self.plugin_buffer.views:
                view.add_regions("2", warning_regions, "region.orangish", "", flags)
                view.add_regions("1", error_regions, "region.redish", "", flags)

        def remove_squiggly_underlines(self):
            for view in self.plugin_buffer.views:
                view.erase_regions("2")
                view.erase_regions("1")

        def get_regions_by_severity(self):
            warning_regions = []
            error_regions = []
            for diagnostic in self.plugin_buffer.parser.diagnostics:
                if Severity.warning == diagnostic.severity:
                    warning_regions.append(diagnostic.region)
                elif Severity.error == diagnostic.severity:
                    error_regions.append(diagnostic.region)
            return warning_regions, error_regions

    class QuickLintJsViewEventListener(
        sublime_plugin.ViewEventListener, QuickLintJsListener
    ):
        """Listens for events bound to a specific view."""

        @classmethod
        def is_applicable(cls, settings):
            if not Parser.is_working():
                return False
            syntax = settings.get("syntax", "")
            return "JavaScript.sublime-syntax" in syntax

        @classmethod
        def applies_to_primary_view_only(cls):
            return False

        # Won't on_load get called anyway? Why do we need to explicitly lint
        # in __init__?
        #
        # Answer:
        #     Inside the quick-lint-js/docs/SUBLIME_TEXT.md or
        #     https://github.com/quick-lint/quick-lint-js/pull/328#discussion_r670077226

        def __init__(self, view):
            """Called when the view is finished loading."""
            ViewEventListener.__init__(self, view)
            QuickLintJsListener.__init__(self, view)
            self.on_load()

        def on_load(self):
            """Called when the file is finished loading."""
            try:
                self.plugin_buffer.parser.set_text()
                self.plugin_buffer.parser.lint()
                self.add_squiggly_underlines()
            except Error as error:
                self.remove_squiggly_underlines()
                if error.has_message():
                    error.display_message()

        def on_reload(self):
            """Called, for example, when a file is changed outside of the editor.

            More details:
                https://github.com/sublimehq/sublime_text/issues/9#issuecomment-16922940
            """
            self.on_load()

        def on_revert(self):
            """Called, for example, through menu entry under `File | Revert file`.

            More details:
                https://superuser.com/q/815045
            """
            self.on_load()

        def on_hover(self, point, hover_zone):
            if hover_zone == sublime.HOVER_TEXT:
                for diagnostic in self.plugin_buffer.parser.diagnostics:
                    # If the user hovers over the diagnostic region
                    # (region with squiggly underlines).
                    if diagnostic.region.contains(point):
                        self.add_popup(diagnostic)

        def add_popup(self, diagnostic):
            minihtml = """
            <body style="margin: 0.8rem;">
                <div>%s</div>
                <div style="color: %s;">quick-lint-js(%s)</div>
            </body>
            """
            color = self.view.style_for_scope("comment.line")["foreground"]
            content = minihtml % (
                html.escape(diagnostic.message),
                html.escape(color),
                html.escape(diagnostic.code),
            )

            flags = sublime.HIDE_ON_MOUSE_MOVE_AWAY
            location = diagnostic.region.begin()
            max_width, max_height = (1280, 720)  # 1280x720 Screen Resolution
            self.view.show_popup(content, flags, location, max_width, max_height)

    class QuickLintJsTextChangeListener(
        sublime_plugin.TextChangeListener, QuickLintJsListener
    ):
        """Event handling about text changes made to a specific buffer."""

        @classmethod
        def is_applicable(cls, buffer):
            if not Parser.is_working():
                return False
            settings = buffer.primary_view().settings()
            syntax = settings.get("syntax", "")
            return "JavaScript.sublime-syntax" in syntax

        def __init__(self):
            TextChangeListener.__init__(self)
            QuickLintJsListener.__init__(self, None)

        def on_text_changed(self, changes):
            self.plugin_buffer = (
                QuickLintJsListener.plugin_buffers_manager.get_plugin_buffer(
                    self.buffer.id()
                )
            )
            try:
                for change in changes:
                    self.plugin_buffer.parser.replace_text(change)
                self.plugin_buffer.parser.lint()
                self.add_squiggly_underlines()
            except Error as error:
                self.remove_squiggly_underlines()
                if error.has_message():
                    error.display_message()


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
