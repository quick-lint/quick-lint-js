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


class BaseType:
    def __init_subclass__(cls, /, **kwargs):
        super().__init_subclass__(**kwargs)
        try:
            cls._fields_ = list(cls.fields.items())
        except AttributeError:
            pass

    @composed(classmethod, cache)
    def pointer(cls):
        return ctypes.POINTER(cls)


class BaseStruct(BaseType, ctypes.Structure):
    pass


class BaseUnion(BaseType, ctypes.Union):
    pass


class Diagnostic(BaseStruct):
    if SublimeUtils.major_version() == "3":
        fields = {
            "message": ctypes.c_char_p,
            "code": ctypes.c_char_p,
            "severity": ctypes.c_int,
            "begin_offset": ctypes.c_int,
            "end_offset": ctypes.c_int,
        }
    elif SublimeUtils.major_version() == "4":
        fields = {
            "message": ctypes.c_char_p,
            "code": ctypes.c_char_p,
            "severity": ctypes.c_int,
            "start_line": ctypes.c_int,
            "start_character": ctypes.c_int,
            "end_line": ctypes.c_int,
            "end_character": ctypes.c_int,
        }


class Error(BaseStruct):
    _fields_ = [
        ("message", ctypes.c_char_p),
    ]


class Result(BaseStruct):
    class Value(BaseUnion):
        _fields_ = [
            ("diagnostics", Diagnostic.pointer()),
            ("error", Error.pointer()),
        ]

    _fields_ = [
        ("value", Value),
        ("is_diagnostics", ctypes.c_bool),
    ]


class Parser(BaseStruct):
    _fields_ = []


def get_module_path():
    return os.path.realpath(__file__)


@contextmanager
def changed_directory(path):
    previous = os.getcwd()
    try:
        yield os.chdir(path)
    finally:
        os.chdir(previous)


# TODO: Create an custom exception and one with "os not supported" message
# TODO: Convert severity value to string ("Error"|"Warning")


class Object:
    def __init__(self, cdll):  # TODO: cdll is a good name?
        version = SublimeUtils.major_version()
        self.create_parser = getattr(cdll, "qljs_st%d_create_parser" % (version))
        self.create_parser.argtypes = []
        self.create_parser.restype = Parser.pointer()
        self.destroy_parser = getattr(cdll, "qljs_st%d_destroy_parser" % (version))
        self.destroy_parser.argtypes = [Parser.pointer()]
        self.destroy_parser.restype = None
        self.lint = getattr(cdll, "qljs_st%d_lint" % (version))
        self.lint.argtypes = [Parser.pointer()]
        self.lint.restype = Result.pointer()
        if version == "3":
            self.set_text = cdll.qljs_sublime_text_3_set_text
            self.set_text.argtypes = [
                Parser.pointer(),
                ctypes.c_void_p,
                ctypes.c_size_t,
            ]
            self.set_text.restype = Error.pointer()
        elif version == "4":
            self.replace_text = cdll.qljs_sublime_text_4_replace_text
            self.replace_text.argtypes = [
                Parser.pointer(), ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_void_p, ctypes.c_size_t,  # fmt: skip
            ]
            self.replace_text.restype = Error.pointer()


class Library:
    def __init__(self):
        self.directory = os.path.dirname(get_module_path())
        self.path = os.path.join(self.directory, self.filename)
        with changed_directory(self.directory):
            self.object = Object(ctypes.ctypes.CDLL(self.path))

    @cached_property
    def filename(self):
        if system() == "Windows":
            return "quick-lint-js-lib.dll"
        elif system() == "Darwin":
            return "libquick-lint-js-lib.dylib"  # TODO: Remove lib prefix with CMake
        elif system() == "Linux":
            return "libquick-lint-js-lib.so"


def load_library():
    Library = namedtuple("Library", ["filename", "directory", "path", "object"])
    library = Library()
    if platform.system() == "Windows":
        library.filename = "quick-lint-js-lib.dll"
    elif platform.system() == "Darwin":
        library.filename = "libquick-lint-js-lib.dylib"
    elif platform.system() == "Linux":
        library.filename = "libquick-lint-js-lib.so"
    else:
        raise OSError("Operating system not supported.")

    library.directory = os.path.dirname(get_module_path())
    library.path = os.path.join(library.directory, library.filename)

    # On Windows, for ctypes to load a library, it also needs to load
    # the dependencies of that library in case the library we want to
    # load, all its dependencies are in the same directory. For ctypes
    # to find these dependencies, we will need to temporarily change
    # the current working directory to the same location of these dependencies.

    # It's need multiple DLLs for load the library object on Windows, for ctypes find
    # these DLLs we need to change the current directory.

    # It's need multiple DLLs for load the library object on Windows, these DLLs
    # are all in the same folder, for ctypes find these DLLs we need to change
    # the current directory.
    with changed_directory(library.directory):
        library.object = ctypes.CDLL(lib_path)
    return library


# TODO: Very importart that you test if has async method and if not use normal method


class Error(Exception):
    """Error layer used to communicate with the plugin."""

    def __init__(self, ctypes_error):
        self.message = ctypes_error.message.decode(encoding="utf-8")
        super().__init__(self.message)

    def has_message(self):
        return bool(self.message)

    def display_message(self):
        base = "Error Message: quick-lint-js:\n"
        sublime.error_message(base + self.message)


def is_pointer_null(pointer):
    return not bool(pointer)


if SUBLIME_TEXT_MAJOR_VERSION == "3":

    def create_library():
        lib = load_library()

        lib.qljs_sublime_text_3_create_parser.argtypes = []
        lib.qljs_sublime_text_3_create_parser.restype = ParserPointer

        lib.qljs_sublime_text_3_destroy_parser.argtypes = [ParserPointer]
        lib.qljs_sublime_text_3_destroy_parser.restype = None

        lib.qljs_sublime_text_3_set_text.argtypes = [
            ParserPointer,
            ctypes.c_void_p,
            ctypes.c_size_t,
        ]
        lib.qljs_sublime_text_3_set_text.restype = ErrorPointer

        lib.qljs_sublime_text_3_lint.argtypes = [ParserPointer]
        lib.qljs_sublime_text_3_lint.restype = ResultPointer

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
            if is_pointer_null(self._ctypes_parser_pointer):
                raise MemoryError()

        def __del__(self):
            if not is_pointer_null(self._ctypes_parser_pointer):
                Parser.lib.qljs_sublime_text_3_destroy_parser(
                    self._ctypes_parser_pointer
                )
                self._ctypes_parser_pointer = None

        def set_text(self):
            view_size = self.view.size()
            all_region = sublime.Region(0, view_size)
            all_content = self.view.substr(all_region)
            text_utf8 = all_content.encode(encoding="utf-8")
            text_len_utf8 = len(text_utf8)
            ctypes_error_pointer = Parser.lib.qljs_sublime_text_3_set_text(
                self._ctypes_parser_pointer,
                text_utf8,
                text_len_utf8,
            )
            if not is_pointer_null(ctypes_error_pointer):
                ctypes_error = ctypes_error_pointer.contents
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
                    if is_pointer_null(ctypes_diagnostic.message):
                        break
                    diagnostics.append(Diagnostic(ctypes_diagnostic))
                self.diagnostics = diagnostics
            else:
                self.diagnostics = []
                ctypes_error_pointer = ctypes_result.value.error
                ctypes_error = ctypes_error_pointer.contents
                raise Error(ctypes_error)

elif SUBLIME_TEXT_MAJOR_VERSION == "4":

    def create_library():
        lib = load_library()

        lib.qljs_sublime_text_4_create_parser.argtypes = []
        lib.qljs_sublime_text_4_create_parser.restype = ParserPointer

        lib.qljs_sublime_text_4_destroy_parser.argtypes = [ParserPointer]
        lib.qljs_sublime_text_4_destroy_parser.restype = None

        lib.qljs_sublime_text_4_replace_text.argtypes = [
            ParserPointer,
            ctypes.c_int,
            ctypes.c_int,
            ctypes.c_int,
            ctypes.c_int,
            ctypes.c_void_p,
            ctypes.c_size_t,
        ]
        lib.qljs_sublime_text_4_replace_text.restype = ErrorPointer

        lib.qljs_sublime_text_4_lint.argtypes = [ParserPointer]
        lib.qljs_sublime_text_4_lint.restype = ResultPointer

        return lib

    class Diagnostic:
        """Diagnostic layer used to communicate with the plugin."""

        def __init__(self, ctypes_diagnostic, view):
            self.message = ctypes_diagnostic.message.decode(encoding="utf-8")
            self.code = ctypes_diagnostic.code.decode(encoding="utf-8")
            self.severity = ctypes_diagnostic.severity
            start_point = view.text_point_utf16(
                ctypes_diagnostic.start_line, ctypes_diagnostic.start_character
            )
            end_point = view.text_point_utf16(
                ctypes_diagnostic.end_line, ctypes_diagnostic.end_character
            )
            self.region = sublime.Region(start_point, end_point)

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
            self._ctypes_parser_pointer = Parser.lib.qljs_sublime_text_4_create_parser()
            if is_pointer_null(self._ctypes_parser_pointer):
                raise MemoryError()

        def __del__(self):
            if not is_pointer_null(self._ctypes_parser_pointer):
                Parser.lib.qljs_sublime_text_4_destroy_parser(
                    self._ctypes_parser_pointer
                )
                self._ctypes_parser_pointer = None

        def set_text(self):
            view_size = self.view.size()
            all_region = sublime.Region(0, view_size)
            all_content = self.view.substr(all_region)
            text_utf8 = all_content.encode(encoding="utf-8")
            text_len_utf8 = len(text_utf8)
            ctypes_error_pointer = Parser.lib.qljs_sublime_text_4_replace_text(
                self._ctypes_parser_pointer, 0, 0, 0, 0, text_utf8, text_len_utf8
            )
            if not is_pointer_null(ctypes_error_pointer):
                ctypes_error = ctypes_error_pointer.contents
                raise Error(ctypes_error)

        def replace_text(self, change):
            replacement_text_utf8 = change.str.encode(encoding="utf-8")
            replacement_text_len_utf8 = len(replacement_text_utf8)
            ctypes_error_pointer = Parser.lib.qljs_sublime_text_4_replace_text(
                self._ctypes_parser_pointer,
                change.a.row,
                change.a.col_utf16,
                change.b.row,
                change.b.col_utf16,
                replacement_text_utf8,
                replacement_text_len_utf8,
            )
            if not is_pointer_null(ctypes_error_pointer):
                ctypes_error = ctypes_error_pointer.contents
                raise Error(ctypes_error)

        def lint(self):
            ctypes_result_pointer = Parser.lib.qljs_sublime_text_4_lint(
                self._ctypes_parser_pointer
            )
            ctypes_result = ctypes_result_pointer.contents
            if ctypes_result.is_diagnostics:
                ctypes_diagnostics_pointer = ctypes_result.value.diagnostics
                diagnostics = []
                for ctypes_diagnostic in ctypes_diagnostics_pointer:
                    if is_pointer_null(ctypes_diagnostic.message):
                        break
                    diagnostics.append(Diagnostic(ctypes_diagnostic, self.view))
                self.diagnostics = diagnostics
            else:
                self.diagnostics = []
                ctypes_error_pointer = ctypes_result.value.error
                ctypes_error = ctypes_error_pointer.contents
                raise Error(ctypes_error)


# Just for the sake of clarity, you can think of a buffer as a block of
# memory that contains the file's text and a view as a tab in the
# sublime text.

if SUBLIME_TEXT_MAJOR_VERSION == "3":

    class Buffer:
        """Represents the file's text buffer.

        Multiple view objects may share the same buffer.
        """

        def __init__(self, view):
            self.views = [view]
            self.parser = Parser(view)

    class BuffersManager:
        """Manages the buffers."""

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

    class QuickLintJsListener(sublime_plugin.ViewEventListener):
        """Listens for events bound to a specific view."""

        # The internal strategy used is to share information between
        # all views that belong to the same buffer. Because that way,
        # if there are multiple views (tabs) of the same buffer (file),
        # they will all apply the same changes (have squiggly underlines
        # and pop-ups available).
        #
        # More details:
        #     Inside the quick-lint-js/docs/SUBLIME_TEXT.md or
        #     https://github.com/quick-lint/quick-lint-js/pull/328#issuecomment-869038036

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

    class PluginBuffer:
        """Represents the file's text buffer.

        Implementation specifically for this plugin.

        Multiple view objects may share the same buffer.
        """

        def __init__(self, view):
            self.views = {view}
            self.parser = Parser(view)

    class PluginBuffersManager:
        """Manages the plugin buffers."""

        def __init__(self):
            self.plugin_buffers = {}

        def add_view(self, view):
            id_ = view.buffer_id()
            if id_ not in self.plugin_buffers:
                self.plugin_buffers[id_] = PluginBuffer(view)
            else:
                self.plugin_buffers[id_].views.add(view)
            return self.plugin_buffers[id_]

        def remove_view(self, view):
            id_ = view.buffer_id()
            if id_ not in self.plugin_buffers:
                return
            self.plugin_buffers[id_].views.discard(view)
            if not self.plugin_buffers[id_].views:
                del self.plugin_buffers[id_]

        def get_plugin_buffer(self, id_):
            return self.plugin_buffers[id_]

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
