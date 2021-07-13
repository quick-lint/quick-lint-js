# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

"""Shows to the user the diagnostics provided by quick-lint-js."""

# Just for the sake of clarity, you can think of a buffer as a block of
# memory that contains the file's text and a view as a tab in the
# sublime text.

import sublime
from sublime_plugin import ViewEventListener, TextChangeListener

from . import c_api


class PluginBuffer:
    """Represents the file's text buffer.

    Implementation specifically for this plugin.

    Multiple view objects may share the same buffer.
    """

    def __init__(self, view):
        self.views = set((view,))
        self.parser = c_api.Parser(view)


class PluginBuffersManager:
    """Manages the plugin buffers."""

    def __init__(self):
        self.plugin_buffers = {}

    def add_view(self, view, id_):
        if id_ not in self.plugin_buffers:
            self.plugin_buffers[id_] = PluginBuffer(view)
        else:
            self.plugin_buffers[id_].views.add(view)

    def remove_view(self, view, id_):
        if id_ not in self.plugin_buffers:
            return
        self.plugin_buffers[id_].views.discard(view)
        if not self.plugin_buffers[id_].views:
            del self.plugin_buffers[id_]

    def get_plugin_buffer(self, id_):
        return self.plugin_buffers[id_]


# The internal strategy used is to share information between
# all views that belong to the same buffer. Because that way,
# if there are multiple views (tabs) of the same buffer (file),
# they will all apply the same changes (have squiggly underlines
# and pop-ups available):
#
# https://github.com/quick-lint/quick-lint-js/pull/328#issuecomment-869038036


class QuickLintJsListener:
    """Base for the listeners present in this plugin."""

    plugin_buffers_manager = PluginBuffersManager()

    def __init__(self, view):
        if not view:
            return
        self.view = view
        self.buffer_id = view.buffer_id()
        self.plugin_buffers_manager.add_view(self.view, self.buffer_id)
        self.plugin_buffer = self.plugin_buffers_manager.get_plugin_buffer(
            self.buffer_id
        )

    def __del__(self):
        if hasattr(self, "view") and hasattr(self, "buffer_id"):
            self.plugin_buffers_manager.remove_view(self.view, self.buffer_id)

    def add_squiggly_underlines(self):
        warning_regions, error_regions = self.get_severity_regions()
        flags = (
            sublime.DRAW_SQUIGGLY_UNDERLINE
            | sublime.DRAW_NO_FILL
            | sublime.DRAW_NO_OUTLINE
        )
        for view in self.plugin_buffer.views:
            view.add_regions("2", warning_regions, "region.orangish", "", flags)
            view.add_regions("1", error_regions, "region.redish", "", flags)

    def get_severity_regions(self):
        warning_regions = []
        error_regions = []
        for diagnostic in self.plugin_buffer.parser.diagnostics:
            if c_api.SeverityEnumeration.WARNING == diagnostic.severity:
                warning_regions.append(diagnostic.region)
            elif c_api.SeverityEnumeration.ERROR == diagnostic.severity:
                error_regions.append(diagnostic.region)
        return warning_regions, error_regions


class QuickLintJsViewEventListener(ViewEventListener, QuickLintJsListener):
    """Listens for events bound to a specific view."""

    @classmethod
    def is_applicable(cls, settings):
        syntax = settings.get("syntax", "")
        return "JavaScript" in syntax

    @classmethod
    def applies_to_primary_view_only(cls):
        return False

    def __init__(self, view):
        ViewEventListener.__init__(self, view)
        QuickLintJsListener.__init__(self, view)
        self.on_load()

    def on_load(self):
        self.plugin_buffer.parser.set_text()
        self.plugin_buffer.parser.lint()
        self.add_squiggly_underlines()

    def on_reload(self):
        # This function is called, for example, when a file is changed
        # outside of the editor:
        #
        # https://github.com/sublimehq/sublime_text/issues/9#issuecomment-16922940
        self.on_load()

    def on_revert(self):
        # This function is called, for example, when the user clicks
        # the menu entry under `File | Revert file`
        #
        # https://superuser.com/q/815045
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
        <span>%s
            <span style=\"color: %s;\">quick-lint-js(%s)</span>
        </span>
        """
        color = self.view.style_for_scope("comment.line")["foreground"]
        content = minihtml % (diagnostic.message, color, diagnostic.code)

        flags = sublime.HIDE_ON_MOUSE_MOVE_AWAY
        location = diagnostic.region.begin()
        max_width, max_height = (1366, 768)
        self.view.show_popup(content, flags, location, max_width, max_height)


class QuickLintJsTextChangeListener(TextChangeListener, QuickLintJsListener):
    """Event handling about text changes made to a specific buffer."""

    @classmethod
    def is_applicable(cls, buffer):
        settings = buffer.primary_view().settings()
        syntax = settings.get("syntax", "")
        return "JavaScript" in syntax

    def __init__(self):
        TextChangeListener.__init__(self)
        QuickLintJsListener.__init__(self, None)

    def on_text_changed(self, changes):
        self.plugin_buffer = (
            QuickLintJsListener.plugin_buffers_manager.get_plugin_buffer(
                self.buffer.id()
            )
        )
        for change in changes:
            self.plugin_buffer.parser.replace_text(change)
        self.plugin_buffer.parser.lint()
        self.add_squiggly_underlines()


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
