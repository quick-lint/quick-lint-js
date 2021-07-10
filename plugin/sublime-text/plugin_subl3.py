# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

"""Shows to the user the diagnostics provided by quick-lint-js."""

# Just for the sake of clarity, you can think of a buffer as a block of
# memory that contains the file's text and a view as a tab in the
# sublime text.

import sublime
import sublime_plugin

from . import c_api


class Buffer:
    """Represents the file's text buffer.

    Multiple view objects may share the same buffer.
    """

    def __init__(self, view):
        self.views = [view]
        self.parser = c_api.Parser(view)


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

    def remove_view(self, view):
        id_ = view.buffer_id()
        self.buffers[id_].views.remove(view)
        if not self.buffers[id_].views:
            del self.buffers[id_]

    def get_buffer(self, view):
        id_ = view.buffer_id()
        return self.buffers[id_]


class QuickLintJsListener(sublime_plugin.ViewEventListener):
    """Listens for events bound to a specific view."""

    # The internal strategy used is to share information between
    # all views that belong to the same buffer. Because that way,
    # if there are multiple views (tabs) of the same buffer (file),
    # they will all apply the same changes (have squiggly underlines
    # and pop-ups available):
    #
    # https://github.com/quick-lint/quick-lint-js/pull/328#issuecomment-869038036

    buffers_manager = BuffersManager()

    @classmethod
    def is_applicable(cls, settings):
        syntax = settings.get("syntax", "")
        if "JavaScript" in syntax:
            return True
        return False

    @classmethod
    def applies_to_primary_view_only(cls):
        return False

    def __init__(self, view):
        super().__init__(view)
        self.buffers_manager.add_view(self.view)
        self.buffer = self.buffers_manager.get_buffer(self.view)
        self.on_modified()

    def __del__(self):
        self.buffers_manager.remove_view(self.view)

    def on_load(self):
        self.on_modified()

    def on_modified(self):
        self.buffer.parser.set_text()
        self.buffer.parser.lint()
        self.add_squiggly_underlines()

    def on_hover(self, point, hover_zone):
        if hover_zone == sublime.HOVER_TEXT:
            for diagnostic in self.buffer.parser.diagnostics:
                # If the user hovers over the diagnostic region
                # (region with squiggly underlines).
                if diagnostic.region.contains(point):
                    self.add_popup(diagnostic)

    def add_squiggly_underlines(self):
        warning_regions, error_regions = self.get_severity_regions()
        flags = (
            sublime.DRAW_SQUIGGLY_UNDERLINE
            | sublime.DRAW_NO_FILL
            | sublime.DRAW_NO_OUTLINE
        )
        for view in self.buffer.views:
            view.add_regions("2", warning_regions, "region.orangish", "", flags)
            view.add_regions("1", error_regions, "region.redish", "", flags)

    def get_severity_regions(self):
        warning_regions = []
        error_regions = []
        for diagnostic in self.buffer.parser.diagnostics:
            if c_api.SeverityEnumeration.WARNING == diagnostic.severity:
                warning_regions.append(diagnostic.region)
            elif c_api.SeverityEnumeration.ERROR == diagnostic.severity:
                error_regions.append(diagnostic.region)
        return warning_regions, error_regions

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
