# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import sublime
import sublime_plugin

from . import c_api


class Buffer:
    def __init__(self):
        self.views = []
        self.parser = c_api.Parser()
        self.diagnostics = []


class QuickLintJsListener(sublime_plugin.ViewEventListener):
    _buffers = {}

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
        buffer_id = view.buffer_id()
        if not self._buffers.get(buffer_id):
            self._buffers[buffer_id] = Buffer()
        self.buffer = self._buffers[buffer_id]
        self.buffer.views.append(self.view)
        self.on_modified()

    def __del__(self):
        self.buffer.views.remove(self.view)

    def on_load(self):
        self.on_modified()

    def on_modified(self):
        viewsize = self.view.size()
        allregion = sublime.Region(0, viewsize)
        allcontent = self.view.substr(allregion)
        self.buffer.parser.set_text(allcontent)
        self.buffer.diagnostics = self.buffer.parser.lint()
        self._add_squiggly_underlines()

    def on_hover(self, point, hover_zone):
        if hover_zone == sublime.HOVER_TEXT:
            for diag in self.buffer.diagnostics:
                if diag.begin_offset <= point <= diag.end_offset:
                    self._add_popup(diag)

    def _add_squiggly_underlines(self):
        warning_regions = []
        error_regions = []
        for diag in self.buffer.diagnostics:
            region = sublime.Region(diag.begin_offset, diag.end_offset)
            if diag.severity == c_api.SeverityEnumeration.WARNING:
                warning_regions.append(region)
            elif diag.severity == c_api.SeverityEnumeration.ERROR:
                error_regions.append(region)

        flags = (
            sublime.DRAW_SQUIGGLY_UNDERLINE
            | sublime.DRAW_NO_FILL
            | sublime.DRAW_NO_OUTLINE
        )
        for view in self.buffer.views:
            view.add_regions("2", warning_regions, "region.orangish", "", flags)
            view.add_regions("1", error_regions, "region.redish", "", flags)

    def _add_popup(self, diagnostic):
        minihtml = """
        <span>%s
            <span style=\"color: %s;\">quick-lint-js(%s)</span>
        </span>
        """
        color = self.view.style_for_scope("comment.line")["foreground"]
        content = minihtml % (diagnostic.message, color, diagnostic.code)

        flags = sublime.HIDE_ON_MOUSE_MOVE_AWAY
        location = diagnostic.begin_offset
        max_width, max_height = self.view.viewport_extent()
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
