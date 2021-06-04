# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

import sublime
import sublime_plugin

from . import c_api


class QuickLintJsListener(sublime_plugin.ViewEventListener):
    @classmethod
    def is_applicable(cls, settings):
        syntax = settings.get("syntax")
        if "JavaScript" in syntax:
            return True
        return False

    def __init__(self, view):
        self.view = view
        self.parser = c_api.Parser()
        self.diagnostics = []
        self.on_modified()

    def on_load(self):
        self.on_modified()

    def on_modified(self):
        viewsize = self.view.size()
        allregion = sublime.Region(0, viewsize)
        allcontent = self.view.substr(allregion)
        self.parser.set_text(allcontent, viewsize)
        self.diagnostics = self.parser.lint()
        self._add_squiggly_underlines()

    def on_hover(self, point, hover_zone):
        if hover_zone == sublime.HOVER_TEXT:
            self._add_popup(point)

    def _add_squiggly_underlines(self):
        error_regions = []
        warning_regions = []
        for diag in self.diagnostics:
            region = sublime.Region(diag.begin_offset, diag.end_offset)
            if diag.severity == c_api.SeverityEnumeration.ERROR:
                error_regions.append(region)
            elif diag.severity == c_api.SeverityEnumeration.WARNING:
                warning_regions.append(region)

        flags = (
            sublime.DRAW_SQUIGGLY_UNDERLINE
            | sublime.DRAW_NO_FILL
            | sublime.DRAW_NO_OUTLINE
        )
        self.view.add_regions("1", error_regions, "invalid.illegal", "", flags)
        self.view.add_regions("2", warning_regions, "invalid.deprecated", "", flags)

    def _add_popup(self, point):
        for diag in self.diagnostics:
            if diag.begin_offset <= point <= diag.end_offset:
                minihtml = """
                <span>%(message)s
                    <span style=\"color: %(color)s;\">quick-lint-js(%(code)s)</span>
                </span>
                """
                content = minihtml % {
                    "message": diag.message,
                    "code": diag.code,
                    "color": self.view.style_for_scope("comment.line")["foreground"],
                }
                flags = sublime.HIDE_ON_MOUSE_MOVE_AWAY
                location = diag.begin_offset
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
