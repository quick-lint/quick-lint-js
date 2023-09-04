// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <quick-lint-js/logging/trace-types.h>

namespace quick_lint_js {
bool operator==(const Trace_Event_Header& lhs, const Trace_Event_Header& rhs) {
  return lhs.timestamp == rhs.timestamp;
}

bool operator!=(const Trace_Event_Header& lhs, const Trace_Event_Header& rhs) {
  return !(lhs == rhs);
}

bool operator==(const Trace_Event_Init& lhs, const Trace_Event_Init& rhs) {
  return lhs.version == rhs.version;
}

bool operator!=(const Trace_Event_Init& lhs, const Trace_Event_Init& rhs) {
  return !(lhs == rhs);
}

bool operator==(const Trace_Event_LSP_Client_To_Server_Message& lhs,
                const Trace_Event_LSP_Client_To_Server_Message& rhs) {
  return lhs.body == rhs.body;
}

bool operator!=(const Trace_Event_LSP_Client_To_Server_Message& lhs,
                const Trace_Event_LSP_Client_To_Server_Message& rhs) {
  return !(lhs == rhs);
}

bool operator==(Trace_Vector_Max_Size_Histogram_Entry lhs,
                Trace_Vector_Max_Size_Histogram_Entry rhs) {
  return lhs.max_size == rhs.max_size && lhs.count == rhs.count;
}

bool operator!=(Trace_Vector_Max_Size_Histogram_Entry lhs,
                Trace_Vector_Max_Size_Histogram_Entry rhs) {
  return !(lhs == rhs);
}

bool operator==(const Trace_Vector_Max_Size_Histogram_By_Owner_Entry& lhs,
                const Trace_Vector_Max_Size_Histogram_By_Owner_Entry& rhs) {
  return lhs.owner == rhs.owner && lhs.max_size_entries == rhs.max_size_entries;
}

bool operator!=(const Trace_Vector_Max_Size_Histogram_By_Owner_Entry& lhs,
                const Trace_Vector_Max_Size_Histogram_By_Owner_Entry& rhs) {
  return !(lhs == rhs);
}

bool operator==(const Trace_Event_Vector_Max_Size_Histogram_By_Owner& lhs,
                const Trace_Event_Vector_Max_Size_Histogram_By_Owner& rhs) {
  return lhs.entries == rhs.entries;
}

bool operator!=(const Trace_Event_Vector_Max_Size_Histogram_By_Owner& lhs,
                const Trace_Event_Vector_Max_Size_Histogram_By_Owner& rhs) {
  return !(lhs == rhs);
}

bool operator==(const Trace_Event_Process_ID& lhs,
                const Trace_Event_Process_ID& rhs) {
  return lhs.process_id == rhs.process_id;
}

bool operator!=(const Trace_Event_Process_ID& lhs,
                const Trace_Event_Process_ID& rhs) {
  return !(lhs == rhs);
}

bool operator==(const Trace_LSP_Document_State& lhs,
                const Trace_LSP_Document_State& rhs) {
  return lhs.type == rhs.type && lhs.uri == rhs.uri && lhs.text == rhs.text &&
         lhs.language_id == rhs.language_id;
}

bool operator!=(const Trace_LSP_Document_State& lhs,
                const Trace_LSP_Document_State& rhs) {
  return !(lhs == rhs);
}

bool operator==(const Trace_Event_LSP_Documents& lhs,
                const Trace_Event_LSP_Documents& rhs) {
  return lhs.documents == rhs.documents;
}

bool operator!=(const Trace_Event_LSP_Documents& lhs,
                const Trace_Event_LSP_Documents& rhs) {
  return !(lhs == rhs);
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
