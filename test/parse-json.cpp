// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <gtest/gtest.h>
#include <json/reader.h>
#include <json/value.h>
#include <quick-lint-js/parse-json.h>
#include <sstream>

namespace quick_lint_js {
::Json::Value parse_json(std::stringstream &stream) {
  SCOPED_TRACE(stream.str());
  stream.seekg(0);
  ::Json::Value root;
  ::Json::CharReaderBuilder builder;
  builder.strictMode(&builder.settings_);
  ::Json::String errors;
  bool ok = ::Json::parseFromStream(builder, stream, &root, &errors);
  EXPECT_TRUE(ok) << errors;
  return root;
}
}
