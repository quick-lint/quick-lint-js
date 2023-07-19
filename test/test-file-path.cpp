// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/port/have.h>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

using ::testing::AnyOf;

namespace quick_lint_js {
namespace {
#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
TEST(Test_File_Path, parent_path_posix) {
  EXPECT_EQ(parent_path("x/y"), "x");
  EXPECT_EQ(parent_path("x/y/z"), "x/y");

  EXPECT_EQ(parent_path("x/y/"), "x") << "trailing / should be ignored";
  EXPECT_EQ(parent_path("x/y///"), "x") << "trailing slashes should be ignored";

  EXPECT_EQ(parent_path("x"), ".");
  EXPECT_EQ(parent_path("x/"), ".");

  EXPECT_EQ(parent_path("/x"), "/");
  EXPECT_EQ(parent_path("/x/y"), "/x");

  EXPECT_EQ(parent_path("/x/"), "/");
  EXPECT_EQ(parent_path("/x/y/"), "/x");

  EXPECT_EQ(parent_path("/"), "/");

  EXPECT_EQ(parent_path("///"), "/") << "/// should flatten to /";
  EXPECT_EQ(parent_path("////"), "/");

  EXPECT_THAT(parent_path("//x"), AnyOf("/", "//"))
      << "// is implementation-defined";
  EXPECT_THAT(parent_path("//x/"), AnyOf("/", "//"))
      << "// is implementation-defined";
  EXPECT_THAT(parent_path("//x/y"), AnyOf("/x", "//x"))
      << "// is implementation-defined";
  EXPECT_THAT(parent_path("//"), AnyOf("/", "//"))
      << "// is implementation-defined";
}

TEST(Test_File_Path, path_file_name_posix) {
  EXPECT_EQ(path_file_name(""), "");
  EXPECT_EQ(path_file_name("x"), "x");

  EXPECT_EQ(path_file_name("x/y"), "y");
  EXPECT_EQ(path_file_name("x/y/z"), "z");

  EXPECT_EQ(path_file_name("x/"), "x") << "trailing / should be ignored";
  EXPECT_EQ(path_file_name("x/y/"), "y") << "trailing / should be ignored";
  EXPECT_EQ(path_file_name("x/y///"), "y")
      << "trailing slashes should be ignored";

  EXPECT_EQ(path_file_name("/x"), "x");
  EXPECT_EQ(path_file_name("/x/y"), "y");

  EXPECT_EQ(path_file_name("/x/"), "x") << "trailing / should be ignored";
  EXPECT_EQ(path_file_name("/x/y/"), "y") << "trailing / should be ignored";

  EXPECT_EQ(path_file_name("/"), "");

  EXPECT_EQ(path_file_name("///"), "");
  EXPECT_EQ(path_file_name("////"), "");

  EXPECT_EQ(path_file_name("//x"), "x") << "// is implementation-defined";
  EXPECT_THAT(path_file_name("//x/"), AnyOf("x", ""))
      << "// is implementation-defined";
  EXPECT_EQ(path_file_name("//x/y"), "y") << "// is implementation-defined";
  EXPECT_EQ(path_file_name("//"), "") << "// is implementation-defined";
}
#endif

#if defined(_WIN32)
TEST(Test_File_Path, parent_path_windows) {
  EXPECT_EQ(parent_path(R"(x/y)"), R"(x)");
  EXPECT_EQ(parent_path(R"(x/y/z)"), R"(x/y)");
  EXPECT_EQ(parent_path(R"(x\y)"), R"(x)");
  EXPECT_EQ(parent_path(R"(x\y\z)"), R"(x\y)");

  EXPECT_EQ(parent_path(R"(x\y/z)"), R"(x\y)");
  EXPECT_EQ(parent_path(R"(x/y\z)"), R"(x/y)");

  EXPECT_EQ(parent_path(R"(x/y/)"), R"(x)") << "trailing / should be ignored";
  EXPECT_EQ(parent_path(R"(x/y///)"), R"(x)")
      << "trailing slashes should be ignored";
  EXPECT_EQ(parent_path(R"(x\y\)"), R"(x)") << "trailing \\ should be ignored";
  EXPECT_EQ(parent_path(R"(x\y\\\)"), R"(x)")
      << "trailing slashes should be ignored";

  EXPECT_EQ(parent_path(R"(x)"), R"(.)");
  EXPECT_EQ(parent_path(R"(x/)"), R"(.)");
  EXPECT_EQ(parent_path(R"(x\)"), R"(.)");

  EXPECT_EQ(parent_path(R"(/x)"), R"(/)");
  EXPECT_EQ(parent_path(R"(/x/y)"), R"(/x)");
  EXPECT_EQ(parent_path(R"(\x)"), R"(\)");
  EXPECT_EQ(parent_path(R"(\x\y)"), R"(\x)");

  EXPECT_EQ(parent_path(R"(/x/)"), R"(/)");
  EXPECT_EQ(parent_path(R"(/x/y/)"), R"(/x)");
  EXPECT_EQ(parent_path(R"(\x\)"), R"(\)");
  EXPECT_EQ(parent_path(R"(\x\y\)"), R"(\x)");

  EXPECT_EQ(parent_path(R"(//x)"), R"(//x)")
      << "parent of share host is itself";
  EXPECT_EQ(parent_path(R"(//x/)"), R"(//x)")
      << "parent of share host is itself";
  EXPECT_EQ(parent_path(R"(\\x)"), R"(\\x)")
      << "parent of share host is itself";
  EXPECT_EQ(parent_path(R"(\\x\)"), R"(\\x)")
      << "parent of share host is itself";

  EXPECT_EQ(parent_path(R"(\\host\share)"), R"(\\host\share)")
      << "share is a root";
  EXPECT_EQ(parent_path(R"(\\host\share\)"), R"(\\host\share)")
      << "share is a root";

  EXPECT_EQ(parent_path(R"(\\host\share\dir)"), R"(\\host\share)");

  EXPECT_EQ(parent_path(R"(/)"), R"(/)");
  EXPECT_EQ(parent_path(R"(//)"), R"(//)") << "// is a root";
  EXPECT_EQ(parent_path(R"(///)"), R"(//)") << "// is a root";
  EXPECT_EQ(parent_path(R"(/////)"), R"(//)") << "// is a root";
  EXPECT_EQ(parent_path(R"(\)"), R"(\)");
  EXPECT_EQ(parent_path(R"(\\)"), R"(\\)") << R"(\\ is a root)";
  EXPECT_EQ(parent_path(R"(\\\)"), R"(\\)") << R"(\\ is a root)";
  EXPECT_EQ(parent_path(R"(\\\\\)"), R"(\\)") << R"(\\ is a root)";

  EXPECT_EQ(parent_path(R"(C:\)"), R"(C:\)");
  EXPECT_EQ(parent_path(R"(C:/)"), R"(C:/)");

  EXPECT_EQ(parent_path(R"(\\?\C:\)"), R"(\\?\C:\)");

  EXPECT_EQ(parent_path(R"(C:)"), R"(C:)");
  EXPECT_EQ(parent_path(R"(z:)"), R"(z:)");

  EXPECT_EQ(parent_path(R"(C:\x)"), R"(C:\)");
  EXPECT_EQ(parent_path(R"(C:\x\)"), R"(C:\)");

  EXPECT_EQ(parent_path(R"(C:x)"), R"(C:)");
  EXPECT_EQ(parent_path(R"(C:x\)"), R"(C:)");
  EXPECT_EQ(parent_path(R"(C:x\y)"), R"(C:x)");

  EXPECT_EQ(parent_path(R"(\\?\C:\x)"), R"(\\?\C:\)");
  EXPECT_EQ(parent_path(R"(\\?\C:\x\)"), R"(\\?\C:\)");

  EXPECT_EQ(parent_path(R"(\\?\)"), R"(\\?\)") << "invalid path remains as-is";
  EXPECT_EQ(parent_path(R"(\\?)"), R"(\\?)") << "invalid path remains as-is";

  // TODO(strager): Test \\.\ paths.
  // TODO(strager): Test \??\ paths.
  // TODO(strager): Test \\?\UNC\host\share paths.
}

TEST(Test_File_Path, path_file_name_windows) {
  EXPECT_EQ(path_file_name(""), "");
  EXPECT_EQ(path_file_name(R"(x)"), "x");

  EXPECT_EQ(path_file_name(R"(x/y)"), "y");
  EXPECT_EQ(path_file_name(R"(x/y/z)"), "z");
  EXPECT_EQ(path_file_name(R"(x\y)"), "y");
  EXPECT_EQ(path_file_name(R"(x\y\z)"), "z");

  EXPECT_EQ(path_file_name(R"(x\y/z)"), "z");
  EXPECT_EQ(path_file_name(R"(x/y\z)"), "z");

  EXPECT_EQ(path_file_name(R"(x/y/)"), "y") << "trailing / should be ignored";
  EXPECT_EQ(path_file_name(R"(x/y///)"), "y")
      << "trailing slashes should be ignored";
  EXPECT_EQ(path_file_name(R"(x\y\)"), "y") << "trailing \\ should be ignored";
  EXPECT_EQ(path_file_name(R"(x\y\\\)"), "y")
      << "trailing slashes should be ignored";

  EXPECT_EQ(path_file_name(R"(x/)"), "x") << "trailing / should be ignored";
  EXPECT_EQ(path_file_name(R"(x\)"), "x") << "trailing \\ should be ignored";

  EXPECT_EQ(path_file_name(R"(/x)"), "x");
  EXPECT_EQ(path_file_name(R"(/x/y)"), "y");
  EXPECT_EQ(path_file_name(R"(\x)"), "x");
  EXPECT_EQ(path_file_name(R"(\x\y)"), "y");

  EXPECT_EQ(path_file_name(R"(/x/)"), "x") << "trailing / should be ignored";
  EXPECT_EQ(path_file_name(R"(/x/y/)"), "y") << "trailing / should be ignored";
  EXPECT_EQ(path_file_name(R"(\x\)"), "x") << "trailing \\ should be ignored";
  EXPECT_EQ(path_file_name(R"(\x\y\)"), "y") << "trailing \\ should be ignored";

  EXPECT_EQ(path_file_name(R"(//x)"), "") << "share host does not have a file";
  EXPECT_EQ(path_file_name(R"(//x/)"), "") << "share host does not have a file";
  EXPECT_EQ(path_file_name(R"(\\x)"), "") << "share host does not have a file";
  EXPECT_EQ(path_file_name(R"(\\x\)"), "") << "share host does not have a file";

  EXPECT_EQ(path_file_name(R"(\\host\share)"), "")
      << "share does not have a file";
  EXPECT_EQ(path_file_name(R"(\\host\share\)"), "")
      << "share does not have a file";

  EXPECT_EQ(path_file_name(R"(\\host\share\dir)"), "dir");

  EXPECT_EQ(path_file_name(R"(/)"), "");
  EXPECT_EQ(path_file_name(R"(//)"), "") << "// is a root";
  EXPECT_EQ(path_file_name(R"(///)"), "") << "// is a root";
  EXPECT_EQ(path_file_name(R"(/////)"), "") << "// is a root";
  EXPECT_EQ(path_file_name(R"(\)"), "");
  EXPECT_EQ(path_file_name(R"(\\)"), "") << R"(\\ is a root)";
  EXPECT_EQ(path_file_name(R"(\\\)"), "") << R"(\\ is a root)";
  EXPECT_EQ(path_file_name(R"(\\\\\)"), "") << R"(\\ is a root)";

  EXPECT_EQ(path_file_name(R"(C:\)"), "");
  EXPECT_EQ(path_file_name(R"(C:/)"), "");

  EXPECT_EQ(path_file_name(R"(\\?\C:\)"), "");

  EXPECT_EQ(path_file_name(R"(C:)"), "");
  EXPECT_EQ(path_file_name(R"(z:)"), "");

  EXPECT_EQ(path_file_name(R"(C:\x)"), "x");
  EXPECT_EQ(path_file_name(R"(C:\x\)"), "x");

  EXPECT_EQ(path_file_name(R"(C:x)"), "x");
  EXPECT_EQ(path_file_name(R"(C:x\)"), "x");
  EXPECT_EQ(path_file_name(R"(C:x\y)"), "y");

  EXPECT_EQ(path_file_name(R"(\\?\C:\x)"), "x");
  EXPECT_EQ(path_file_name(R"(\\?\C:\x\)"), "x");

  EXPECT_EQ(path_file_name(R"(\\?\)"), "") << "invalid path";
  EXPECT_EQ(path_file_name(R"(\\?)"), "") << "invalid path";

  // TODO(strager): Test \\.\ paths.
  // TODO(strager): Test \??\ paths.
  // TODO(strager): Test \\?\UNC\host\share paths.
}
#endif
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
