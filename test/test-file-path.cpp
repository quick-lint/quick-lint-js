// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/util/utf-16.h>

#if QLJS_HAVE_UNISTD_H
#include <unistd.h>
#endif

using ::testing::AnyOf;

namespace quick_lint_js {
namespace {
class Test_File_Path : public ::testing::Test, public Filesystem_Test {
 public:
  Monotonic_Allocator allocator_{"test"};
};

#if defined(QLJS_HAVE_UNISTD_H) && defined(_POSIX_VERSION) && \
    _POSIX_VERSION >= 200112L
TEST_F(Test_File_Path, parent_path_posix) {
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

TEST_F(Test_File_Path, path_file_name_posix) {
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
TEST_F(Test_File_Path, parent_path_windows) {
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

TEST_F(Test_File_Path, path_file_name_windows) {
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

#if defined(_WIN32)
struct Simplified_Path_Assertion {
  const wchar_t* full_path;
  const wchar_t* root;
  const wchar_t* relative;

  friend bool operator==(Simplified_Path lhs, Simplified_Path_Assertion rhs) {
    if (rhs.full_path != nullptr &&
        std::wstring_view(lhs.full_path) != std::wstring_view(rhs.full_path)) {
      return false;
    }
    if (rhs.root != nullptr && lhs.root != rhs.root) {
      return false;
    }
    if (rhs.relative != nullptr && lhs.relative != rhs.relative) {
      return false;
    }
    return true;
  }

  friend std::ostream& operator<<(std::ostream& out,
                                  Simplified_Path_Assertion assertion) {
    auto maybe_write_field = [&](const char* name, const wchar_t* s) -> void {
      if (s != nullptr) {
        out << "  ." << name << " = \"" << wstring_to_mbstring(s).value()
            << "\",\n";
      }
    };
    out << "Simplified_Path{\n";
    maybe_write_field("full_path", assertion.full_path);
    maybe_write_field("root", assertion.root);
    maybe_write_field("relative", assertion.relative);
    out << "}";
    return out;
  }
};
#endif

#if defined(_WIN32)
#define CHECK_SIMPLIFY(input_path, expected_full_path, expected_root,       \
                       expected_relative)                                   \
  EXPECT_EQ(simplify_path_and_make_absolute(&this->allocator_, input_path), \
            (Simplified_Path_Assertion{                                     \
                .full_path = expected_full_path,                            \
                .root = expected_root,                                      \
                .relative = expected_relative,                              \
            }))

TEST_F(Test_File_Path, simplify_path_and_make_absolute_empty_path) {
  CHECK_SIMPLIFY(L"", L"", L"", L"");
}

TEST_F(Test_File_Path, simplify_path_and_make_absolute_already_absolute) {
  // clang-format off
  // Path splitting with no modifications:
  CHECK_SIMPLIFY(LR"(C:\)",     LR"(C:\)",     L"C:", L"");
  CHECK_SIMPLIFY(LR"(C:\a\b)",  LR"(C:\a\b)",  L"C:", LR"(a\b)");
  CHECK_SIMPLIFY(LR"(C:\a\b\)", LR"(C:\a\b\)", L"C:", LR"(a\b\)");

  CHECK_SIMPLIFY(LR"(\\?\C:\)",     LR"(\\?\C:\)",     LR"(\\?\C:)", L"");
  CHECK_SIMPLIFY(LR"(\\?\C:\a\b)",  LR"(\\?\C:\a\b)",  LR"(\\?\C:)", LR"(a\b)");
  CHECK_SIMPLIFY(LR"(\\?\C:\a\b\)", LR"(\\?\C:\a\b\)", LR"(\\?\C:)", LR"(a\b\)");

  CHECK_SIMPLIFY(LR"(\\server\share)",          LR"(\\server\share)",          LR"(\\server\share)", L"");
  CHECK_SIMPLIFY(LR"(\\server\share\)",         LR"(\\server\share\)",         LR"(\\server\share)", L"");
  CHECK_SIMPLIFY(LR"(\\server\share\file.txt)", LR"(\\server\share\file.txt)", LR"(\\server\share)", L"file.txt");

  // TODO(strager): Handle \\?\UNC\ paths correctly.
  // CHECK_SIMPLIFY(LR"(\\?\UNC\server\share)",          LR"(\\?\UNC\server\share)",          LR"(\\?\UNC\server\share)", L"");
  // CHECK_SIMPLIFY(LR"(\\?\UNC\server\share\)",         LR"(\\?\UNC\server\share\)",         LR"(\\?\UNC\server\share)", L"");
  // CHECK_SIMPLIFY(LR"(\\?\UNC\server\share\file.txt)", LR"(\\?\UNC\server\share\file.txt)", LR"(\\?\UNC\server\share)", L"file.txt");

  CHECK_SIMPLIFY(LR"(\\.\device)",     LR"(\\.\device)",     LR"(\\.\device)", L"");
  CHECK_SIMPLIFY(LR"(\\.\device\)",    LR"(\\.\device\)",    LR"(\\.\device)", L"");

  // '.' components are dropped:
  CHECK_SIMPLIFY(LR"(C:\a\.\b)",  LR"(C:\a\b)", L"C:", LR"(a\b)");
  CHECK_SIMPLIFY(LR"(C:\a\b\.)",  LR"(C:\a\b)", L"C:", LR"(a\b)");
  CHECK_SIMPLIFY(LR"(C:\a\b\.\)", LR"(C:\a\b\)", L"C:", LR"(a\b\)");

  CHECK_SIMPLIFY(LR"(\\?/C:\a\.\b)", LR"(\\?\C:\a\b)", LR"(\\?\C:)", LR"(a\b)");

  // '..' components are resolved:
  CHECK_SIMPLIFY(LR"(C:\a\..\b)",    LR"(C:\b)",  L"C:", L"b");
  CHECK_SIMPLIFY(LR"(C:\a\b\..)",    LR"(C:\a)",  L"C:", L"a");
  CHECK_SIMPLIFY(LR"(C:\a\b\..\)",   LR"(C:\a\)", L"C:", LR"(a\)");
  CHECK_SIMPLIFY(LR"(C:\a\b\..\..)", LR"(C:\)",   L"C:", L"");

  CHECK_SIMPLIFY(LR"(\\?/C:\a\..\b)", LR"(\\?\C:\b)", LR"(\\?\C:)", L"b");

  // Redundant '\'s are merged:
  CHECK_SIMPLIFY(LR"(C:\\a\\b\\)",  LR"(C:\a\b\)", L"C:", LR"(a\b\)");
  CHECK_SIMPLIFY(LR"(C:\a\\\\\\b)", LR"(C:\a\b)",  L"C:", LR"(a\b)");

  CHECK_SIMPLIFY(LR"(\\server\\share)", LR"(\\server\share)", LR"(\\server\share)", L"");

  CHECK_SIMPLIFY(LR"(\\?/C:\\a\\b)", LR"(\\?\C:\a\b)", LR"(\\?\C:)", LR"(a\b)");

  // '/' is converted to '\':
  CHECK_SIMPLIFY(LR"(C:/)",     LR"(C:\)",     L"C:", L"");
  CHECK_SIMPLIFY(LR"(C:/a/b)",  LR"(C:\a\b)",  L"C:", LR"(a\b)");
  CHECK_SIMPLIFY(LR"(C:/a/b/)", LR"(C:\a\b\)", L"C:", LR"(a\b\)");

  CHECK_SIMPLIFY(LR"(\\server\share)",     LR"(\\server\share)",     LR"(\\server\share)", L"");
  CHECK_SIMPLIFY(LR"(\\server\share/)",    LR"(\\server\share\)",    LR"(\\server\share)", L"");
  CHECK_SIMPLIFY(LR"(\\server\share/a/b)", LR"(\\server\share\a\b)", LR"(\\server\share)", LR"(a\b)");

  CHECK_SIMPLIFY(LR"(\\?/C:/a/b)", LR"(\\?\C:\a\b)", LR"(\\?\C:)", LR"(a\b)");

  // '.' is not resolved for \\?\ paths:
  CHECK_SIMPLIFY(LR"(\\?\C:\a\.\b)", LR"(\\?\C:\a\.\b)", LR"(\\?\C:)", LR"(a\.\b)");

  // '..' is not resolved for \\?\ paths:
  CHECK_SIMPLIFY(LR"(\\?\C:\a\b\..)", LR"(\\?\C:\a\b\..)", LR"(\\?\C:)", LR"(a\b\..)");

  // '/' is not converted to '\' for \\?\ paths:
  CHECK_SIMPLIFY(LR"(\\?\C:/a/b\c/d/)", LR"(\\?\C:/a/b\c/d/)", LR"(\\?\C:/a/b)", LR"(c/d/)");

  // clang-format on
}

struct CWD_Parts {
  // Example:
  // cwd:      L"C:\\projects\\dir"
  // root:     L"C:"
  // relative: L"projects\\dir"
  std::wstring cwd;
  std::wstring root;
  std::wstring relative;
};

CWD_Parts get_cwd_parts() {
  std::wstring cwd;
  if (!get_current_working_directory(cwd).ok()) {
    QLJS_UNIMPLEMENTED();
  }
  SCOPED_TRACE(cwd);

  // Assumption: cwd has the form 'C:\' or 'C:\dir\subdir'.
  EXPECT_GE(cwd.size(), 3);
  EXPECT_EQ(cwd[1], L':');
  EXPECT_EQ(cwd[2], L'\\');
  if (cwd.size() > 3) {
    EXPECT_NE(cwd.back(), L'\\');
  }

  return CWD_Parts{
      .cwd = cwd,
      .root = cwd.substr(0, 2),   // "C:"
      .relative = cwd.substr(3),  // Skip "C:\".
  };
}

TEST_F(Test_File_Path, simplify_path_and_make_absolute_cwd_relative) {
  CWD_Parts cwd = get_cwd_parts();

  // clang-format off
  // Basic path is appended to cwd:
  CHECK_SIMPLIFY(L"hello.txt",    (cwd.cwd + LR"(\hello.txt)").c_str(), cwd.root.c_str(), (cwd.relative + LR"(\hello.txt)").c_str());
  CHECK_SIMPLIFY(LR"(dir\file)",  (cwd.cwd + LR"(\dir\file)").c_str(),  cwd.root.c_str(), (cwd.relative + LR"(\dir\file)").c_str());
  CHECK_SIMPLIFY(LR"(dir\file\)", (cwd.cwd + LR"(\dir\file\)").c_str(), cwd.root.c_str(), (cwd.relative + LR"(\dir\file\)").c_str());

  // '.' components are dropped:
  CHECK_SIMPLIFY(L".",        cwd.cwd.c_str(),                cwd.root.c_str(), cwd.relative.c_str());
  CHECK_SIMPLIFY(LR"(.\)",    (cwd.cwd + LR"(\)").c_str(),    cwd.root.c_str(), (cwd.relative + LR"(\)").c_str());
  CHECK_SIMPLIFY(LR"(a\.\b)", (cwd.cwd + LR"(\a\b)").c_str(), cwd.root.c_str(), (cwd.relative + LR"(\a\b)").c_str());

  // '..' components are resolved:
  CHECK_SIMPLIFY(LR"(a\..)",  cwd.cwd.c_str(),             cwd.root.c_str(), cwd.relative.c_str());
  CHECK_SIMPLIFY(LR"(a\..\)", (cwd.cwd + LR"(\)").c_str(), cwd.root.c_str(), (cwd.relative + LR"(\)").c_str());
  // clang-format on
}

TEST_F(Test_File_Path,
       simplify_path_and_make_absolute_path_starting_with_dot_dot) {
  {
    this->set_current_working_directory(this->make_temporary_directory());
    CWD_Parts cwd = get_cwd_parts();
    ASSERT_NE(cwd.relative, L"")
        << "temporary directory shouldn't be a root path";
    std::size_t relative_last_slash_index = cwd.relative.rfind(L'\\');
    std::wstring parent_relative =
        relative_last_slash_index == cwd.relative.npos
            ? L""
            : cwd.relative.substr(0, relative_last_slash_index);
    std::wstring expected_full_path = cwd.root + L'\\' + parent_relative;
    CHECK_SIMPLIFY(L"..",
                   /*full_path=*/expected_full_path.c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/parent_relative.c_str());
    CHECK_SIMPLIFY(LR"(..\.)",
                   /*full_path=*/expected_full_path.c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/parent_relative.c_str());
    CHECK_SIMPLIFY(LR"(.\..)",
                   /*full_path=*/expected_full_path.c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/parent_relative.c_str());
  }

  {
    // Change the current directory to e.g. "C:\".
    this->set_current_working_directory(
        wstring_to_mbstring(get_cwd_parts().root + L"\\").value());
    CWD_Parts cwd = get_cwd_parts();
    ASSERT_EQ(cwd.relative, L"") << "current directory should be root path";
    std::wstring expected_full_path = cwd.root + L'\\';
    CHECK_SIMPLIFY(L"..",
                   /*full_path=*/expected_full_path.c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/L"");
    CHECK_SIMPLIFY(LR"(..\..)",
                   /*full_path=*/expected_full_path.c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/L"");
  }
}

TEST_F(Test_File_Path,
       simplify_path_and_make_absolute_drive_cwd_relative_path) {
  {
    CWD_Parts cwd = get_cwd_parts();
    // L"C:dir" for example.
    CHECK_SIMPLIFY((cwd.root + L"dir").c_str(),
                   /*full_path=*/(cwd.cwd + LR"(\dir)").c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/(cwd.relative + LR"(\dir)").c_str());
    // L"C:dir\\subdir" for example.
    CHECK_SIMPLIFY((cwd.root + LR"(dir\subdir)").c_str(),
                   /*full_path=*/(cwd.cwd + LR"(\dir\subdir)").c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/(cwd.relative + LR"(\dir\subdir)").c_str());
  }
}

// TODO(strager): Test mixing drives (e.g. cwd is on C: but path is "D:foo").

TEST_F(
    Test_File_Path,
    simplify_path_and_make_absolute_drive_cwd_relative_path_starting_with_dot_dot) {
  {
    this->set_current_working_directory(this->make_temporary_directory());
    CWD_Parts cwd = get_cwd_parts();
    ASSERT_NE(cwd.relative, L"")
        << "temporary directory shouldn't be a root path";
    std::size_t relative_last_slash_index = cwd.relative.rfind(L'\\');
    std::wstring parent_relative =
        relative_last_slash_index == cwd.relative.npos
            ? L""
            : cwd.relative.substr(0, relative_last_slash_index);
    std::wstring expected_full_path = cwd.root + L'\\' + parent_relative;
    // L"C:.." for example.
    CHECK_SIMPLIFY((cwd.root + L"..").c_str(),
                   /*full_path=*/expected_full_path.c_str(),
                   /*root=*/cwd.root.c_str(),
                   /*relative=*/parent_relative.c_str());
  }
}

TEST_F(Test_File_Path,
       simplify_path_and_make_absolute_current_drive_relative_path) {
  CWD_Parts cwd = get_cwd_parts();
  CHECK_SIMPLIFY(LR"(\somedir)",
                 /*full_path=*/(cwd.root + LR"(\somedir)").c_str(),
                 /*root=*/cwd.root.c_str(),
                 /*relative=*/L"somedir");
  CHECK_SIMPLIFY(L"/somedir",
                 /*full_path=*/(cwd.root + LR"(\somedir)").c_str(),
                 /*root=*/cwd.root.c_str(),
                 /*relative=*/L"somedir");
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
