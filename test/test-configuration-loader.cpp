// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdio>
#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/configuration-loader.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/file-matcher.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/options.h>
#include <quick-lint-js/temporary-directory.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

#define EXPECT_DEFAULT_CONFIG(config)                                  \
  do {                                                                 \
    EXPECT_TRUE((config).globals().find(u8"Array"sv));                 \
    EXPECT_TRUE((config).globals().find(u8"console"sv));               \
    EXPECT_FALSE((config).globals().find(u8"variableDoesNotExist"sv)); \
  } while (false)

using ::testing::AnyOf;
using ::testing::HasSubstr;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
class test_configuration_loader : public ::testing::Test {
 public:
  std::string make_temporary_directory() {
    std::string temp_dir = quick_lint_js::make_temporary_directory();
    this->temporary_directories_.emplace_back(temp_dir);
    return temp_dir;
  }

  void set_current_working_directory(const std::string& path) {
    this->set_current_working_directory(path.c_str());
  }

  void set_current_working_directory(const char* path) {
    if (!this->old_working_directory_.has_value()) {
      this->old_working_directory_ = get_current_working_directory();
    }
    quick_lint_js::set_current_working_directory(path);
  }

 protected:
  void TearDown() override {
    if (this->old_working_directory_.has_value()) {
      set_current_working_directory(*this->old_working_directory_);
    }
    for (const std::string& temp_dir : this->temporary_directories_) {
      delete_directory_recursive(temp_dir);
    }
  }

 private:
  std::vector<std::string> temporary_directories_;
  std::optional<std::string> old_working_directory_;
};

TEST_F(test_configuration_loader,
       file_with_no_config_file_gets_default_config) {
  // NOTE(strager): This test assumes that there is no quick-lint-js.config file
  // in /tmp or in /.
  std::string temp_dir = this->make_temporary_directory();

  configuration_loader loader;
  std::string js_file = temp_dir + "/hello.js";
  write_file(js_file, u8""sv);
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });
  EXPECT_DEFAULT_CONFIG(*config);
}

TEST_F(test_configuration_loader, find_quick_lint_js_config_in_same_directory) {
  std::string temp_dir = this->make_temporary_directory();
  std::string config_file = temp_dir + "/quick-lint-js.config";
  write_file(config_file, u8R"({})"sv);

  std::string js_file = temp_dir + "/hello.js";
  write_file(js_file, u8""sv);
  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });

  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader,
       find_dot_quick_lint_js_config_in_same_directory) {
  std::string temp_dir = this->make_temporary_directory();
  std::string config_file = temp_dir + "/.quick-lint-js.config";
  write_file(config_file, u8"{}"sv);

  std::string js_file = temp_dir + "/hello.js";
  write_file(js_file, u8""sv);
  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });

  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader,
       quick_lint_js_config_is_used_over_dot_quick_lint_js_config) {
  std::string temp_dir = this->make_temporary_directory();
  write_file(temp_dir + "/quick-lint-js.config", u8"{}"sv);
  write_file(temp_dir + "/.quick-lint-js.config", u8"{}"sv);

  std::string js_file = temp_dir + "/hello.js";
  write_file(js_file, u8""sv);
  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });

  EXPECT_SAME_FILE(config->config_file_path(),
                   temp_dir + "/quick-lint-js.config");
}

TEST_F(test_configuration_loader,
       find_config_in_same_directory_of_relative_path) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);
  std::string config_file = "quick-lint-js.config";
  write_file(config_file, u8"{}"sv);

  std::string js_file = "hello.js";
  write_file(js_file, u8""sv);
  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });

  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader, quick_lint_js_config_directory_fails) {
  for (const char* config_file_name :
       {"quick-lint-js.config", ".quick-lint-js.config"}) {
    std::string temp_dir = this->make_temporary_directory();
    std::string config_file = temp_dir + "/" + config_file_name;
    create_directory(config_file);

    std::string js_file = temp_dir + "/hello.js";
    write_file(js_file, u8""sv);
    configuration_loader loader;
    configuration* config = loader.load_for_file(file_to_lint{
        .path = js_file.c_str(),
        .config_file = nullptr,
    });

    EXPECT_FALSE(config);
    EXPECT_THAT(loader.error(),
                HasSubstr(canonicalize_path(config_file).c_str()));
    EXPECT_THAT(
        loader.error(),
        AnyOf(HasSubstr("Is a directory"),
              HasSubstr(
                  "Access is denied")  // TODO(strager): Improve this message.
              ));
  }
}

TEST_F(test_configuration_loader, find_config_in_parent_directory) {
  for (const char* config_file_name :
       {"quick-lint-js.config", ".quick-lint-js.config"}) {
    SCOPED_TRACE(config_file_name);
    std::string temp_dir = this->make_temporary_directory();
    create_directory(temp_dir + "/dir");
    std::string config_file = temp_dir + "/" + config_file_name;
    write_file(config_file, u8"{}"sv);

    std::string js_file = temp_dir + "/dir/hello.js";
    write_file(js_file, u8""sv);
    configuration_loader loader;
    configuration* config = loader.load_for_file(file_to_lint{
        .path = js_file.c_str(),
        .config_file = nullptr,
    });

    EXPECT_SAME_FILE(config->config_file_path(), config_file);
  }
}

TEST_F(test_configuration_loader,
       find_config_in_parent_directory_of_relative_path) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);
  create_directory("dir");
  std::string config_file = "quick-lint-js.config";
  write_file(config_file, u8"{}"sv);

  std::string js_file = "dir/hello.js";
  write_file(js_file, u8""sv);
  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });

  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader, find_config_in_parent_directory_of_cwd) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  this->set_current_working_directory(temp_dir + "/dir");
  std::string config_file = "../quick-lint-js.config";
  write_file(config_file, u8"{}"sv);

  std::string js_file = "hello.js";
  write_file(js_file, u8""sv);
  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });

  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader, find_config_in_ancestor_directory) {
  for (const char* config_file_name :
       {"quick-lint-js.config", ".quick-lint-js.config"}) {
    SCOPED_TRACE(config_file_name);
    std::string temp_dir = this->make_temporary_directory();
    create_directory(temp_dir + "/a");
    create_directory(temp_dir + "/a/b");
    create_directory(temp_dir + "/a/b/c");
    create_directory(temp_dir + "/a/b/c/d");
    create_directory(temp_dir + "/a/b/c/d/e");
    create_directory(temp_dir + "/a/b/c/d/e/f");
    std::string config_file = temp_dir + "/" + config_file_name;
    write_file(config_file, u8"{}"sv);

    std::string js_file = temp_dir + "/a/b/c/d/e/f/hello.js";
    write_file(js_file, u8""sv);
    configuration_loader loader;
    configuration* config = loader.load_for_file(file_to_lint{
        .path = js_file.c_str(),
        .config_file = nullptr,
    });

    EXPECT_SAME_FILE(config->config_file_path(), config_file);
  }
}

TEST_F(test_configuration_loader,
       dot_dot_component_is_resolved_before_finding) {
  for (const char* config_file_name :
       {"quick-lint-js.config", ".quick-lint-js.config"}) {
    SCOPED_TRACE(config_file_name);
    std::string temp_dir = this->make_temporary_directory();
    create_directory(temp_dir + "/dir");
    create_directory(temp_dir + "/dir/subdir");
    std::string config_file_outside_dir = temp_dir + "/" + config_file_name;
    write_file(config_file_outside_dir, u8"{}"sv);
    std::string config_file_inside_subdir =
        temp_dir + "/dir/subdir/" + config_file_name;
    write_file(config_file_inside_subdir, u8"{}"sv);

    // Valid search path order:
    // * $temp_dir/dir/$config_file_name
    // * $temp_dir/$config_file_name
    //
    // Invalid search path order:
    // * $temp_dir/dir/$config_file_name
    //   (i.e. $temp_dir/dir/subdir/../$config_file_name)
    // * $temp_dir/dir/subdir/$config_file_name -- wrong; shouldn't be searched
    // * $temp_dir/dir/$config_file_name
    // * $temp_dir/$config_file_name

    std::string js_file = temp_dir + "/dir/subdir/../hello.js";
    write_file(js_file, u8""sv);
    configuration_loader loader;
    configuration* config = loader.load_for_file(file_to_lint{
        .path = js_file.c_str(),
        .config_file = nullptr,
    });

    EXPECT_SAME_FILE(config->config_file_path(), config_file_outside_dir);
  }
}

TEST_F(test_configuration_loader, find_config_in_cwd_if_stdin) {
  std::string temp_dir = this->make_temporary_directory();
  this->set_current_working_directory(temp_dir);
  std::string config_file = "quick-lint-js.config";
  write_file(config_file, u8"{}"sv);

  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = nullptr,
      .config_file = nullptr,
      .is_stdin = true,
  });

  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader, find_config_in_parent_of_cwd_if_stdin) {
  std::string temp_dir = this->make_temporary_directory();
  create_directory(temp_dir + "/dir");
  this->set_current_working_directory(temp_dir + "/dir");
  std::string config_file = "../quick-lint-js.config";
  write_file(config_file, u8"{}"sv);

  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = nullptr,
      .config_file = nullptr,
      .is_stdin = true,
  });

  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader, file_with_config_file_gets_loaded_config) {
  std::string temp_dir = this->make_temporary_directory();
  std::string config_file = temp_dir + "/config.json";
  write_file(config_file, u8R"({"globals": {"testGlobalVariable": true}})"sv);

  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = "hello.js",
      .config_file = config_file.c_str(),
  });

  EXPECT_TRUE(config->globals().find(u8"testGlobalVariable"sv));
  EXPECT_SAME_FILE(config->config_file_path(), config_file);
}

TEST_F(test_configuration_loader,
       files_with_same_config_file_get_same_loaded_config) {
  std::string temp_dir = this->make_temporary_directory();
  std::string config_file = temp_dir + "/config.json";
  write_file(config_file, u8R"({"globals": {"testGlobalVariable": true}})"sv);

  configuration_loader loader;
  configuration* config_one = loader.load_for_file(file_to_lint{
      .path = "one.js",
      .config_file = config_file.c_str(),
  });
  configuration* config_two = loader.load_for_file(file_to_lint{
      .path = "two.js",
      .config_file = config_file.c_str(),
  });

  EXPECT_EQ(config_one, config_two) << "pointers should be the same";
}

TEST_F(test_configuration_loader,
       files_with_different_config_files_get_different_loaded_config) {
  std::string temp_dir = this->make_temporary_directory();
  std::string config_file_one = temp_dir + "/config-one.json";
  write_file(config_file_one,
             u8R"({"globals": {"testGlobalVariableOne": true}})"sv);
  std::string config_file_two = temp_dir + "/config-two.json";
  write_file(config_file_two,
             u8R"({"globals": {"testGlobalVariableTwo": true}})"sv);

  configuration_loader loader;
  configuration* config_one = loader.load_for_file(file_to_lint{
      .path = "one.js",
      .config_file = config_file_one.c_str(),
  });
  configuration* config_two = loader.load_for_file(file_to_lint{
      .path = "two.js",
      .config_file = config_file_two.c_str(),
  });

  EXPECT_NE(config_one, config_two) << "pointers should be different";

  EXPECT_TRUE(config_one->globals().find(u8"testGlobalVariableOne"sv));
  EXPECT_FALSE(config_one->globals().find(u8"testGlobalVariableTwo"sv));
  EXPECT_SAME_FILE(config_one->config_file_path(), config_file_one);

  EXPECT_FALSE(config_two->globals().find(u8"testGlobalVariableOne"sv));
  EXPECT_TRUE(config_two->globals().find(u8"testGlobalVariableTwo"sv));
  EXPECT_SAME_FILE(config_two->config_file_path(), config_file_two);
}

TEST_F(test_configuration_loader, missing_config_file_fails) {
  std::string temp_dir = this->make_temporary_directory();
  std::string config_file = temp_dir + "/config.json";

  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = "hello.js",
      .config_file = config_file.c_str(),
  });

  EXPECT_FALSE(config);
  EXPECT_THAT(loader.error(), HasSubstr(config_file));
  EXPECT_THAT(loader.error(),
              AnyOf(HasSubstr("No such file"), HasSubstr("cannot find")));
}

TEST_F(test_configuration_loader,
       found_quick_lint_js_config_is_loaded_only_once) {
  for (const char* config_file_name :
       {"quick-lint-js.config", ".quick-lint-js.config"}) {
    std::string temp_dir = this->make_temporary_directory();
    std::string config_file = temp_dir + "/" + config_file_name;
    write_file(config_file, u8R"({"globals": {"testGlobalVariable": true}})"sv);

    configuration_loader loader;
    std::string js_file_one = temp_dir + "/one.js";
    write_file(js_file_one, u8""sv);
    configuration* config_one = loader.load_for_file(file_to_lint{
        .path = js_file_one.c_str(),
        .config_file = nullptr,
    });
    std::string js_file_two = temp_dir + "/two.js";
    write_file(js_file_two, u8""sv);
    configuration* config_two = loader.load_for_file(file_to_lint{
        .path = js_file_two.c_str(),
        .config_file = nullptr,
    });

    EXPECT_EQ(config_one, config_two) << "pointers should be the same";
  }
}

TEST_F(
    test_configuration_loader,
    found_quick_lint_js_config_and_explicit_config_file_is_loaded_only_once) {
  for (const char* config_file_name :
       {"quick-lint-js.config", ".quick-lint-js.config"}) {
    std::string temp_dir = this->make_temporary_directory();
    std::string config_file = temp_dir + "/" + config_file_name;
    write_file(config_file, u8R"({"globals": {"testGlobalVariable": true}})"sv);

    configuration_loader loader;
    std::string js_file_one = temp_dir + "/one.js";
    write_file(js_file_one, u8""sv);
    configuration* config_one = loader.load_for_file(file_to_lint{
        .path = js_file_one.c_str(),
        .config_file = nullptr,
    });
    std::string js_file_two = temp_dir + "/two.js";
    write_file(js_file_two, u8""sv);
    configuration* config_two = loader.load_for_file(file_to_lint{
        .path = js_file_two.c_str(),
        .config_file = config_file.c_str(),
    });

    EXPECT_EQ(config_one, config_two) << "pointers should be the same";
  }

  for (const char* config_file_name :
       {"quick-lint-js.config", ".quick-lint-js.config"}) {
    std::string temp_dir = this->make_temporary_directory();
    std::string config_file = temp_dir + "/" + config_file_name;
    write_file(config_file, u8R"({"globals": {"testGlobalVariable": true}})"sv);

    configuration_loader loader;
    std::string js_file_one = temp_dir + "/one.js";
    write_file(js_file_one, u8""sv);
    configuration* config_one = loader.load_for_file(file_to_lint{
        .path = js_file_one.c_str(),
        .config_file = config_file.c_str(),
    });
    std::string js_file_two = temp_dir + "/two.js";
    write_file(js_file_two, u8""sv);
    configuration* config_two = loader.load_for_file(file_to_lint{
        .path = js_file_two.c_str(),
        .config_file = nullptr,
    });

    EXPECT_EQ(config_one, config_two) << "pointers should be the same";
  }
}

TEST_F(
    test_configuration_loader,
    found_quick_lint_js_config_and_explicit_dot_quick_lint_js_config_are_loaded_separately) {
  {
    std::string temp_dir = this->make_temporary_directory();
    std::string config_file = temp_dir + "/quick-lint-js.config";
    write_file(config_file,
               u8R"({"globals": {"testGlobalVariableNoDot": true}})"sv);
    std::string dot_config_file = temp_dir + "/.quick-lint-js.config";
    write_file(dot_config_file,
               u8R"({"globals": {"testGlobalVariableDot": true}})"sv);

    configuration_loader loader;
    std::string js_file_one = temp_dir + "/one.js";
    write_file(js_file_one, u8""sv);
    configuration* config_one = loader.load_for_file(file_to_lint{
        .path = js_file_one.c_str(),
        .config_file = nullptr,
    });
    std::string js_file_two = temp_dir + "/two.js";
    write_file(js_file_two, u8""sv);
    configuration* config_two = loader.load_for_file(file_to_lint{
        .path = js_file_two.c_str(),
        .config_file = dot_config_file.c_str(),
    });

    EXPECT_NE(config_one, config_two) << "pointers should be different";
    EXPECT_TRUE(config_one->globals().find(u8"testGlobalVariableNoDot"sv));
    EXPECT_FALSE(config_one->globals().find(u8"testGlobalVariableDot"sv));
    EXPECT_FALSE(config_two->globals().find(u8"testGlobalVariableNoDot"sv));
    EXPECT_TRUE(config_two->globals().find(u8"testGlobalVariableDot"sv));
  }

  {
    std::string temp_dir = this->make_temporary_directory();
    std::string config_file = temp_dir + "/quick-lint-js.config";
    write_file(config_file,
               u8R"({"globals": {"testGlobalVariableNoDot": true}})"sv);
    std::string dot_config_file = temp_dir + "/.quick-lint-js.config";
    write_file(dot_config_file,
               u8R"({"globals": {"testGlobalVariableDot": true}})"sv);

    configuration_loader loader;
    std::string js_file_one = temp_dir + "/one.js";
    write_file(js_file_one, u8""sv);
    configuration* config_one = loader.load_for_file(file_to_lint{
        .path = js_file_one.c_str(),
        .config_file = dot_config_file.c_str(),
    });
    std::string js_file_two = temp_dir + "/two.js";
    write_file(js_file_two, u8""sv);
    configuration* config_two = loader.load_for_file(file_to_lint{
        .path = js_file_two.c_str(),
        .config_file = nullptr,
    });

    EXPECT_NE(config_one, config_two) << "pointers should be different";
    EXPECT_FALSE(config_one->globals().find(u8"testGlobalVariableNoDot"sv));
    EXPECT_TRUE(config_one->globals().find(u8"testGlobalVariableDot"sv));
    EXPECT_TRUE(config_two->globals().find(u8"testGlobalVariableNoDot"sv));
    EXPECT_FALSE(config_two->globals().find(u8"testGlobalVariableDot"sv));
  }
}

TEST_F(test_configuration_loader, finding_config_fails_if_file_is_missing) {
  std::string temp_dir = this->make_temporary_directory();
  std::string config_file = temp_dir + "/quick-lint-js.config";
  write_file(config_file, u8R"({})"sv);

  std::string js_file = temp_dir + "/hello.js";
  configuration_loader loader;
  configuration* config = loader.load_for_file(file_to_lint{
      .path = js_file.c_str(),
      .config_file = nullptr,
  });

  EXPECT_FALSE(config);
  EXPECT_THAT(loader.error(), HasSubstr(js_file));
  EXPECT_THAT(loader.error(),
              AnyOf(HasSubstr("No such file"), HasSubstr("cannot find")));
}
}
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
