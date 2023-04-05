// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <filesystem>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/cli/text-diag-reporter.h>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/hash-set.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/diag/buffering-diag-reporter.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/file-canonical.h>
#include <quick-lint-js/io/file-path.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/port/memory-resource.h>
#include <string>
#include <system_error>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct test_typescript_options {
  std::vector<const char*> test_case_paths;
};

test_typescript_options parse_test_options(int argc, char** argv) {
  test_typescript_options o;

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      o.test_case_paths.push_back(argument);
    } else {
      const char* unrecognized = parser.match_anything();
      std::fprintf(stderr, "error: unrecognized option: %s\n", unrecognized);
      std::exit(2);
    }
  }

  return o;
}

class expected_test_results {
 public:
  struct expectations {
    bool has_errors;
  };

  result<void, platform_file_io_error> load_from_tests_directory(
      const char* baselines_path) {
    auto visit_entry = [&](const char* entry_name) -> void {
      static constexpr std::string_view errors_suffix = ".errors.txt"sv;

      std::string_view entry_name_view(entry_name);
      if (ends_with(entry_name_view, errors_suffix)) {
        entry_name_view.remove_suffix(errors_suffix.size());
        this->test_cases_expecting_error_.emplace(entry_name_view);
      }
    };
    result<void, platform_file_io_error> list = list_directory(
        concat(std::string_view(baselines_path), "/baselines/reference/"sv)
            .c_str(),
        visit_entry);
    if (!list.ok()) {
      return list.propagate();
    }
    return {};
  }

  std::optional<expectations> get_test_case_expectations(
      const char* path) const {
    std::string_view test_name = path_file_name(path);
    for (std::string_view suffix : {".ts"sv, ".tsx"sv}) {
      if (ends_with(test_name, suffix)) {
        test_name.remove_suffix(suffix.size());
        return expectations{
            // FIXME(strager): We shouldn't have to create an std::string here.
            .has_errors = this->test_cases_expecting_error_.contains(
                std::string(test_name)),
        };
      }
    }
    return std::nullopt;
  }

 private:
  hash_set<std::string> test_cases_expecting_error_;
};

void process_test_case_file(expected_test_results& expected_results,
                            const char* path) {
  std::fprintf(stderr, "note: checking %s\n", path);
  std::optional<expected_test_results::expectations> expected =
      expected_results.get_test_case_expectations(path);
  if (!expected.has_value()) {
    std::fprintf(stderr,
                 "fatal: failed to load expectations for test case %s\n", path);
    std::exit(1);
  }

  result<padded_string, read_file_io_error> source = read_file(path);
  if (!source.ok()) {
    std::fprintf(stderr, "fatal: failed to load file %s: %s\n", path,
                 source.error_to_string().c_str());
    std::exit(1);
  }
  padded_string_view source_view(&*source);

  buffering_diag_reporter diags(new_delete_resource());
  global_declared_variable_set globals;
  globals.add_literally_everything();
  linter_options options;
  options.jsx = false;
  options.typescript = true;

  parse_and_lint(source_view, diags, globals, options);

  bool did_error = !diags.empty();
  bool should_error = expected->has_errors;
  if (did_error != should_error) {
    if (did_error) {
      std::fprintf(stderr, "fail: test case errored but should not have: %s\n",
                   path);
      output_stream* out = file_output_stream::get_stderr();
      text_diag_reporter text_reporter(translator(), out,
                                       /*escape_errors=*/false);
      text_reporter.set_source(source_view, path);
      diags.move_into(&text_reporter);
      out->flush();
      std::exit(1);
    } else {
      // quick-lint-js probably failed to report a TypeScript type error. For
      // now, we ignore these false negatives.
      //
      // TODO(strager): Filter out type errors from TypeScript's .errors.txt and
      // compare.
    }
  }
}

void process_test_case_directory_or_file(
    expected_test_results& expected_results, const char* path) {
  auto visit_file = [&](const std::string& file_path) -> void {
    if (ends_with(file_path, ".ts"sv) || ends_with(file_path, ".tsx"sv)) {
      process_test_case_file(expected_results, file_path.c_str());
    }
  };
  auto on_error = [&](const platform_file_io_error& error, int depth) -> void {
    if (depth == 0 && error.is_not_a_directory_error()) {
      process_test_case_file(expected_results, path);
      return;
    }
    std::fprintf(stderr, "fatal: error while traversing directory: %s\n",
                 error.to_string().c_str());
    std::exit(1);
  };
  list_directory_recursively(path, visit_file, on_error);
}

std::string find_typescript_tests_directory(const char* descendant_path) {
  result<canonical_path_result, canonicalize_path_io_error> path_result =
      canonicalize_path(descendant_path);
  if (!path_result.ok()) {
    std::fprintf(stderr, "fatal: failed to canonicalize: %s\n",
                 path_result.error_to_string().c_str());
    std::exit(1);
  }
  canonical_path path = std::move(*path_result).canonical();
  for (;;) {
    if (path_file_name(path.path()) == "tests") {
      return std::move(path).path();
    }
    if (!path.parent()) {
      std::fprintf(stderr,
                   "fatal: failed to find TypeScript tests dir by searching "
                   "parents of path %s\n",
                   descendant_path);
    }
  }
}
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  test_typescript_options o = parse_test_options(argc, argv);
  if (o.test_case_paths.empty()) {
    std::fprintf(stderr, "error: missing path to TypeScript test case\n");
    return 2;
  }

  expected_test_results expected_results;
  result<void, platform_file_io_error> load =
      expected_results.load_from_tests_directory(
          find_typescript_tests_directory(o.test_case_paths[0]).c_str());
  if (!load.ok()) {
    std::fprintf(stderr, "fatal: failed to load baselines: %s\n",
                 load.error_to_string().c_str());
    std::exit(1);
  }

  for (const char* test_case_path : o.test_case_paths) {
    process_test_case_directory_or_file(expected_results, test_case_path);
  }

  return 0;
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
