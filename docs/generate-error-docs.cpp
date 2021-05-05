// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/error-documentation.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/parse.h>
#include <quick-lint-js/string-view.h>

#if QLJS_HAVE_DIRENT_H
#include <dirent.h>
#include <sys/types.h>
#endif

#if QLJS_HAVE_STD_FILESYSTEM
#include <filesystem>
#endif

namespace quick_lint_js {
namespace {
#if QLJS_HAVE_STD_FILESYSTEM
std::vector<std::string> list_directory(const char* directory_path) {
  std::vector<std::string> file_names;
  for (const std::filesystem::directory_entry& entry :
       std::filesystem::directory_iterator(directory_path)) {
    file_names.emplace_back(entry.path().filename().string());
  }
  return file_names;
}
#elif QLJS_HAVE_DIRENT_H
std::vector<std::string> list_directory(const char* directory_path) {
  ::DIR* d = ::opendir(directory_path);
  if (!d) {
    std::cerr << "fatal: failed to list files in directory '" << directory_path
              << "': " << std::strerror(errno) << '\n';
    std::exit(EXIT_FAILURE);
  }

  std::vector<std::string> file_names;
  for (;;) {
    errno = 0;
    ::dirent* entry = ::readdir(d);
    if (!entry) {
      if (errno != 0) {
        std::cerr << "fatal: failed to list files in directory '"
                  << directory_path << "': " << std::strerror(errno) << '\n';
        std::exit(EXIT_FAILURE);
      }
      break;
    }
    file_names.emplace_back(entry->d_name);
  }

  ::closedir(d);
  return file_names;
}
#else
#error "Unsupported platform"
#endif

std::vector<error_documentation> load_error_documentation_files(
    const char* root_path) {
  std::vector<error_documentation> out;
  for (const std::string& file_name : list_directory(root_path)) {
    if (ends_with(file_name, ".md")) {
      out.emplace_back(parse_error_documentation_file(std::string(root_path) +
                                                      "/" + file_name));
    }
  }
  return out;
}

void analyze_error_documentation(const error_documentation& doc,
                                 bool* found_problems) {
  if (doc.title_error_code != doc.file_path_error_code()) {
    std::cout << doc.file_path
              << ": error: file name doesn't match error code in title ("
              << doc.title_error_code << ")\n";
    *found_problems = true;
  }
  if (doc.code_blocks.empty()) {
    std::cout << doc.file_path << ": error: missing code blocks\n";
    *found_problems = true;
  }
  for (std::size_t i = 0; i < doc.code_blocks.size(); ++i) {
    error_collector errors;
    parser p(&doc.code_blocks[i], &errors);
    linter l(&errors);
    p.parse_and_visit_module(l);

    bool expect_error = i == 0;
    if (expect_error) {
      if (errors.errors.empty()) {
        std::cout << doc.file_path
                  << ": error: expected error in first code block but found no "
                     "errors\n";
        *found_problems = true;
      } else {
        for (const error_collector::error& e : errors.errors) {
          if (e.error_code() != doc.title_error_code) {
            std::cout << doc.file_path << ": error: expected only "
                      << doc.title_error_code
                      << " errors in first code block but found "
                      << e.error_code() << "\n";
            *found_problems = true;
          }
        }
      }
    } else {
      if (!errors.errors.empty()) {
        std::cout << doc.file_path
                  << ": error: expected no error in code block #" << (i + 1)
                  << " but found errors\n";
        *found_problems = true;
      }
    }
  }
}
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  if (argc != 2) {
    std::cerr << "error: expected exactly 1 arguments, but got " << (argc - 1)
              << '\n';
    std::cerr << "usage: " << argv[0] << " docs/errors/\n";
    return EXIT_FAILURE;
  }
  const char* documentation_directory_path = argv[1];

  std::vector<error_documentation> documents =
      load_error_documentation_files(documentation_directory_path);
  if (documents.empty()) {
    std::cerr << "error: found no .md files in " << documentation_directory_path
              << '\n';
    return EXIT_FAILURE;
  }

  bool found_problems = false;
  for (const error_documentation& doc : documents) {
    analyze_error_documentation(doc, &found_problems);
  }
  return found_problems ? EXIT_FAILURE : EXIT_SUCCESS;
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
