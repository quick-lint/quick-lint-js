// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <md4c.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-documentation.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/string-view.h>
#include <string>
#include <utility>
#include <vector>

namespace quick_lint_js {
std::string_view error_documentation::file_path_error_code() const {
  std::string_view code = remove_suffix_if_present(this->file_path, ".md");
  std::size_t last_directory_separator = code.find_last_of("/\\");
  if (last_directory_separator != std::string::npos) {
    code = code.substr(last_directory_separator + 1);
  }
  return code;
}

error_documentation parse_error_documentation(std::string&& file_path,
                                              string8_view markdown) {
  struct data {
    error_documentation doc;
    string8 current_block;
    bool in_code_block = false;
    bool in_title = false;
  };

  ::MD_PARSER parser = {
      .abi_version = 0,
      .flags = 0,

      .enter_block = [](::MD_BLOCKTYPE type, void* detail,
                        void* userdata) -> int {
        data* d = reinterpret_cast<data*>(userdata);
        switch (type) {
        case ::MD_BLOCK_CODE:
          d->in_code_block = true;
          d->current_block.clear();
          break;

        case ::MD_BLOCK_H: {
          ::MD_BLOCK_H_DETAIL* block_detail =
              reinterpret_cast<::MD_BLOCK_H_DETAIL*>(detail);
          if (block_detail->level == 1) {
            d->in_title = true;
            d->current_block.clear();
          }
          break;
        }

        default:
          break;
        }
        return 0;
      },

      .leave_block = [](::MD_BLOCKTYPE type, [[maybe_unused]] void* detail,
                        void* userdata) -> int {
        data* d = reinterpret_cast<data*>(userdata);
        switch (type) {
        case ::MD_BLOCK_CODE:
          d->in_code_block = false;
          d->doc.code_blocks.emplace_back(std::move(d->current_block));
          break;

        case ::MD_BLOCK_H: {
          d->in_title = false;
          std::size_t colon_index = d->current_block.find(u8':');
          if (colon_index != string8::npos) {
            d->doc.title_error_code =
                to_string(d->current_block.substr(0, colon_index));
            std::size_t non_space_index =
                d->current_block.find_first_not_of(u8" ", colon_index + 1);
            if (non_space_index != string8::npos) {
              d->doc.title_error_description =
                  d->current_block.substr(non_space_index);
            }
          }
          break;
        }

        default:
          break;
        }
        return 0;
      },

      .enter_span = []([[maybe_unused]] ::MD_SPANTYPE type,
                       [[maybe_unused]] void* detail,
                       [[maybe_unused]] void* userdata) -> int { return 0; },

      .leave_span = []([[maybe_unused]] ::MD_SPANTYPE type,
                       [[maybe_unused]] void* detail,
                       [[maybe_unused]] void* userdata) -> int { return 0; },

      .text = [](::MD_TEXTTYPE type, const ::MD_CHAR* text, ::MD_SIZE size,
                 void* userdata) -> int {
        data* d = reinterpret_cast<data*>(userdata);
        if (d->in_code_block || d->in_title) {
          string8_view text_view(reinterpret_cast<const char8*>(text),
                                 narrow_cast<std::size_t>(size));
          switch (type) {
          case ::MD_TEXT_NORMAL:
          case ::MD_TEXT_CODE:
            d->current_block.append(text_view);
            break;

          case ::MD_TEXT_ENTITY:
            // TODO(strager): Decode the entity.
            d->current_block.append(text_view);
            break;

          case ::MD_TEXT_BR:
          case ::MD_TEXT_HTML:
          case ::MD_TEXT_LATEXMATH:
          case ::MD_TEXT_NULLCHAR:
          case ::MD_TEXT_SOFTBR:
          default:
            QLJS_UNIMPLEMENTED();
            break;
          }
        }
        return 0;
      },

      .debug_log = nullptr,
      .syntax = nullptr,
  };

  data d;
  d.doc.file_path = std::move(file_path);
  int rc = ::md_parse(reinterpret_cast<const char*>(markdown.data()),
                      narrow_cast<::MD_SIZE>(markdown.size()), &parser, &d);
  if (rc == -1) {
    QLJS_UNIMPLEMENTED();
  }

  return std::move(d.doc);
}

error_documentation parse_error_documentation_file(std::string&& file_path) {
  read_file_result result = read_file(file_path.c_str());
  result.exit_if_not_ok();
  return parse_error_documentation(std::move(file_path),
                                   result.content.string_view());
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
