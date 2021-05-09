// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#include <cstdlib>
#include <iostream>
#include <md4c-html.h>
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

void error_documentation::to_html(string8* out) const {
  struct parser {
    ::MD_PARSER parser;
    ::MD_PARSER* html_renderer;
    ::MD_SIZE html_renderer_size;
    const error_documentation* doc;
    string8* out;
  };

  auto write_html = [](const ::MD_CHAR* buffer, ::MD_SIZE buffer_size,
                       void* userdata) -> void {
    parser* p = reinterpret_cast<parser*>(userdata);
    p->out->append(reinterpret_cast<const char8*>(buffer), buffer_size);
  };

  unsigned parser_flags = 0;
  unsigned renderer_flags = 0;
  parser p = {
      .parser = {
          .abi_version = 0,
          .flags = parser_flags,
          .enter_block = [](::MD_BLOCKTYPE type, void* detail,
                            void* userdata) -> int {
            parser* p = reinterpret_cast<parser*>(userdata);
            switch (type) {
            case ::MD_BLOCK_CODE:
              p->out->append(u8"<figure><pre><code>");
              break;

            case ::MD_BLOCK_H: {
              ::MD_BLOCK_H_DETAIL* block_detail =
                  reinterpret_cast<::MD_BLOCK_H_DETAIL*>(detail);
              if (block_detail->level == 1) {
                p->out->append(u8"<h2><a class='self-reference' href='#");
                p->out->append(to_string8(p->doc->title_error_code));
                p->out->append(u8"'>");
              } else {
                block_detail->level += 1;
                p->html_renderer->enter_block(type, block_detail,
                                              p->html_renderer);
              }
              break;
            }

            default:
              p->html_renderer->enter_block(type, detail, p->html_renderer);
              break;
            }
            return 0;
          },

          .leave_block = [](::MD_BLOCKTYPE type, void* detail,
                            void* userdata) -> int {
            parser* p = reinterpret_cast<parser*>(userdata);
            switch (type) {
            case ::MD_BLOCK_CODE:
              p->out->append(u8"</code></pre></figure>");
              break;

            case ::MD_BLOCK_H: {
              ::MD_BLOCK_H_DETAIL* block_detail =
                  reinterpret_cast<::MD_BLOCK_H_DETAIL*>(detail);
              if (block_detail->level == 1) {
                p->out->append(u8"</a></h2>");
              } else {
                block_detail->level += 1;
                p->html_renderer->enter_block(type, block_detail,
                                              p->html_renderer);
              }
              break;
            }

            default:
              p->html_renderer->leave_block(type, detail, p->html_renderer);
              break;
            }
            return 0;
          },

          .enter_span = [](::MD_SPANTYPE type, void* detail,
                           void* userdata) -> int {
            parser* p = reinterpret_cast<parser*>(userdata);
            p->html_renderer->enter_span(type, detail, p->html_renderer);
            return 0;
          },

          .leave_span = [](::MD_SPANTYPE type, void* detail,
                           void* userdata) -> int {
            parser* p = reinterpret_cast<parser*>(userdata);
            p->html_renderer->leave_span(type, detail, p->html_renderer);
            return 0;
          },

          .text = [](::MD_TEXTTYPE type, const ::MD_CHAR* text, ::MD_SIZE size,
                     void* userdata) -> int {
            parser* p = reinterpret_cast<parser*>(userdata);

            // Wrap BOM in a <span>.
            if (size >= 3 && static_cast<unsigned char>(text[0]) == 0xef &&
                static_cast<unsigned char>(text[1]) == 0xbb &&
                static_cast<unsigned char>(text[2]) == 0xbf) {
              // U+FEFF Zero Width No-Break Space (Byte Order Mark)
              // Assumption: the BOM won't be split into multiple pieces.
              p->out->append(u8"<span class='unicode-bom'>\ufeff</span>");
              text += 3;
              size -= 3;
            }

            p->html_renderer->text(type, text, size, p->html_renderer);
            return 0;
          },

          .debug_log = nullptr,
          .syntax = nullptr,
      },
      .html_renderer = nullptr,
      .html_renderer_size = 0,
      .doc = this,
      .out = out,
  };

  ::md_html_create(nullptr, &p.html_renderer_size, write_html, &p, parser_flags,
                   renderer_flags);
  p.html_renderer =
      reinterpret_cast<::MD_PARSER*>(std::malloc(p.html_renderer_size));
  int rc = ::md_html_create(p.html_renderer, &p.html_renderer_size, write_html,
                            &p, parser_flags, renderer_flags);
  if (rc != 0) {
    std::cerr << this->file_path
              << ": fatal: creating Markdown->HTML converter failed\n";
    std::exit(EXIT_FAILURE);
  }

  rc = ::md_parse(reinterpret_cast<const ::MD_CHAR*>(this->markdown.data()),
                  this->markdown.size(), &p.parser, &p);
  if (rc != 0) {
    std::cerr << this->file_path
              << ": fatal: failed to convert Markdown to HTML\n";
    std::exit(EXIT_FAILURE);
  }

  ::md_html_destroy(p.html_renderer, p.html_renderer_size);
  std::free(p.html_renderer);
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
  d.doc.markdown = padded_string(markdown);
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

string8 substitute_error_documentation_template(
    string8_view template_string, string8_view error_documentation) {
  using string8_pos = string8_view::size_type;

  string8 out;

  string8_view remaining_template(template_string);
  string8_pos dollar_index;
  while ((dollar_index = remaining_template.find(u8'$')) != string8::npos) {
    if (dollar_index >= remaining_template.size() - 1) {
      // '$' at end of string.
      QLJS_UNIMPLEMENTED();
    }
    string8_pos left_curly_index = dollar_index + 1;
    if (remaining_template[left_curly_index] != u8'{') {
      // '$' not followed by '{'.
      QLJS_UNIMPLEMENTED();
    }

    out.append(remaining_template.substr(0, dollar_index));

    string8_pos right_curly_index =
        remaining_template.find(u8'}', left_curly_index + 1);
    if (right_curly_index == string8::npos) {
      // '${' with no matching '}'.
      QLJS_UNIMPLEMENTED();
    }
    string8_view variable_name = remaining_template.substr(
        left_curly_index + 1, right_curly_index - (left_curly_index + 1));

    string8_view variable_value;
    if (variable_name == u8"generated_message") {
      variable_value =
          u8"This file was generated using generate-error-docs.cpp.";
    } else if (variable_name == u8"error_documentation") {
      variable_value = error_documentation;
    } else {
      // Unrecognized variable name.
      QLJS_UNIMPLEMENTED();
    }
    out.append(variable_value);

    remaining_template = remaining_template.substr(right_curly_index + 1);
  }
  out.append(remaining_template);

  return out;
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
