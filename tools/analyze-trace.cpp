// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstdio>
#include <quick-lint-js/arg-parser.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/trace-stream-reader.h>
#include <vector>

namespace quick_lint_js {
namespace {
struct analyze_options {
  std::vector<const char*> trace_files;
};

class event_dumper : public trace_stream_event_visitor {
 private:
  static constexpr int header_width = 16;

 public:
  void visit_error_invalid_magic() override {
    std::printf("error: invalid magic\n");
  }

  void visit_error_invalid_uuid() override {
    std::printf("error: invalid UUID\n");
  }

  void visit_error_unsupported_compression_mode(std::uint8_t mode) override {
    std::printf("error: unsupported compression mode: %#02x\n", mode);
  }

  void visit_packet_header(const packet_header&) override {}

  void visit_init_event(const init_event& event) override {
    this->print_event_header(event);
    std::printf("init version='%s'\n", event.version);
  }

  void visit_vscode_document_opened_event(
      const vscode_document_opened_event& event) override {
    this->print_event_header(event);
    std::printf("document ");
    this->print_document_id(event.document_id);
    std::printf(" opened: ");
    this->print_utf16(event.uri);
    std::printf("\n");
  }

  void visit_vscode_document_closed_event(
      const vscode_document_closed_event& event) override {
    this->print_event_header(event);
    std::printf("document ");
    this->print_document_id(event.document_id);
    std::printf(" closed: ");
    this->print_utf16(event.uri);
    std::printf("\n");
  }

  void visit_vscode_document_changed_event(
      const vscode_document_changed_event& event) override {
    this->print_event_header(event);
    std::printf("document ");
    this->print_document_id(event.document_id);
    std::printf(" changed\n");
    for (const auto& change : event.changes) {
      std::printf("%*s%llu:%llu->%llu:%llu: '", this->header_width, "",
                  narrow_cast<unsigned long long>(change.range.start.line),
                  narrow_cast<unsigned long long>(change.range.start.character),
                  narrow_cast<unsigned long long>(change.range.end.line),
                  narrow_cast<unsigned long long>(change.range.end.character));
      this->print_utf16(change.text);
      std::printf("'\n");
    }
  }

 private:
  template <class Event>
  void print_event_header(const Event& event) {
    std::uint64_t ns_per_s = 1'000'000'000;
    std::printf("@%0*llu.%09llu ", this->header_width - 1 - 1 - 9 - 1,
                narrow_cast<unsigned long long>(event.timestamp % ns_per_s),
                narrow_cast<unsigned long long>(event.timestamp / ns_per_s));
  }

  void print_document_id(std::uint64_t document_id) {
    std::printf("%#llx", narrow_cast<unsigned long long>(document_id));
  }

  void print_utf16(std::u16string_view s) {
    // TODO(strager): Support non-ASCII.
    for (char16_t c : s) {
      putchar(narrow_cast<char8>(c));
    }
  }
};

analyze_options parse_analyze_options(int argc, char** argv) {
  analyze_options o;

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      o.trace_files.push_back(argument);
    } else {
      const char* unrecognized = parser.match_anything();
      std::fprintf(stderr, "error: unrecognized option: %s\n", unrecognized);
      std::exit(2);
    }
  }

  return o;
}
}
}

int main(int argc, char** argv) {
  using namespace quick_lint_js;

  analyze_options o = parse_analyze_options(argc, argv);
  if (o.trace_files.empty()) {
    std::fprintf(stderr, "error: missing trace file\n");
    return 2;
  }
  if (o.trace_files.size() > 1) {
    std::fprintf(stderr, "error: unexpected arguments\n");
    return 2;
  }

  auto file = read_file(argv[1]);
  if (!file.ok()) {
    file.error().print_and_exit();
  }
  event_dumper dumper;
  read_trace_stream(file->data(), narrow_cast<std::size_t>(file->size()),
                    dumper);

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
