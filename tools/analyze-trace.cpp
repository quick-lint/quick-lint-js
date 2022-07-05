// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <quick-lint-js/arg-parser.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/document.h>
#include <quick-lint-js/file-path.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/lsp-location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/padded-string.h>
#include <quick-lint-js/temporary-directory.h>
#include <quick-lint-js/trace-stream-reader.h>
#include <quick-lint-js/utf-16.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
struct analyze_options {
  std::vector<const char*> trace_files;
  std::optional<std::uint64_t> dump_final_document_content_document_id;
  bool check_document_consistency = false;
};

lsp_range to_lsp_range(
    const trace_stream_event_visitor::vscode_document_range& range) {
  return lsp_range{
      .start =
          {
              .line = narrow_cast<int>(range.start.line),
              .character = narrow_cast<int>(range.start.character),
          },
      .end =
          {
              .line = narrow_cast<int>(range.end.line),
              .character = narrow_cast<int>(range.end.character),
          },
  };
}

class document_content_dumper : public trace_stream_event_visitor {
 public:
  explicit document_content_dumper(std::uint64_t document_id)
      : document_id_(document_id) {}

  void visit_error_invalid_magic() override {
    std::fprintf(stderr, "error: invalid magic\n");
  }

  void visit_error_invalid_uuid() override {
    std::fprintf(stderr, "error: invalid UUID\n");
  }

  void visit_error_unsupported_compression_mode(std::uint8_t mode) override {
    std::fprintf(stderr, "error: unsupported compression mode: %#02x\n", mode);
  }

  void visit_packet_header(const packet_header&) override {}

  void visit_init_event(const init_event&) override {}

  void visit_vscode_document_opened_event(
      const vscode_document_opened_event& event) override {
    if (event.document_id != this->document_id_) {
      return;
    }
    this->doc_.set_text(utf_16_to_utf_8(event.content));
  }

  void visit_vscode_document_closed_event(
      const vscode_document_closed_event& event) override {
    if (event.document_id != this->document_id_) {
      return;
    }
    this->doc_.set_text(u8"(document closed)");
  }

  void visit_vscode_document_changed_event(
      const vscode_document_changed_event& event) override {
    if (event.document_id != this->document_id_) {
      return;
    }

    for (const auto& change : event.changes) {
      this->doc_.replace_text(to_lsp_range(change.range),
                              utf_16_to_utf_8(change.text));
    }
  }

  void visit_vscode_document_sync_event(
      const vscode_document_sync_event&) override {}

  void visit_lsp_client_to_server_message_event(
      const lsp_client_to_server_message_event&) override {}

  void print_document_content() {
    padded_string_view s = this->doc_.string();
    std::fwrite(s.data(), 1, narrow_cast<std::size_t>(s.size()), stdout);
  }

 private:
  document<lsp_locator> doc_;
  std::uint64_t document_id_;
};

class document_content_checker : public trace_stream_event_visitor {
 public:
  void visit_error_invalid_magic() override {
    std::fprintf(stderr, "error: invalid magic\n");
  }

  void visit_error_invalid_uuid() override {
    std::fprintf(stderr, "error: invalid UUID\n");
  }

  void visit_error_unsupported_compression_mode(std::uint8_t mode) override {
    std::fprintf(stderr, "error: unsupported compression mode: %#02x\n", mode);
  }

  void visit_packet_header(const packet_header&) override {}

  void visit_init_event(const init_event&) override {}

  void visit_vscode_document_opened_event(
      const vscode_document_opened_event& event) override {
    if (event.document_id == 0) {
      return;
    }
    this->documents_[event.document_id].set_text(
        utf_16_to_utf_8(event.content));
  }

  void visit_vscode_document_closed_event(
      const vscode_document_closed_event& event) override {
    if (event.document_id == 0) {
      return;
    }
    this->documents_.erase(event.document_id);
  }

  void visit_vscode_document_changed_event(
      const vscode_document_changed_event& event) override {
    if (event.document_id == 0) {
      return;
    }

    auto doc_it = this->documents_.find(event.document_id);
    if (doc_it == this->documents_.end()) {
      std::fprintf(stderr,
                   "warning: document %#llx changed but wasn't opened\n",
                   narrow_cast<unsigned long long>(event.document_id));
      return;
    }
    document<lsp_locator>& doc = doc_it->second;
    for (const auto& change : event.changes) {
      doc.replace_text(to_lsp_range(change.range),
                       utf_16_to_utf_8(change.text));
    }
  }

  void visit_vscode_document_sync_event(
      const vscode_document_sync_event& event) override {
    if (event.document_id == 0) {
      return;
    }

    auto doc_it = this->documents_.find(event.document_id);
    if (doc_it == this->documents_.end()) {
      std::fprintf(stderr, "warning: document %#llx synced but wasn't opened\n",
                   narrow_cast<unsigned long long>(event.document_id));
      return;
    }
    document<lsp_locator>& doc = doc_it->second;

    string8_view actual = doc.string().string_view();
    string8 expected = utf_16_to_utf_8(event.content);
    if (actual != expected) {
      std::fprintf(stderr, "error: document mismatch\n");

      std::string dir = make_temporary_directory();
      std::string expected_path =
          dir + QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "expected";
      write_file_or_exit(expected_path, expected);
      std::fprintf(stderr, "note: expected written to %s\n",
                   expected_path.c_str());

      std::string actual_path =
          dir + QLJS_PREFERRED_PATH_DIRECTORY_SEPARATOR + "actual";
      write_file_or_exit(actual_path, actual);
      std::fprintf(stderr, "note: actual written to %s\n", actual_path.c_str());
    }
  }

  void visit_lsp_client_to_server_message_event(
      const lsp_client_to_server_message_event&) override {}

 private:
  std::unordered_map<std::uint64_t, document<lsp_locator>> documents_;
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

  void visit_vscode_document_sync_event(
      const vscode_document_sync_event& event) override {
    this->print_event_header(event);
    std::printf("document ");
    this->print_document_id(event.document_id);
    std::printf(" sync: ");
    this->print_utf16(event.uri);
    std::printf("\n");
  }

  void visit_lsp_client_to_server_message_event(
      const lsp_client_to_server_message_event& event) override {
    this->print_event_header(event);
    std::printf("client->server LSP message: ");
    this->print_utf8(event.body);
    std::printf("\n");
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
    this->print_utf8(utf_16_to_utf_8(s));
  }

  void print_utf8(string8_view s) {
    std::fwrite(s.data(), 1, s.size(), stdout);
  }
};

analyze_options parse_analyze_options(int argc, char** argv) {
  analyze_options o;

  arg_parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      o.trace_files.push_back(argument);
    } else if (parser.match_flag_option("--check-document-consistency"sv,
                                        "--check"sv)) {
      o.check_document_consistency = true;
    } else if (const char* arg_value = parser.match_option_with_value(
                   "--dump-final-document-content"sv)) {
      errno = 0;
      char* end;
      unsigned long long document_id =
          std::strtoull(arg_value, &end, /*base=*/0);
      if (errno != 0 || end == arg_value || *end != '\0') {
        std::fprintf(stderr, "error: malformed document ID: %s\n", arg_value);
        std::exit(2);
      }
      o.dump_final_document_content_document_id =
          narrow_cast<std::uint64_t>(document_id);
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

  auto file = read_file(o.trace_files[0]);
  if (!file.ok()) {
    file.error().print_and_exit();
  }

  if (o.check_document_consistency) {
    document_content_checker checker;
    read_trace_stream(file->data(), narrow_cast<std::size_t>(file->size()),
                      checker);
  }

  if (o.dump_final_document_content_document_id.has_value()) {
    document_content_dumper dumper(*o.dump_final_document_content_document_id);
    read_trace_stream(file->data(), narrow_cast<std::size_t>(file->size()),
                      dumper);
    dumper.print_document_content();
  } else if (!o.check_document_consistency) {
    event_dumper dumper;
    read_trace_stream(file->data(), narrow_cast<std::size_t>(file->size()),
                      dumper);
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
