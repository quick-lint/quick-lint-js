// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VSCODE_VSCODE_TRACER_H
#define QUICK_LINT_JS_VSCODE_VSCODE_TRACER_H

#include <cstddef>
#include <napi.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/vscode/napi-support.h>

namespace quick_lint_js {
// Writes N-API strings into buffers.
//
// Use this as a StringWriter for trace_writer.
class NAPI_String_Writer {
 public:
  std::size_t string_size(::Napi::Value string) const {
    std::size_t size;
    ::napi_status status = ::napi_get_value_string_utf16(string.Env(), string,
                                                         /*buf=*/nullptr,
                                                         /*bufsize=*/0,
                                                         /*result=*/&size);
    QLJS_ASSERT(status == ::napi_ok);
    return size;
  }

  void copy_string(::Napi::Value string, char16_t* out,
                   std::size_t capacity) const {
    std::size_t length;
    ::napi_status status = ::napi_get_value_string_utf16(string.Env(), string,
                                                         /*buf=*/out,
                                                         /*bufsize=*/capacity,
                                                         /*result=*/&length);
    QLJS_ASSERT(status == ::napi_ok);
    // If the following assertion fails, napi_get_value_string_utf16 truncated
    // the string.
    QLJS_ASSERT(length < capacity);
  }
};

// Manages traces in the VS Code extension directory.
class VSCode_Tracer {
 public:
  explicit VSCode_Tracer(const std::string& log_directory)
      : log_directory_(log_directory) {}

  ~VSCode_Tracer() { this->disable(); }

  void register_current_thread() {
    Trace_Flusher* tracer = Trace_Flusher::instance();
    tracer->register_current_thread();
    tracer->flush_sync();
  }

  void unregister_current_thread() {
    Trace_Flusher::instance()->unregister_current_thread();
  }

  void enable() {
    if (this->log_directory_.empty()) {
      // No log directory was given to us by VS Code, so we can't enable
      // tracing.
      return;
    }

    this->disable();

    auto new_backend = Trace_Flusher_Directory_Backend::create_child_directory(
        this->log_directory_);
    if (!new_backend) {
      return;
    }

    this->tracer_backend_ = std::make_unique<Trace_Flusher_Directory_Backend>(
        std::move(*new_backend));
    Trace_Flusher::instance()->enable_backend(this->tracer_backend_.get());
    QLJS_DEBUG_LOG("enabled tracing in directory %s\n",
                   this->tracer_backend_->trace_directory().c_str());
  }

  void disable() {
    if (this->tracer_backend_) {
      Trace_Flusher::instance()->disable_backend(this->tracer_backend_.get());
    }
    this->tracer_backend_.reset();
  }

  void trace_vscode_document_opened(::Napi::Env, VSCode_Document vscode_doc,
                                    void* doc) {
    Trace_Writer* tw =
        Trace_Flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      ::Napi::Object uri = vscode_doc.uri();
      tw->write_event_vscode_document_opened(
          Trace_Event_VSCode_Document_Opened<::Napi::Value>{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .uri = uri.Get("toString").As<::Napi::Function>().Call(uri, {}),
              .language_id = vscode_doc.get().Get("languageId"),
              .content = vscode_doc.get_text(),
          },
          NAPI_String_Writer());
      tw->commit();
      Trace_Flusher::instance()->flush_async();
    }
  }

  void trace_vscode_document_changed(::Napi::Env, void* doc,
                                     ::Napi::Array changes) {
    Trace_Writer* tw =
        Trace_Flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      std::vector<Trace_VSCode_Document_Change<::Napi::Value>> traced_changes(
          changes.Length());
      for (std::size_t i = 0; i < traced_changes.size(); ++i) {
        ::Napi::Object change =
            changes.Get(narrow_cast<std::uint32_t>(i)).As<::Napi::Object>();
        ::Napi::Object range = change.Get("range").As<::Napi::Object>();
        ::Napi::Object start = range.Get("start").As<::Napi::Object>();
        ::Napi::Object end = range.Get("end").As<::Napi::Object>();
        traced_changes[i] = Trace_VSCode_Document_Change<::Napi::Value>{
            .range =
                {
                    .start =
                        {
                            .line = to_uint64(start.Get("line")),
                            .character = to_uint64(start.Get("character")),
                        },
                    .end =
                        {
                            .line = to_uint64(end.Get("line")),
                            .character = to_uint64(end.Get("character")),
                        },
                },
            .range_offset = to_uint64(change.Get("rangeOffset")),
            .range_length = to_uint64(change.Get("rangeLength")),
            .text = change.Get("text"),
        };
      }
      tw->write_event_vscode_document_changed(
          Trace_Event_VSCode_Document_Changed<::Napi::Value>{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .changes = traced_changes.data(),
              .change_count = traced_changes.size(),
          },
          NAPI_String_Writer());
      tw->commit();
      Trace_Flusher::instance()->flush_async();
    }
  }

  void trace_vscode_document_closed(::Napi::Env, VSCode_Document vscode_doc,
                                    void* doc) {
    Trace_Writer* tw =
        Trace_Flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      ::Napi::Object uri = vscode_doc.uri();
      tw->write_event_vscode_document_closed(
          Trace_Event_VSCode_Document_Closed<::Napi::Value>{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .uri = uri.Get("toString").As<::Napi::Function>().Call(uri, {}),
              .language_id = vscode_doc.get().Get("languageId"),
          },
          NAPI_String_Writer());
      tw->commit();
      Trace_Flusher::instance()->flush_async();
    }
  }

  void trace_vscode_document_sync(::Napi::Env, VSCode_Document vscode_doc,
                                  void* doc) {
    Trace_Writer* tw =
        Trace_Flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      ::Napi::Object uri = vscode_doc.uri();
      tw->write_event_vscode_document_sync(
          Trace_Event_VSCode_Document_Sync<::Napi::Value>{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .uri = uri.Get("toString").As<::Napi::Function>().Call(uri, {}),
              .language_id = vscode_doc.get().Get("languageId"),
              .content = vscode_doc.get_text(),
          },
          NAPI_String_Writer());
      tw->commit();
      Trace_Flusher::instance()->flush_async();
    }
  }

 private:
  std::uint64_t timestamp() {
    // TODO(strager)
    return 0;
  }

  std::string log_directory_;
  std::unique_ptr<Trace_Flusher_Directory_Backend> tracer_backend_;
};
}

#endif

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
