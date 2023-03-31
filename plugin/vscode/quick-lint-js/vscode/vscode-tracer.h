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
class napi_string_writer {
 public:
  explicit napi_string_writer(::Napi::Env env) : env_(env) {}

  std::size_t string_size(void* string) const noexcept {
    std::size_t size;
    ::napi_status status =
        ::napi_get_value_string_utf16(this->env_, this->get(string),
                                      /*buf=*/nullptr,
                                      /*bufsize=*/0,
                                      /*result=*/&size);
    QLJS_ASSERT(status == ::napi_ok);
    return size;
  }

  void copy_string(void* string, char16_t* out, std::size_t capacity) const
      noexcept {
    std::size_t length;
    ::napi_status status =
        ::napi_get_value_string_utf16(this->env_, this->get(string),
                                      /*buf=*/out,
                                      /*bufsize=*/capacity,
                                      /*result=*/&length);
    QLJS_ASSERT(status == ::napi_ok);
    // If the following assertion fails, napi_get_value_string_utf16 truncated
    // the string.
    QLJS_ASSERT(length < capacity);
  }

 private:
  static ::napi_value get(void* string) noexcept {
    return reinterpret_cast<::napi_value>(string);
  }

  ::Napi::Env env_;
};

// Manages traces in the VS Code extension directory.
class vscode_tracer {
 public:
  explicit vscode_tracer(const std::string& log_directory)
      : log_directory_(log_directory) {}

  ~vscode_tracer() { this->disable(); }

  void register_current_thread() {
    trace_flusher* tracer = trace_flusher::instance();
    tracer->register_current_thread();
    tracer->flush_sync();
  }

  void unregister_current_thread() {
    trace_flusher::instance()->unregister_current_thread();
  }

  void enable() {
    if (this->log_directory_.empty()) {
      // No log directory was given to us by VS Code, so we can't enable
      // tracing.
      return;
    }

    this->disable();

    auto new_backend = trace_flusher_directory_backend::create_child_directory(
        this->log_directory_);
    if (!new_backend) {
      return;
    }

    this->tracer_backend_ = std::make_unique<trace_flusher_directory_backend>(
        std::move(*new_backend));
    trace_flusher::instance()->enable_backend(this->tracer_backend_.get());
    QLJS_DEBUG_LOG("enabled tracing in directory %s\n",
                   this->tracer_backend_->trace_directory().c_str());
  }

  void disable() {
    if (this->tracer_backend_) {
      trace_flusher::instance()->disable_backend(this->tracer_backend_.get());
    }
    this->tracer_backend_.reset();
  }

  void trace_vscode_document_opened(::Napi::Env env, vscode_document vscode_doc,
                                    void* doc) {
    trace_writer* tw =
        trace_flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      ::Napi::Object uri = vscode_doc.uri();
      tw->write_event_vscode_document_opened(
          trace_event_vscode_document_opened{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .uri = ::napi_value(
                  uri.Get("toString").As<::Napi::Function>().Call(uri, {})),
              .language_id = ::napi_value(vscode_doc.get().Get("languageId")),
              .content = ::napi_value(vscode_doc.get_text()),
          },
          napi_string_writer(env));
      tw->commit();
      trace_flusher::instance()->flush_async();
    }
  }

  void trace_vscode_document_changed(::Napi::Env env, void* doc,
                                     ::Napi::Array changes) {
    trace_writer* tw =
        trace_flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      std::vector<trace_vscode_document_change> traced_changes(
          changes.Length());
      for (std::size_t i = 0; i < traced_changes.size(); ++i) {
        ::Napi::Object change =
            changes.Get(narrow_cast<std::uint32_t>(i)).As<::Napi::Object>();
        ::Napi::Object range = change.Get("range").As<::Napi::Object>();
        ::Napi::Object start = range.Get("start").As<::Napi::Object>();
        ::Napi::Object end = range.Get("end").As<::Napi::Object>();
        traced_changes[i] = trace_vscode_document_change{
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
            .text = ::napi_value(change.Get("text")),
        };
      }
      tw->write_event_vscode_document_changed(
          trace_event_vscode_document_changed{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .changes = traced_changes.data(),
              .change_count = traced_changes.size(),
          },
          napi_string_writer(env));
      tw->commit();
      trace_flusher::instance()->flush_async();
    }
  }

  void trace_vscode_document_closed(::Napi::Env env, vscode_document vscode_doc,
                                    void* doc) {
    trace_writer* tw =
        trace_flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      ::Napi::Object uri = vscode_doc.uri();
      tw->write_event_vscode_document_closed(
          trace_event_vscode_document_closed{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .uri = ::napi_value(
                  uri.Get("toString").As<::Napi::Function>().Call(uri, {})),
              .language_id = ::napi_value(vscode_doc.get().Get("languageId")),
          },
          napi_string_writer(env));
      tw->commit();
      trace_flusher::instance()->flush_async();
    }
  }

  void trace_vscode_document_sync(::Napi::Env env, vscode_document vscode_doc,
                                  void* doc) {
    trace_writer* tw =
        trace_flusher::instance()->trace_writer_for_current_thread();
    if (tw) {
      ::Napi::Object uri = vscode_doc.uri();
      tw->write_event_vscode_document_sync(
          trace_event_vscode_document_sync{
              .timestamp = this->timestamp(),
              .document_id = reinterpret_cast<std::uintptr_t>(doc),
              .uri = ::napi_value(
                  uri.Get("toString").As<::Napi::Function>().Call(uri, {})),
              .language_id = ::napi_value(vscode_doc.get().Get("languageId")),
              .content = ::napi_value(vscode_doc.get_text()),
          },
          napi_string_writer(env));
      tw->commit();
      trace_flusher::instance()->flush_async();
    }
  }

 private:
  std::uint64_t timestamp() {
    // TODO(strager)
    return 0;
  }

  std::string log_directory_;
  std::unique_ptr<trace_flusher_directory_backend> tracer_backend_;
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
