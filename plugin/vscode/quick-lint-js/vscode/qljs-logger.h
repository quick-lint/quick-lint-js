// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <cstdio>
#include <memory>
#include <napi.h>
#include <optional>
#include <quick-lint-js/configuration/basic-configuration-filesystem.h>
#include <quick-lint-js/configuration/change-detecting-filesystem.h>
#include <quick-lint-js/configuration/configuration-loader.h>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/container/hash-map.h>
#include <quick-lint-js/container/padded-string.h>
#include <quick-lint-js/diag/diag-reporter.h>
#include <quick-lint-js/diag/diagnostic-formatter.h>
#include <quick-lint-js/diag/diagnostic-types.h>
#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/logger.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/thread-safe-js-function.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode-tracer.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>
#include <vector>
// TODO(strager): Trim includes.

namespace quick_lint_js {
class QLJS_Logger : public Logger, public ::Napi::ObjectWrap<QLJS_Logger> {
 public:
  static ::Napi::Function init(::Napi::Env env) {
    return DefineClass(env, "QLJSLogger", {});
  }

  explicit QLJS_Logger(const ::Napi::CallbackInfo& info)
      : ::Napi::ObjectWrap<QLJS_Logger>(info),
        output_channel_ref_(::Napi::Persistent(info[0].As<::Napi::Object>())),
        flush_on_js_thread_(
            /*env=*/info.Env(),
            /*resourceName=*/"quick-lint-js-log",
            /*object=*/this->Value()) {}

  void log(std::string_view message) override {
    {
      std::lock_guard lock(this->mutex_);
      this->pending_log_messages_.emplace_back(message);
    }

    this->begin_flush_async();
  }

  void flush(::Napi::Env env) {
    std::vector<std::string> log_messages;

    {
      std::lock_guard lock(this->mutex_);
      std::swap(log_messages, this->pending_log_messages_);
    }

    for (const std::string& message : log_messages) {
      this->output_channel_ref_.Value()
          .Get("append")
          .As<::Napi::Function>()
          .Call(this->output_channel_ref_.Value(),
                {::Napi::String::New(env, message)});
    }
  }

 private:
  void begin_flush_async() { this->flush_on_js_thread_.BlockingCall(); }

  static void flush_from_thread(::Napi::Env env, ::Napi::Object logger_object) {
    QLJS_Logger* logger = QLJS_Logger::Unwrap(logger_object);
    logger->flush(env);
  }

  Mutex mutex_;
  std::vector<std::string> pending_log_messages_;

  ::Napi::Reference<::Napi::Object> output_channel_ref_;
  Thread_Safe_JS_Function<flush_from_thread> flush_on_js_thread_;
};
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
