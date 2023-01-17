// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

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
#include <quick-lint-js/document.h>
#include <quick-lint-js/fe/diag-reporter.h>
#include <quick-lint-js/fe/diagnostic-formatter.h>
#include <quick-lint-js/fe/diagnostic-types.h>
#include <quick-lint-js/fe/diagnostic.h>
#include <quick-lint-js/fe/linter.h>
#include <quick-lint-js/io/event-loop.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/logger.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/qljs-document.h>
#include <quick-lint-js/vscode/qljs-logger.h>
#include <quick-lint-js/vscode/thread-safe-js-function.h>
#include <quick-lint-js/vscode/vscode-configuration-filesystem.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode-tracer.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>
#include <vector>
// TODO(strager): Trim includes.

namespace quick_lint_js {
class extension_configuration {
 public:
  enum class logging_value {
    off,  // default
    verbose,
  };

  enum class tracing_value {
    off,  // default
    verbose,
  };

  explicit extension_configuration(::Napi::Env env, vscode_module& vscode)
      : config_ref_(::Napi::Persistent(
            vscode.get_configuration(env, "quick-lint-js"))) {}

  logging_value get_logging(::Napi::Env env) {
    ::Napi::Value value = this->get(env, "logging");
    if (!value.IsString()) {
      return logging_value::off;
    }
    std::string string_value = value.As<::Napi::String>().Utf8Value();
    if (string_value == "verbose") {
      return logging_value::verbose;
    } else {
      return logging_value::off;
    }
  }

  tracing_value get_tracing(::Napi::Env env) {
    ::Napi::Value value = this->get(env, "tracing");
    if (!value.IsString()) {
      return tracing_value::off;
    }
    std::string string_value = value.As<::Napi::String>().Utf8Value();
    if (string_value == "verbose") {
      return tracing_value::verbose;
    } else {
      return tracing_value::off;
    }
  }

  ::Napi::Value get(::Napi::Env env, const char* section) {
    return this->config_ref_.Get("get").As<::Napi::Function>().Call(
        this->config_ref_.Value(), {::Napi::String::New(env, section)});
  }

 private:
  ::Napi::ObjectReference config_ref_;
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
