// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_VSCODE_VSCODE_CONFIGURATION_FILESYSTEM_H
#define QUICK_LINT_JS_VSCODE_VSCODE_CONFIGURATION_FILESYSTEM_H

// TODO(strager): Trim includes.
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
#include <quick-lint-js/lsp/lsp-document-text.h>
#include <quick-lint-js/lsp/lsp-location.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/vscode/napi-support.h>
#include <quick-lint-js/vscode/vscode-diag-reporter.h>
#include <quick-lint-js/vscode/vscode-tracer.h>
#include <quick-lint-js/vscode/vscode.h>
#include <string>
#include <string_view>
#include <vector>

namespace quick_lint_js {
template <class UnderlyingFilesystem>
class thread_safe_configuration_filesystem : public configuration_filesystem {
 public:
  template <class... Args>
  explicit thread_safe_configuration_filesystem(Args&&... args)
      : underlying_fs_(std::forward<Args>(args)...) {}

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.canonicalize_path(path);
  }

  result<padded_string, read_file_io_error> read_file(
      const canonical_path& path) override {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.read_file(path);
  }

  auto get_inotify_fd() {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.get_inotify_fd();
  }

  template <class Event>
  auto handle_kqueue_event(Event&& event) {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.handle_kqueue_event(std::forward<Event>(event));
  }

  template <class Event>
  auto handle_poll_event(Event&& event) {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.handle_poll_event(std::forward<Event>(event));
  }

  template <class Overlapped, class Number, class Error>
  auto handle_event(Overlapped overlapped, Number number_of_bytes_transferred,
                    Error error) {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.handle_event(
        overlapped, number_of_bytes_transferred, error);
  }

  void clear_watches() {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.clear_watches();
  }

  auto take_watch_errors() {
    std::lock_guard lock(this->lock_);
    return this->underlying_fs_.take_watch_errors();
  }

 private:
  mutex lock_;
  UnderlyingFilesystem underlying_fs_;
};

// A configuration_filesystem which allows unsaved VS Code documents to appear
// as real files.
class vscode_configuration_filesystem : public configuration_filesystem {
 public:
  explicit vscode_configuration_filesystem(
      configuration_filesystem* underlying_fs)
      : underlying_fs_(underlying_fs) {}

  result<canonical_path_result, canonicalize_path_io_error> canonicalize_path(
      const std::string& path) override {
    return this->underlying_fs_->canonicalize_path(path);
  }

  result<padded_string, read_file_io_error> read_file(
      const canonical_path& path) override {
    qljs_document_base* doc = this->find_document(path.path());
    if (!doc) {
      QLJS_DEBUG_LOG("Reading file from disk: %s\n", path.c_str());
      return this->underlying_fs_->read_file(path);
    }
    QLJS_DEBUG_LOG("Reading file from open document: %s\n", path.c_str());
    return padded_string(doc->document_string().string_view());
  }

  void clear() { this->overlaid_documents_.clear(); }

  void overlay_document(const std::string& file_path, qljs_document_base* doc) {
    auto [_it, inserted] =
        this->overlaid_documents_.try_emplace(file_path, doc);
    QLJS_ASSERT(inserted);
  }

  void forget_document(qljs_document_base* doc) {
    auto doc_it = std::find_if(this->overlaid_documents_.begin(),
                               this->overlaid_documents_.end(),
                               [&](auto& pair) { return pair.second == doc; });
    if (doc_it == this->overlaid_documents_.end()) {
      // The document could be missing for any of the following reasons:
      // * Our extension was loaded after the document was opened.
      //   (TODO(strager): We should fix this.)
      // * The closed document was unnamed.
    } else {
      this->overlaid_documents_.erase(doc_it);
    }
  }

 private:
  qljs_document_base* find_document(std::string_view path) {
#if QLJS_HAVE_STD_TRANSPARENT_KEYS
    std::string_view key = path;
#else
    std::string key(path);
#endif
    auto doc_it = this->overlaid_documents_.find(key);
    if (doc_it == this->overlaid_documents_.end()) {
      return nullptr;
    }
    return doc_it->second;
  }

  hash_map<std::string, qljs_document_base*> overlaid_documents_;
  configuration_filesystem* underlying_fs_;
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
