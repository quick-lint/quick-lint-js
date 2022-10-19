// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <cstddef>
#include <cstdint>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-metadata.h>
#include <string>
#include <utility>

namespace quick_lint_js {
trace_flusher_directory_backend::trace_flusher_directory_backend(
    const std::string &trace_directory)
    : trace_directory_(trace_directory) {}

void trace_flusher_directory_backend::trace_thread_begin(
    trace_flusher_thread_index thread_index) {
  std::string stream_path =
      this->trace_directory_ + "/thread" + std::to_string(thread_index);
  auto file = open_file_for_writing(stream_path.c_str());
  if (!file.ok()) {
    QLJS_DEBUG_LOG("warning: failed to create trace stream file %s: %s\n",
                   stream_path.c_str(), file.error_to_string().c_str());
    return;
  }

  auto [_it, inserted] =
      this->thread_files_.emplace(thread_index, std::move(*file));
  QLJS_ASSERT(inserted);
}

void trace_flusher_directory_backend::trace_thread_end(
    trace_flusher_thread_index thread_index) {
  auto it = this->thread_files_.find(thread_index);
  if (it == this->thread_files_.end()) {
    // Opening the file failed, so there's nothing to close.
  } else {
    // Close the file we opened in trace_thread_begin.
    this->thread_files_.erase(it);
  }
}

void trace_flusher_directory_backend::trace_thread_write_data(
    trace_flusher_thread_index thread_index, const std::byte *data,
    std::size_t size) {
  auto it = this->thread_files_.find(thread_index);
  if (it == this->thread_files_.end()) {
    // Opening the file failed. Don't write anything.
    return;
  }
  platform_file_ref file = it->second.ref();

  auto write_result = file.write_full(data, size);
  if (!write_result.ok()) {
    QLJS_DEBUG_LOG("warning: failed to append to trace stream file: %s\n",
                   write_result.error_to_string().c_str());
    // TODO(strager): Disable further writes to prevent file corruption
    // and noisy logs.
  }
}

result<trace_flusher_directory_backend, write_file_io_error>
trace_flusher_directory_backend::init_directory(
    const std::string &trace_directory) {
  auto write_result = write_file(trace_directory + "/metadata", trace_metadata);
  if (!write_result.ok()) {
    return write_result.propagate();
  }
  return trace_flusher_directory_backend(trace_directory);
}

std::optional<trace_flusher_directory_backend>
trace_flusher_directory_backend::create_child_directory(
    const std::string &directory) {
  auto dir_result = create_directory(directory);
  if (!dir_result.ok()) {
    if (!dir_result.error().is_directory_already_exists_error) {
      QLJS_DEBUG_LOG("failed to create log directory %s: %s\n",
                     directory.c_str(), dir_result.error_to_string().c_str());
      return std::nullopt;
    }
  }
  result<std::string, platform_file_io_error> trace_directory =
      make_timestamped_directory(directory, "trace_%Y-%m-%d-%H-%M-%S");
  if (!trace_directory.ok()) {
    QLJS_DEBUG_LOG("failed to create tracing directory in %s: %s\n",
                   directory.c_str(),
                   trace_directory.error_to_string().c_str());
    return std::nullopt;
  }

  auto backend = init_directory(*trace_directory);
  if (!backend.ok()) {
    QLJS_DEBUG_LOG("failed to enable tracing in %s: %s\n",
                   trace_directory->c_str(), backend.error_to_string().c_str());
    return std::nullopt;
  }

  return std::move(*backend);
}
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
