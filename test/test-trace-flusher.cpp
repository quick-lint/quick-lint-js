// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <chrono>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <optional>
#include <quick-lint-js/async-byte-queue.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/file-handle.h>
#include <quick-lint-js/file.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/have.h>
#include <quick-lint-js/log.h>
#include <quick-lint-js/thread.h>
#include <quick-lint-js/trace-flusher.h>
#include <quick-lint-js/trace-metadata.h>
#include <quick-lint-js/trace-stream-reader-mock.h>
#include <quick-lint-js/trace-writer.h>
#include <quick-lint-js/version.h>
#include <quick-lint-js/warning.h>
#include <string>
#include <thread>

QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

using ::testing::ElementsAre;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
class test_trace_flusher : public ::testing::Test, public filesystem_test {
 protected:
  std::string trace_dir = this->make_temporary_directory();
};

void read_trace_stream_file(const std::string& path,
                            trace_stream_event_visitor& v) {
  auto stream_file = read_file(path);
  ASSERT_TRUE(stream_file.ok()) << stream_file.error_to_string();
  read_trace_stream(stream_file->data(),
                    narrow_cast<std::size_t>(stream_file->size()), v);
}

struct trace_init_event_spy : trace_stream_event_visitor {
  static std::vector<std::string> read_init_versions(
      const std::string& stream_path) {
    trace_init_event_spy v;
    read_trace_stream_file(stream_path, v);
    return v.init_versions;
  }

  void visit_error_invalid_magic() override {}
  void visit_error_invalid_uuid() override {}
  void visit_error_unsupported_compression_mode(std::uint8_t) override {}

  void visit_packet_header(const packet_header&) override {}

  void visit_init_event(const init_event& e) override {
    this->init_versions.emplace_back(e.version);
  }
  void visit_vscode_document_opened_event(
      const vscode_document_opened_event&) override {}
  void visit_vscode_document_closed_event(
      const vscode_document_closed_event&) override {}
  void visit_vscode_document_changed_event(
      const vscode_document_changed_event&) override {}
  void visit_vscode_document_sync_event(
      const vscode_document_sync_event&) override {}
  void visit_lsp_client_to_server_message_event(
      const lsp_client_to_server_message_event&) override {}

  std::vector<std::string> init_versions;
};

TEST_F(test_trace_flusher, enabling_creates_metadata_file) {
  trace_flusher flusher;
  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  auto metadata_file = read_file((trace_dir + "/metadata").c_str());
  ASSERT_TRUE(metadata_file.ok()) << metadata_file.error_to_string();

  EXPECT_THAT(to_string(metadata_file->string_view()),
              ::testing::StartsWith("/* CTF 1.8"))
      << "TSDL specification: https://diamon.org/ctf/#spec7.1";
}

TEST_F(test_trace_flusher, enabling_fails_if_directory_is_missing) {
  trace_flusher flusher;
  auto result =
      flusher.enable_for_directory(this->trace_dir + "/does-not-exist");
  EXPECT_FALSE(result.ok());
}

TEST_F(test_trace_flusher,
       enabling_with_no_threads_registered_creates_no_stream_files) {
  trace_flusher flusher;
  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  std::vector<std::string> files = list_files_in_directory(trace_dir);
  EXPECT_THAT(files, ElementsAre("metadata"));
}

TEST_F(test_trace_flusher,
       registering_then_unregistering_then_enabling_creates_no_stream_files) {
  trace_flusher flusher;
  flusher.register_current_thread();
  flusher.unregister_current_thread();

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  std::vector<std::string> files = list_files_in_directory(trace_dir);
  EXPECT_THAT(files, ElementsAre("metadata"));
}

TEST_F(test_trace_flusher, enabling_after_register_writes_stream_file) {
  trace_flusher flusher;

  flusher.register_current_thread();
  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(QUICK_LINT_JS_VERSION_STRING));
}

TEST_F(test_trace_flusher, registering_after_enabling_writes_stream_file) {
  trace_flusher flusher;

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  flusher.register_current_thread();

  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(QUICK_LINT_JS_VERSION_STRING));
}

TEST_F(test_trace_flusher, write_event_after_enabling_and_registering) {
  trace_flusher flusher;

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  flusher.register_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(::testing::_, "testing"));
}

TEST_F(test_trace_flusher, write_event_after_registering_and_enabling) {
  trace_flusher flusher;

  flusher.register_current_thread();

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(::testing::_, "testing"));
}

TEST_F(test_trace_flusher, cannot_write_events_before_enabling) {
  trace_flusher flusher;

  flusher.register_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);
}

TEST_F(test_trace_flusher, cannot_write_events_before_registering) {
  trace_flusher flusher;
  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);
}

TEST_F(test_trace_flusher, cannot_write_events_after_unregistering) {
  trace_flusher flusher;
  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  flusher.register_current_thread();
  flusher.unregister_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);
}

TEST_F(test_trace_flusher, cannot_write_events_after_enabling_then_disabling) {
  trace_flusher flusher;

  flusher.register_current_thread();

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  flusher.disable();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);
}

TEST_F(test_trace_flusher,
       can_write_events_after_enabling_then_enabling_again) {
  std::string trace_dir_2 = this->make_temporary_directory();
  trace_flusher flusher;

  flusher.register_current_thread();

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  auto result_2 = flusher.enable_for_directory(trace_dir_2);
  ASSERT_TRUE(result_2.ok()) << result.error_to_string();

  EXPECT_TRUE(flusher.trace_writer_for_current_thread());
  EXPECT_THAT(list_files_in_directory(this->trace_dir),
              UnorderedElementsAre("metadata", "thread1"));
  EXPECT_THAT(list_files_in_directory(trace_dir_2),
              UnorderedElementsAre("metadata", "thread1"));
}

TEST_F(test_trace_flusher, second_directory_stream_count_starts_at_1) {
  trace_flusher flusher;

  flusher.register_current_thread();
  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  flusher.disable();

  std::string trace_dir_2 = this->make_temporary_directory();
  auto result_2 = flusher.enable_for_directory(trace_dir_2);
  ASSERT_TRUE(result_2.ok()) << result.error_to_string();
  EXPECT_THAT(list_files_in_directory(trace_dir_2),
              UnorderedElementsAre("metadata", "thread1"));
}

TEST_F(test_trace_flusher, write_events_from_multiple_threads) {
  trace_flusher flusher;

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  flusher.register_current_thread();

  thread other_thread([&]() {
    flusher.register_current_thread();

    trace_writer* writer = flusher.trace_writer_for_current_thread();
    ASSERT_TRUE(writer);
    writer->write_event_init(trace_event_init{
        .version = u8"other thread",
    });
    writer->commit();

    flusher.unregister_current_thread();
  });

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"main thread",
  });
  writer->commit();

  other_thread.join();
  flusher.flush_sync();

  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(::testing::_, "main thread"));
  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread2"),
      ElementsAre(::testing::_, "other thread"));
}

TEST_F(test_trace_flusher, stream_file_contains_thread_id) {
  trace_flusher flusher;

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  std::uint64_t main_thread_id = get_current_thread_id();
  std::optional<std::uint64_t> other_thread_id;
  flusher.register_current_thread();
  thread other_thread([&] {
    flusher.register_current_thread();
    flusher.flush_sync();
    flusher.unregister_current_thread();
    other_thread_id = get_current_thread_id();
  });
  other_thread.join();
  flusher.flush_sync();

  nice_mock_trace_stream_event_visitor main_v;
  EXPECT_CALL(main_v, visit_packet_header(::testing::Field(
                          &trace_stream_event_visitor::packet_header::thread_id,
                          main_thread_id)));
  read_trace_stream_file(this->trace_dir + "/thread1", main_v);

  ASSERT_TRUE(other_thread_id.has_value());
  EXPECT_NE(*other_thread_id, main_thread_id);
  nice_mock_trace_stream_event_visitor other_v;
  EXPECT_CALL(other_v,
              visit_packet_header(::testing::Field(
                  &trace_stream_event_visitor::packet_header::thread_id,
                  *other_thread_id)));
  read_trace_stream_file(this->trace_dir + "/thread2", other_v);
}

TEST_F(test_trace_flusher, unregistering_thread_flushes_committed_data) {
  trace_flusher flusher;

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  flusher.register_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();

  flusher.unregister_current_thread();
  // NOTE(strager): We do not call flusher.flush_sync.

  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(::testing::_, "testing"));
}

TEST_F(test_trace_flusher, flush_async_does_not_flush_on_current_thread) {
  trace_flusher flusher;

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  flusher.register_current_thread();
  flusher.flush_sync();  // Write the normal init event.

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();
  flusher.flush_async();  // Flush the testing init event, but not now.

  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(::testing::Not("testing")))
      << "creating the stream file should add an init event automatically (but "
         "not the testing init event)";
}

TEST_F(test_trace_flusher, flush_async_flushes_on_flusher_thread) {
  trace_flusher flusher;
  flusher.start_flushing_thread();

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();

  flusher.register_current_thread();
  flusher.flush_sync();  // Write the normal init event.

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();
  flusher
      .flush_async();  // Flush the testing init event, but not on this thread.

  std::chrono::time_point deadline =
      std::chrono::steady_clock::now() + std::chrono::seconds(3);
  while (trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1")
             .size() <= 1) {
    if (std::chrono::steady_clock::now() >= deadline) {
      ADD_FAILURE() << "timed out waiting for flusher thread to write to file";
      break;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }
  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(::testing::_, "testing"));
}

TEST_F(test_trace_flusher, flushing_disabled_does_nothing) {
  trace_flusher flusher;
  flusher.register_current_thread();

  // This should do nothing. In particular, it should not prevent the
  // stream header or init event from being written.
  flusher.flush_sync();

  auto result = flusher.enable_for_directory(this->trace_dir);
  ASSERT_TRUE(result.ok()) << result.error_to_string();
  EXPECT_THAT(
      trace_init_event_spy::read_init_versions(this->trace_dir + "/thread1"),
      ElementsAre(QUICK_LINT_JS_VERSION_STRING));
}
}
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
