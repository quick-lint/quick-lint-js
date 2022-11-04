// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No filesystem on web.
#else

#include <array>
#include <chrono>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <map>
#include <optional>
#include <quick-lint-js/container/async-byte-queue.h>
#include <quick-lint-js/filesystem-test.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/logging/trace-flusher.h>
#include <quick-lint-js/logging/trace-metadata.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/process.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/trace-stream-reader-mock.h>
#include <quick-lint-js/version.h>
#include <string>
#include <thread>

QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
class test_trace_flusher : public ::testing::Test {
 protected:
  trace_flusher flusher;
};

class test_trace_flusher_directory_backend : public ::testing::Test,
                                             public filesystem_test {
 protected:
  std::string trace_dir = this->make_temporary_directory();
};

void read_trace_stream_file(const std::string& path,
                            trace_stream_event_visitor& v) {
  auto stream_file = read_file(path);
  ASSERT_TRUE(stream_file.ok()) << stream_file.error_to_string();
  trace_stream_reader reader(&v);
  reader.append_bytes(stream_file->data(),
                      narrow_cast<std::size_t>(stream_file->size()));
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
  void visit_vector_max_size_histogram_by_owner_event(
      const vector_max_size_histogram_by_owner_event&) override {}
  void visit_process_id_event(const process_id_event&) override {}

  std::vector<std::string> init_versions;
};

class spy_trace_flusher_backend final : public trace_flusher_backend {
 public:
  void trace_thread_begin(trace_flusher_thread_index thread_index) override {
    std::lock_guard<mutex> lock(this->mutex_);

    thread_state& t = this->thread_states[thread_index];
    EXPECT_EQ(t.begin_calls, 0);
    EXPECT_EQ(t.write_calls, 0);
    EXPECT_EQ(t.end_calls, 0);

    t.begin_calls += 1;
  }

  void trace_thread_end(trace_flusher_thread_index thread_index) override {
    std::lock_guard<mutex> lock(this->mutex_);

    thread_state& t = this->thread_states[thread_index];
    EXPECT_EQ(t.begin_calls, 1);
    EXPECT_EQ(t.end_calls, 0);

    t.end_calls += 1;
  }

  void trace_thread_write_data(trace_flusher_thread_index thread_index,
                               const std::byte* data,
                               std::size_t size) override {
    std::lock_guard<mutex> lock(this->mutex_);

    thread_state& t = this->thread_states[thread_index];
    EXPECT_GE(t.begin_calls, 1);
    EXPECT_EQ(t.end_calls, 0);

    t.written_data.append(reinterpret_cast<const char*>(data), size);
    t.write_calls += 1;
  }

  std::vector<trace_flusher_thread_index> thread_indexes() const {
    std::lock_guard<mutex> lock(this->mutex_);
    std::vector<trace_flusher_thread_index> result;
    for (auto& [thread_index, _t] : this->thread_states) {
      result.push_back(thread_index);
    }
    return result;
  }

  void read_thread_trace_stream(trace_flusher_thread_index thread_index,
                                trace_stream_event_visitor& v) const {
    std::lock_guard<mutex> lock(this->mutex_);
    auto it = this->thread_states.find(thread_index);
    ASSERT_NE(it, this->thread_states.end());
    const thread_state& t = it->second;
    trace_stream_reader reader(&v);
    reader.append_bytes(t.written_data.data(),
                        narrow_cast<std::size_t>(t.written_data.size()));
  }

  std::vector<std::string> read_thread_init_versions(
      trace_flusher_thread_index thread_index) const {
    trace_init_event_spy v;
    this->read_thread_trace_stream(thread_index, v);
    return v.init_versions;
  }

  struct thread_state {
    int begin_calls = 0;
    int end_calls = 0;
    int write_calls = 0;
    std::string written_data;
  };

  void reset() {
    std::lock_guard<mutex> lock(this->mutex_);
    this->thread_states.clear();
  }

  std::map<trace_flusher_thread_index, thread_state> thread_states;
  mutable mutex mutex_;
};

TEST_F(test_trace_flusher, enabling_enables) {
  EXPECT_FALSE(flusher.is_enabled());

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  EXPECT_TRUE(flusher.is_enabled());

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher,
       enabling_with_no_threads_registered_begins_no_threads) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.thread_states, IsEmpty());

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher,
       registering_then_unregistering_then_enabling_begins_no_threads) {
  flusher.register_current_thread();
  flusher.unregister_current_thread();

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.thread_indexes(), IsEmpty());

  flusher.disable_all_backends();
}

TEST_F(
    test_trace_flusher,
    enabling_then_registering_then_unregistering_then_registering_does_not_begin_thread_again) {
  // TODO(strager): Does it make sense for the thread to not begin again? It was
  // ended, after all, and we might call write again.

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  flusher.register_current_thread();
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 0);

  flusher.unregister_current_thread();
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 1);

  flusher.register_current_thread();
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 1);

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, enabling_after_register_begins_thread) {
  flusher.register_current_thread();

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.thread_indexes(), ElementsAre(1));
  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(QUICK_LINT_JS_VERSION_STRING));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, registering_after_enabling_begins_thread) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  flusher.register_current_thread();

  EXPECT_THAT(backend.thread_indexes(), ElementsAre(1));
  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(QUICK_LINT_JS_VERSION_STRING));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher,
       registering_after_enabling_begins_thread_on_all_backends) {
  spy_trace_flusher_backend backend_1;
  flusher.enable_backend(&backend_1);
  spy_trace_flusher_backend backend_2;
  flusher.enable_backend(&backend_2);

  flusher.register_current_thread();

  EXPECT_THAT(backend_1.read_thread_init_versions(1),
              ElementsAre(QUICK_LINT_JS_VERSION_STRING));
  EXPECT_THAT(backend_2.read_thread_init_versions(1),
              ElementsAre(QUICK_LINT_JS_VERSION_STRING));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, write_event_after_enabling_and_registering) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  flusher.register_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, write_event_after_registering_and_enabling) {
  flusher.register_current_thread();

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, cannot_write_events_before_enabling) {
  flusher.register_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);
}

TEST_F(test_trace_flusher, cannot_write_events_before_registering) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, cannot_write_events_after_unregistering) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  flusher.register_current_thread();
  flusher.unregister_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, cannot_write_events_after_enabling_then_disabling) {
  flusher.register_current_thread();

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  flusher.disable_backend(&backend);

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, disabling_disables) {
  spy_trace_flusher_backend backend;

  flusher.enable_backend(&backend);
  ASSERT_TRUE(flusher.is_enabled());

  flusher.disable_backend(&backend);
  EXPECT_FALSE(flusher.is_enabled());

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher,
       can_write_events_after_enabling_then_disabling_then_enabling_again) {
  spy_trace_flusher_backend backend_1;
  spy_trace_flusher_backend backend_2;

  flusher.register_current_thread();

  flusher.enable_backend(&backend_1);
  flusher.disable_backend(&backend_1);
  flusher.enable_backend(&backend_2);

  EXPECT_TRUE(flusher.trace_writer_for_current_thread());
  EXPECT_THAT(backend_1.thread_indexes(), ElementsAre(1));
  EXPECT_THAT(backend_2.thread_indexes(), ElementsAre(1));

  flusher.disable_all_backends();
}

// FIXME(strager): This test is misleading. It used to be that disabling reset
// thread indexes, but now, thread indexes are preserved across
// disables/enables.
TEST_F(test_trace_flusher, second_backend_thread_index_starts_at_1) {
  flusher.register_current_thread();

  spy_trace_flusher_backend backend_1;
  flusher.enable_backend(&backend_1);
  flusher.disable_backend(&backend_1);

  spy_trace_flusher_backend backend_2;
  flusher.enable_backend(&backend_2);

  EXPECT_THAT(backend_2.thread_indexes(), ElementsAre(1));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, disabling_backend_ends_all_registered_threads) {
  flusher.register_current_thread();

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  mutex test_mutex;
  condition_variable cond;
  bool registered_thread_2 = false;
  bool finished_test = false;

  std::thread thread_2([&]() {
    flusher.register_current_thread();
    {
      std::lock_guard<mutex> lock(test_mutex);
      registered_thread_2 = true;
      cond.notify_all();
    }

    {
      std::unique_lock<mutex> lock(test_mutex);
      cond.wait(lock, [&] { return finished_test; });
    }
    flusher.unregister_current_thread();
  });

  {
    std::unique_lock<mutex> lock(test_mutex);
    cond.wait(lock, [&] { return registered_thread_2; });
  }

  ASSERT_THAT(backend.thread_indexes(), ElementsAre(1, 2));
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 0);
  EXPECT_EQ(backend.thread_states[2].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[2].end_calls, 0);

  flusher.disable_backend(&backend);

  EXPECT_THAT(backend.thread_indexes(), ElementsAre(1, 2));
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 1);
  EXPECT_EQ(backend.thread_states[2].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[2].end_calls, 1);

  {
    std::lock_guard<mutex> lock(test_mutex);
    finished_test = true;
    cond.notify_all();
  }
  thread_2.join();

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, unregistering_thread_calls_thread_end) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  mutex test_mutex;
  condition_variable cond;
  bool registered_thread_2 = false;
  bool should_unregister_thread_2 = false;
  bool unregistered_thread_2 = false;
  bool finished_test = false;

  flusher.register_current_thread();

  std::thread thread_2([&]() {
    flusher.register_current_thread();
    {
      std::unique_lock<mutex> lock(test_mutex);
      registered_thread_2 = true;
      cond.notify_all();
      cond.wait(lock, [&] { return should_unregister_thread_2; });
    }
    flusher.unregister_current_thread();
    {
      std::unique_lock<mutex> lock(test_mutex);
      unregistered_thread_2 = true;
      cond.notify_all();
      cond.wait(lock, [&] { return finished_test; });
    }
  });

  {
    std::unique_lock<mutex> lock(test_mutex);
    cond.wait(lock, [&] { return registered_thread_2; });
  }

  ASSERT_THAT(backend.thread_indexes(), ElementsAre(1, 2));
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 0);
  EXPECT_EQ(backend.thread_states[2].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[2].end_calls, 0);

  flusher.unregister_current_thread();

  EXPECT_THAT(backend.thread_indexes(), ElementsAre(1, 2));
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 1);
  EXPECT_EQ(backend.thread_states[2].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[2].end_calls, 0);

  {
    std::unique_lock<mutex> lock(test_mutex);
    should_unregister_thread_2 = true;
    cond.notify_all();
    cond.wait(lock, [&] { return unregistered_thread_2; });
  }

  EXPECT_THAT(backend.thread_indexes(), ElementsAre(1, 2));
  EXPECT_EQ(backend.thread_states[1].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[1].end_calls, 1);
  EXPECT_EQ(backend.thread_states[2].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[2].end_calls, 1);

  {
    std::lock_guard<mutex> lock(test_mutex);
    finished_test = true;
    cond.notify_all();
  }
  thread_2.join();

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, write_events_from_multiple_threads) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

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

  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(::testing::_, "main thread"));
  EXPECT_THAT(backend.read_thread_init_versions(2),
              ElementsAre(::testing::_, "other thread"));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher,
       stream_contains_thread_id_if_registered_after_enabling) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

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
  backend.read_thread_trace_stream(1, main_v);

  ASSERT_TRUE(other_thread_id.has_value());
  EXPECT_NE(*other_thread_id, main_thread_id);
  nice_mock_trace_stream_event_visitor other_v;
  EXPECT_CALL(other_v,
              visit_packet_header(::testing::Field(
                  &trace_stream_event_visitor::packet_header::thread_id,
                  *other_thread_id)));
  backend.read_thread_trace_stream(2, other_v);

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher,
       stream_file_contains_thread_id_if_enabled_after_threads_register) {
  mutex test_mutex;
  condition_variable cond;
  bool other_thread_registered = false;
  bool flusher_enabled = false;

  std::optional<std::uint64_t> other_thread_id;
  thread other_thread([&] {
    other_thread_id = get_current_thread_id();

    flusher.register_current_thread();

    // After the main thread enables the directory backend, flush.
    {
      std::unique_lock<mutex> lock(test_mutex);
      other_thread_registered = true;
      cond.notify_all();
      cond.wait(lock, [&] { return flusher_enabled; });
    }
    flusher.flush_sync();

    flusher.unregister_current_thread();
  });

  // After the other thread registers itself, enable the directory backend.
  {
    std::unique_lock<mutex> lock(test_mutex);
    cond.wait(lock, [&] { return other_thread_registered; });
  }
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);
  {
    std::unique_lock<mutex> lock(test_mutex);
    flusher_enabled = true;
    cond.notify_all();
  }

  // Wait for the other thread to flush.
  other_thread.join();

  ASSERT_TRUE(other_thread_id.has_value());
  EXPECT_NE(*other_thread_id, get_current_thread_id());
  nice_mock_trace_stream_event_visitor other_v;
  EXPECT_CALL(other_v,
              visit_packet_header(::testing::Field(
                  &trace_stream_event_visitor::packet_header::thread_id,
                  *other_thread_id)));
  backend.read_thread_trace_stream(1, other_v);

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, unregistering_thread_flushes_committed_data) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  flusher.register_current_thread();

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();

  flusher.unregister_current_thread();
  // NOTE(strager): We do not call flusher.flush_sync.

  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, flush_async_does_not_flush_on_current_thread) {
  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  flusher.register_current_thread();
  flusher.flush_sync();  // Write the normal init event.

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(trace_event_init{
      .version = u8"testing",
  });
  writer->commit();
  flusher.flush_async();  // Flush the testing init event, but not now.

  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(::testing::Not("testing")))
      << "creating the stream file should add an init event automatically (but "
         "not the testing init event)";

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, flush_async_flushes_on_flusher_thread) {
  flusher.start_flushing_thread();

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

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
  while (backend.read_thread_init_versions(1).size() <= 1) {
    if (std::chrono::steady_clock::now() >= deadline) {
      ADD_FAILURE() << "timed out waiting for flusher thread to write to file";
      break;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }
  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, flushing_disabled_does_nothing) {
  flusher.register_current_thread();

  // This should do nothing. In particular, it should not prevent the
  // stream header or init event from being written.
  flusher.flush_sync();

  spy_trace_flusher_backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.read_thread_init_versions(1),
              ElementsAre(QUICK_LINT_JS_VERSION_STRING));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher,
       write_to_multiple_backends_at_once_enabling_and_disabling) {
  flusher.register_current_thread();

  spy_trace_flusher_backend backend_1;
  flusher.enable_backend(&backend_1);

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);

  writer->write_event_init(trace_event_init{
      .version = u8"A: backend 1",
  });
  writer->commit();
  flusher.flush_sync();

  spy_trace_flusher_backend backend_2;
  flusher.enable_backend(&backend_2);

  writer->write_event_init(trace_event_init{
      .version = u8"B: backend 1 and backend 2",
  });
  writer->commit();
  flusher.flush_sync();

  // Leave only backend_2 enabled.
  flusher.disable_backend(&backend_1);

  writer->write_event_init(trace_event_init{
      .version = u8"C: backend 2",
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(
      backend_1.read_thread_init_versions(1),
      ElementsAre(::testing::_, "A: backend 1", "B: backend 1 and backend 2"));
  EXPECT_THAT(
      backend_2.read_thread_init_versions(1),
      ElementsAre(::testing::_, "B: backend 1 and backend 2", "C: backend 2"));

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, broadcast_to_many_backends_at_once) {
  flusher.register_current_thread();

  std::array<spy_trace_flusher_backend, 5> backends;
  for (spy_trace_flusher_backend& backend : backends) {
    flusher.enable_backend(&backend);
  }

  trace_writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);

  writer->write_event_init(trace_event_init{
      .version = u8"broadcast",
  });
  writer->commit();
  flusher.flush_sync();

  for (spy_trace_flusher_backend& backend : backends) {
    EXPECT_THAT(backend.read_thread_init_versions(1),
                ElementsAre(::testing::_, "broadcast"));
  }

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher, enable_and_disable_and_reenable_multiple_backends) {
  flusher.register_current_thread();

  spy_trace_flusher_backend backend_1;
  spy_trace_flusher_backend backend_2;
  spy_trace_flusher_backend backend_3;
  flusher.enable_backend(&backend_1);
  EXPECT_TRUE(flusher.is_enabled()) << "1";
  flusher.enable_backend(&backend_2);
  EXPECT_TRUE(flusher.is_enabled()) << "1 and 2";
  flusher.disable_backend(&backend_1);
  EXPECT_TRUE(flusher.is_enabled()) << "2";
  flusher.enable_backend(&backend_3);
  EXPECT_TRUE(flusher.is_enabled()) << "2 and 3";
  flusher.disable_backend(&backend_2);
  EXPECT_TRUE(flusher.is_enabled()) << "3";
  backend_1.reset();
  flusher.enable_backend(&backend_1);
  EXPECT_TRUE(flusher.is_enabled()) << "1 and 3";

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher_directory_backend,
       initing_directory_backend_creates_metadata_file) {
  auto backend = trace_flusher_directory_backend::init_directory(trace_dir);
  ASSERT_TRUE(backend.ok()) << backend.error_to_string();

  auto metadata_file = read_file((trace_dir + "/metadata").c_str());
  ASSERT_TRUE(metadata_file.ok()) << metadata_file.error_to_string();

  EXPECT_THAT(to_string(metadata_file->string_view()),
              ::testing::StartsWith("/* CTF 1.8"))
      << "TSDL specification: https://diamon.org/ctf/#spec7.1";
}

TEST_F(test_trace_flusher_directory_backend,
       initing_directory_backend_fails_if_directory_is_missing) {
  auto backend = trace_flusher_directory_backend::init_directory(
      trace_dir + "/does-not-exit");
  EXPECT_FALSE(backend.ok());
}

TEST_F(test_trace_flusher_directory_backend,
       enabling_and_registering_writes_stream_file_header) {
  trace_flusher flusher;

  flusher.register_current_thread();

  auto backend =
      trace_flusher_directory_backend::init_directory(this->trace_dir);
  ASSERT_TRUE(backend.ok()) << backend.error_to_string();
  flusher.enable_backend(&*backend);

  nice_mock_trace_stream_event_visitor v;
  EXPECT_CALL(v, visit_packet_header(::testing::Field(
                     &trace_stream_event_visitor::packet_header::thread_id,
                     get_current_thread_id())));
  EXPECT_CALL(v, visit_init_event(::testing::Field(
                     &trace_stream_event_visitor::init_event::version,
                     ::testing::StrEq(QUICK_LINT_JS_VERSION_STRING))));
  EXPECT_CALL(v, visit_process_id_event(::testing::Field(
                     &trace_stream_event_visitor::process_id_event::process_id,
                     get_current_process_id())));
  read_trace_stream_file(this->trace_dir + "/thread1", v);

  flusher.disable_all_backends();
}

TEST_F(test_trace_flusher_directory_backend,
       write_events_from_multiple_threads) {
  trace_flusher flusher;

  auto backend =
      trace_flusher_directory_backend::init_directory(this->trace_dir);
  ASSERT_TRUE(backend.ok()) << backend.error_to_string();
  flusher.enable_backend(&*backend);

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

  flusher.disable_all_backends();
}
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
