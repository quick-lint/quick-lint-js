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
#include <quick-lint-js/logging/trace-reader.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/process.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/version.h>
#include <string>
#include <thread>

QLJS_WARNING_IGNORE_GCC("-Wmissing-field-initializers")

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
class Test_Trace_Flusher : public ::testing::Test {
 protected:
  void TearDown() override {
    flusher.unregister_all_threads();
    flusher.stop_flushing_thread();
  }

  Trace_Flusher& flusher = *Trace_Flusher::instance();
};

class Test_Trace_Flusher_Directory_Backend : public Test_Trace_Flusher,
                                             public Filesystem_Test {
 protected:
  std::string trace_dir = this->make_temporary_directory();
};

std::vector<Parsed_Trace_Event> read_trace_stream_file(
    const std::string& path) {
  auto stream_file = read_file(path);
  if (!stream_file.ok()) {
    ADD_FAILURE() << stream_file.error_to_string();
    return std::vector<Parsed_Trace_Event>();
  }
  Trace_Reader reader;
  reader.append_bytes(stream_file->data(),
                      narrow_cast<std::size_t>(stream_file->size()));
  return reader.pull_new_events();
}

std::vector<std::string> get_init_versions(
    const std::vector<Parsed_Trace_Event>& events) {
  std::vector<std::string> init_versions;
  for (const Parsed_Trace_Event& event : events) {
    if (event.type == Parsed_Trace_Event_Type::init_event) {
      init_versions.push_back(
          std::string(to_string_view(event.init_event.version)));
    }
  }
  return init_versions;
}

std::vector<std::string> read_init_versions(const std::string& stream_path) {
  return get_init_versions(read_trace_stream_file(stream_path));
}

class Spy_Trace_Flusher_Backend final : public Trace_Flusher_Backend {
 public:
  void trace_thread_begin(Trace_Flusher_Thread_Index thread_index) override {
    std::lock_guard<Mutex> lock(this->mutex_);

    Thread_State& t = this->thread_states[thread_index];
    EXPECT_EQ(t.begin_calls, 0);
    EXPECT_EQ(t.write_calls, 0);
    EXPECT_EQ(t.end_calls, 0);

    t.begin_calls += 1;
  }

  void trace_thread_end(Trace_Flusher_Thread_Index thread_index) override {
    std::lock_guard<Mutex> lock(this->mutex_);

    Thread_State& t = this->thread_states[thread_index];
    EXPECT_EQ(t.begin_calls, 1);
    EXPECT_EQ(t.end_calls, 0);

    t.end_calls += 1;
  }

  void trace_thread_write_data(Trace_Flusher_Thread_Index thread_index,
                               Span<const std::byte> data) override {
    std::lock_guard<Mutex> lock(this->mutex_);

    Thread_State& t = this->thread_states[thread_index];
    EXPECT_GE(t.begin_calls, 1);
    EXPECT_EQ(t.end_calls, 0);

    t.written_data.append(reinterpret_cast<const char*>(data.data()),
                          narrow_cast<std::size_t>(data.size()));
    t.write_calls += 1;
  }

  std::vector<Trace_Flusher_Thread_Index> thread_indexes() const {
    std::lock_guard<Mutex> lock(this->mutex_);
    std::vector<Trace_Flusher_Thread_Index> result;
    for (auto& [thread_index, _t] : this->thread_states) {
      result.push_back(thread_index);
    }
    return result;
  }

  std::vector<Parsed_Trace_Event> parse_thread_trace_stream(
      Trace_Flusher_Thread_Index thread_index) const {
    std::lock_guard<Mutex> lock(this->mutex_);
    auto it = this->thread_states.find(thread_index);
    if (it == this->thread_states.end()) {
      ADD_FAILURE() << "thread index " << thread_index << " is missing";
      return std::vector<Parsed_Trace_Event>();
    }
    const Thread_State& t = it->second;
    Trace_Reader reader;
    reader.append_bytes(t.written_data.data(),
                        narrow_cast<std::size_t>(t.written_data.size()));
    return reader.pull_new_events();
  }

  std::vector<std::string> get_thread_init_versions(
      Trace_Flusher_Thread_Index thread_index) const {
    return get_init_versions(this->parse_thread_trace_stream(thread_index));
  }

  struct Thread_State {
    int begin_calls = 0;
    int end_calls = 0;
    int write_calls = 0;
    std::string written_data;
  };

  void reset() {
    std::lock_guard<Mutex> lock(this->mutex_);
    this->thread_states.clear();
  }

  std::map<Trace_Flusher_Thread_Index, Thread_State> thread_states;
  mutable Mutex mutex_;
};

TEST_F(Test_Trace_Flusher, enabling_enables) {
  EXPECT_FALSE(flusher.is_enabled());

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  EXPECT_TRUE(flusher.is_enabled());

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher,
       enabling_with_no_threads_registered_begins_no_threads) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.thread_states, IsEmpty());

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher,
       registering_then_unregistering_then_enabling_begins_no_threads) {
  flusher.register_current_thread();
  flusher.unregister_current_thread();

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.thread_indexes(), IsEmpty());

  flusher.disable_all_backends();
}

TEST_F(
    Test_Trace_Flusher,
    enabling_then_registering_then_unregistering_then_registering_begins_thread_again_with_different_index) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();
  EXPECT_EQ(backend.thread_states[thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[thread_index].end_calls, 0);

  flusher.unregister_current_thread();
  EXPECT_EQ(backend.thread_states[thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[thread_index].end_calls, 1);

  Trace_Flusher_Thread_Index new_thread_index =
      flusher.register_current_thread();
  EXPECT_EQ(backend.thread_states[thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[thread_index].end_calls, 1);
  EXPECT_NE(new_thread_index, thread_index)
      << "re-registering the thread should have created a second, different "
         "thread index";
  EXPECT_EQ(backend.thread_states[new_thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[new_thread_index].end_calls, 0);

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, enabling_after_register_begins_thread) {
  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.thread_indexes(), ElementsAreArray({thread_index}));
  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAreArray({QUICK_LINT_JS_VERSION_STRING}));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, registering_after_enabling_begins_thread) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  EXPECT_THAT(backend.thread_indexes(), ElementsAreArray({thread_index}));
  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAreArray({QUICK_LINT_JS_VERSION_STRING}));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher,
       registering_after_enabling_begins_thread_on_all_backends) {
  Spy_Trace_Flusher_Backend backend_1;
  flusher.enable_backend(&backend_1);
  Spy_Trace_Flusher_Backend backend_2;
  flusher.enable_backend(&backend_2);

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  EXPECT_THAT(backend_1.get_thread_init_versions(thread_index),
              ElementsAreArray({QUICK_LINT_JS_VERSION_STRING}));
  EXPECT_THAT(backend_2.get_thread_init_versions(thread_index),
              ElementsAreArray({QUICK_LINT_JS_VERSION_STRING}));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, write_event_after_enabling_and_registering) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(Trace_Event_Init{
      .version = u8"testing"_sv,
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, write_event_after_registering_and_enabling) {
  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(Trace_Event_Init{
      .version = u8"testing"_sv,
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, cannot_write_events_before_enabling) {
  flusher.register_current_thread();

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);
}

TEST_F(Test_Trace_Flusher, cannot_write_events_before_registering) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, cannot_write_events_after_unregistering) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  flusher.register_current_thread();
  flusher.unregister_current_thread();

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, cannot_write_events_after_enabling_then_disabling) {
  flusher.register_current_thread();

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  flusher.disable_backend(&backend);

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  EXPECT_FALSE(writer);

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, disabling_disables) {
  Spy_Trace_Flusher_Backend backend;

  flusher.enable_backend(&backend);
  ASSERT_TRUE(flusher.is_enabled());

  flusher.disable_backend(&backend);
  EXPECT_FALSE(flusher.is_enabled());

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher,
       can_write_events_after_enabling_then_disabling_then_enabling_again) {
  Spy_Trace_Flusher_Backend backend_1;
  Spy_Trace_Flusher_Backend backend_2;

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  flusher.enable_backend(&backend_1);
  flusher.disable_backend(&backend_1);
  flusher.enable_backend(&backend_2);

  EXPECT_TRUE(flusher.trace_writer_for_current_thread());
  EXPECT_THAT(backend_1.thread_indexes(), ElementsAreArray({thread_index}));
  EXPECT_THAT(backend_2.thread_indexes(), ElementsAreArray({thread_index}));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, disabling_backend_ends_all_registered_threads) {
  Trace_Flusher_Thread_Index main_thread_index =
      flusher.register_current_thread();

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Mutex test_mutex;
  Condition_Variable cond;
  std::optional<Trace_Flusher_Thread_Index> thread_2_index;
  bool finished_test = false;

  std::thread thread_2([&]() {
    {
      std::lock_guard<Mutex> lock(test_mutex);
      thread_2_index = flusher.register_current_thread();
      cond.notify_all();
    }

    {
      std::unique_lock<Mutex> lock(test_mutex);
      cond.wait(lock, [&] { return finished_test; });
    }
    flusher.unregister_current_thread();
  });

  {
    std::unique_lock<Mutex> lock(test_mutex);
    cond.wait(lock, [&] { return thread_2_index.has_value(); });
  }

  ASSERT_THAT(backend.thread_indexes(), ElementsAreArray({
                                            main_thread_index,
                                            *thread_2_index,
                                        }));
  EXPECT_EQ(backend.thread_states[main_thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[main_thread_index].end_calls, 0);
  EXPECT_EQ(backend.thread_states[*thread_2_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].end_calls, 0);

  flusher.disable_backend(&backend);

  EXPECT_THAT(backend.thread_indexes(), ElementsAreArray({
                                            main_thread_index,
                                            *thread_2_index,
                                        }));
  EXPECT_EQ(backend.thread_states[main_thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[main_thread_index].end_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].end_calls, 1);

  {
    std::lock_guard<Mutex> lock(test_mutex);
    finished_test = true;
    cond.notify_all();
  }
  thread_2.join();

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, unregistering_thread_calls_thread_end) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Mutex test_mutex;
  Condition_Variable cond;
  std::optional<Trace_Flusher_Thread_Index> thread_2_index;
  bool should_unregister_thread_2 = false;
  bool unregistered_thread_2 = false;
  bool finished_test = false;

  Trace_Flusher_Thread_Index main_thread_index =
      flusher.register_current_thread();

  std::thread thread_2([&]() {
    {
      std::unique_lock<Mutex> lock(test_mutex);
      thread_2_index = flusher.register_current_thread();
      cond.notify_all();
      cond.wait(lock, [&] { return should_unregister_thread_2; });
    }
    flusher.unregister_current_thread();
    {
      std::unique_lock<Mutex> lock(test_mutex);
      unregistered_thread_2 = true;
      cond.notify_all();
      cond.wait(lock, [&] { return finished_test; });
    }
  });

  {
    std::unique_lock<Mutex> lock(test_mutex);
    cond.wait(lock, [&] { return thread_2_index.has_value(); });
  }

  ASSERT_THAT(backend.thread_indexes(), ElementsAreArray({
                                            main_thread_index,
                                            *thread_2_index,
                                        }));
  EXPECT_EQ(backend.thread_states[main_thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[main_thread_index].end_calls, 0);
  EXPECT_EQ(backend.thread_states[*thread_2_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].end_calls, 0);

  flusher.unregister_current_thread();

  EXPECT_THAT(backend.thread_indexes(), ElementsAreArray({
                                            main_thread_index,
                                            *thread_2_index,
                                        }));
  EXPECT_EQ(backend.thread_states[main_thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[main_thread_index].end_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].end_calls, 0);

  {
    std::unique_lock<Mutex> lock(test_mutex);
    should_unregister_thread_2 = true;
    cond.notify_all();
    cond.wait(lock, [&] { return unregistered_thread_2; });
  }

  EXPECT_THAT(backend.thread_indexes(), ElementsAreArray({
                                            main_thread_index,
                                            *thread_2_index,
                                        }));
  EXPECT_EQ(backend.thread_states[main_thread_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[main_thread_index].end_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].begin_calls, 1);
  EXPECT_EQ(backend.thread_states[*thread_2_index].end_calls, 1);

  {
    std::lock_guard<Mutex> lock(test_mutex);
    finished_test = true;
    cond.notify_all();
  }
  thread_2.join();

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, write_events_from_multiple_threads) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Flusher_Thread_Index main_thread_index =
      flusher.register_current_thread();

  Trace_Flusher_Thread_Index other_thread_index;
  Thread other_thread([&]() {
    other_thread_index = flusher.register_current_thread();

    Trace_Writer* writer = flusher.trace_writer_for_current_thread();
    ASSERT_TRUE(writer);
    writer->write_event_init(Trace_Event_Init{
        .version = u8"other thread"_sv,
    });
    writer->commit();

    flusher.unregister_current_thread();
  });

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(Trace_Event_Init{
      .version = u8"main thread"_sv,
  });
  writer->commit();

  other_thread.join();
  flusher.flush_sync();

  EXPECT_THAT(backend.get_thread_init_versions(main_thread_index),
              ElementsAre(::testing::_, "main thread"));
  EXPECT_THAT(backend.get_thread_init_versions(other_thread_index),
              ElementsAre(::testing::_, "other thread"));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher,
       stream_contains_thread_id_if_registered_after_enabling) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  std::uint64_t main_thread_id = get_current_thread_id();
  std::optional<std::uint64_t> other_thread_id;
  Trace_Flusher_Thread_Index main_thread_index =
      flusher.register_current_thread();
  Trace_Flusher_Thread_Index other_thread_index;
  Thread other_thread([&] {
    other_thread_index = flusher.register_current_thread();
    flusher.flush_sync();
    flusher.unregister_current_thread();
    other_thread_id = get_current_thread_id();
  });
  other_thread.join();
  flusher.flush_sync();

  std::vector<Parsed_Trace_Event> main_thread_events =
      backend.parse_thread_trace_stream(main_thread_index);
  ASSERT_GE(main_thread_events.size(), 1);
  EXPECT_EQ(main_thread_events[0].type, Parsed_Trace_Event_Type::packet_header);
  EXPECT_EQ(main_thread_events[0].packet_header.thread_id, main_thread_id);

  ASSERT_TRUE(other_thread_id.has_value());
  EXPECT_NE(*other_thread_id, main_thread_id);
  std::vector<Parsed_Trace_Event> other_thread_events =
      backend.parse_thread_trace_stream(other_thread_index);
  ASSERT_GE(other_thread_events.size(), 1);
  EXPECT_EQ(other_thread_events[0].type,
            Parsed_Trace_Event_Type::packet_header);
  EXPECT_EQ(other_thread_events[0].packet_header.thread_id, *other_thread_id);

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher,
       stream_file_contains_thread_id_if_enabled_after_threads_register) {
  Mutex test_mutex;
  Condition_Variable cond;
  std::optional<Trace_Flusher_Thread_Index> other_thread_index;
  bool flusher_enabled = false;

  std::optional<std::uint64_t> other_thread_id;
  Thread other_thread([&] {
    other_thread_id = get_current_thread_id();

    {
      std::unique_lock<Mutex> lock(test_mutex);
      other_thread_index = flusher.register_current_thread();
      cond.notify_all();
      // After the main thread enables the directory backend, flush.
      cond.wait(lock, [&] { return flusher_enabled; });
    }
    flusher.flush_sync();

    flusher.unregister_current_thread();
  });

  // After the other thread registers itself, enable the directory backend.
  {
    std::unique_lock<Mutex> lock(test_mutex);
    cond.wait(lock, [&] { return other_thread_index.has_value(); });
  }
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);
  {
    std::unique_lock<Mutex> lock(test_mutex);
    flusher_enabled = true;
    cond.notify_all();
  }

  // Wait for the other thread to flush.
  other_thread.join();

  ASSERT_TRUE(other_thread_id.has_value());
  EXPECT_NE(*other_thread_id, get_current_thread_id());
  std::vector<Parsed_Trace_Event> other_thread_events =
      backend.parse_thread_trace_stream(*other_thread_index);
  ASSERT_GE(other_thread_events.size(), 1);
  EXPECT_EQ(other_thread_events[0].type,
            Parsed_Trace_Event_Type::packet_header);
  EXPECT_EQ(other_thread_events[0].packet_header.thread_id, *other_thread_id);

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, unregistering_thread_flushes_committed_data) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(Trace_Event_Init{
      .version = u8"testing"_sv,
  });
  writer->commit();

  flusher.unregister_current_thread();
  // NOTE(strager): We do not call flusher.flush_sync.

  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, flush_async_does_not_flush_on_current_thread) {
  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();
  flusher.flush_sync();  // Write the normal init event.

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(Trace_Event_Init{
      .version = u8"testing"_sv,
  });
  writer->commit();
  flusher.flush_async();  // Flush the testing init event, but not now.

  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAreArray({::testing::Not("testing")}))
      << "creating the stream file should add an init event automatically (but "
         "not the testing init event)";

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, flush_async_flushes_on_flusher_thread) {
  flusher.start_flushing_thread();

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();
  flusher.flush_sync();  // Write the normal init event.

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(Trace_Event_Init{
      .version = u8"testing"_sv,
  });
  writer->commit();
  flusher
      .flush_async();  // Flush the testing init event, but not on this thread.

  std::chrono::time_point deadline =
      std::chrono::steady_clock::now() + std::chrono::seconds(3);
  while (backend.get_thread_init_versions(thread_index).size() <= 1) {
    if (std::chrono::steady_clock::now() >= deadline) {
      ADD_FAILURE() << "timed out waiting for flusher thread to write to file";
      break;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }
  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAre(::testing::_, "testing"));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, can_stop_and_restart_flusher_thread) {
  flusher.start_flushing_thread();
  flusher.stop_flushing_thread();
  flusher.start_flushing_thread();
  // TODO(strager): Assert that the flushing thread actually works.
}

TEST_F(Test_Trace_Flusher, flushing_disabled_does_nothing) {
  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  // This should do nothing. In particular, it should not prevent the
  // stream header or init event from being written.
  flusher.flush_sync();

  Spy_Trace_Flusher_Backend backend;
  flusher.enable_backend(&backend);

  EXPECT_THAT(backend.get_thread_init_versions(thread_index),
              ElementsAreArray({QUICK_LINT_JS_VERSION_STRING}));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher,
       write_to_multiple_backends_at_once_enabling_and_disabling) {
  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  Spy_Trace_Flusher_Backend backend_1;
  flusher.enable_backend(&backend_1);

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);

  writer->write_event_init(Trace_Event_Init{
      .version = u8"A: backend 1"_sv,
  });
  writer->commit();
  flusher.flush_sync();

  Spy_Trace_Flusher_Backend backend_2;
  flusher.enable_backend(&backend_2);

  writer->write_event_init(Trace_Event_Init{
      .version = u8"B: backend 1 and backend 2"_sv,
  });
  writer->commit();
  flusher.flush_sync();

  // Leave only backend_2 enabled.
  flusher.disable_backend(&backend_1);

  writer->write_event_init(Trace_Event_Init{
      .version = u8"C: backend 2"_sv,
  });
  writer->commit();
  flusher.flush_sync();

  EXPECT_THAT(
      backend_1.get_thread_init_versions(thread_index),
      ElementsAre(::testing::_, "A: backend 1", "B: backend 1 and backend 2"));
  EXPECT_THAT(
      backend_2.get_thread_init_versions(thread_index),
      ElementsAre(::testing::_, "B: backend 1 and backend 2", "C: backend 2"));

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, broadcast_to_many_backends_at_once) {
  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  std::array<Spy_Trace_Flusher_Backend, 5> backends;
  for (Spy_Trace_Flusher_Backend& backend : backends) {
    flusher.enable_backend(&backend);
  }

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);

  writer->write_event_init(Trace_Event_Init{
      .version = u8"broadcast"_sv,
  });
  writer->commit();
  flusher.flush_sync();

  for (Spy_Trace_Flusher_Backend& backend : backends) {
    EXPECT_THAT(backend.get_thread_init_versions(thread_index),
                ElementsAre(::testing::_, "broadcast"));
  }

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher, enable_and_disable_and_reenable_multiple_backends) {
  flusher.register_current_thread();

  Spy_Trace_Flusher_Backend backend_1;
  Spy_Trace_Flusher_Backend backend_2;
  Spy_Trace_Flusher_Backend backend_3;
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

TEST_F(Test_Trace_Flusher_Directory_Backend,
       initing_directory_backend_creates_metadata_file) {
  auto backend = Trace_Flusher_Directory_Backend::init_directory(trace_dir);
  ASSERT_TRUE(backend.ok()) << backend.error_to_string();

  auto metadata_file = read_file((trace_dir + "/metadata").c_str());
  ASSERT_TRUE(metadata_file.ok()) << metadata_file.error_to_string();

  EXPECT_THAT(to_string(metadata_file->string_view()),
              ::testing::StartsWith("/* CTF 1.8"))
      << "TSDL specification: https://diamon.org/ctf/#spec7.1";
}

TEST_F(Test_Trace_Flusher_Directory_Backend,
       initing_directory_backend_fails_if_directory_is_missing) {
  auto backend = Trace_Flusher_Directory_Backend::init_directory(
      trace_dir + "/does-not-exit");
  EXPECT_FALSE(backend.ok());
}

TEST_F(Test_Trace_Flusher_Directory_Backend,
       enabling_and_registering_writes_stream_file_header) {
  Trace_Flusher_Thread_Index thread_index = flusher.register_current_thread();

  auto backend =
      Trace_Flusher_Directory_Backend::init_directory(this->trace_dir);
  ASSERT_TRUE(backend.ok()) << backend.error_to_string();
  flusher.enable_backend(&*backend);

  std::vector<Parsed_Trace_Event> events = read_trace_stream_file(
      this->trace_dir + "/thread" + std::to_string(thread_index));
  EXPECT_EQ(events.size(), 3);
  EXPECT_EQ(events[0].type, Parsed_Trace_Event_Type::packet_header);
  EXPECT_EQ(events[0].packet_header.thread_id, get_current_thread_id());
  EXPECT_EQ(events[1].type, Parsed_Trace_Event_Type::init_event);
  EXPECT_EQ(events[1].init_event.version, QUICK_LINT_JS_VERSION_STRING_U8_SV);
  EXPECT_EQ(events[2].type, Parsed_Trace_Event_Type::process_id_event);
  EXPECT_EQ(events[2].process_id_event.process_id, get_current_process_id());

  flusher.disable_all_backends();
}

TEST_F(Test_Trace_Flusher_Directory_Backend,
       write_events_from_multiple_threads) {
  auto backend =
      Trace_Flusher_Directory_Backend::init_directory(this->trace_dir);
  ASSERT_TRUE(backend.ok()) << backend.error_to_string();
  flusher.enable_backend(&*backend);

  Trace_Flusher_Thread_Index main_thread_index =
      flusher.register_current_thread();

  Trace_Flusher_Thread_Index other_thread_index;
  Thread other_thread([&]() {
    other_thread_index = flusher.register_current_thread();

    Trace_Writer* writer = flusher.trace_writer_for_current_thread();
    ASSERT_TRUE(writer);
    writer->write_event_init(Trace_Event_Init{
        .version = u8"other thread"_sv,
    });
    writer->commit();

    flusher.unregister_current_thread();
  });

  Trace_Writer* writer = flusher.trace_writer_for_current_thread();
  ASSERT_TRUE(writer);
  writer->write_event_init(Trace_Event_Init{
      .version = u8"main thread"_sv,
  });
  writer->commit();

  other_thread.join();
  flusher.flush_sync();

  EXPECT_THAT(read_init_versions(this->trace_dir + "/thread" +
                                 std::to_string(main_thread_index)),
              ElementsAre(::testing::_, "main thread"));
  EXPECT_THAT(read_init_versions(this->trace_dir + "/thread" +
                                 std::to_string(other_thread_index)),
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
