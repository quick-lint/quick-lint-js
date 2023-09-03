// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No event loops on the web.
#else

#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <memory>
#include <quick-lint-js/container/fixed-vector.h>
#include <quick-lint-js/io/event-loop.h>
#include <quick-lint-js/io/pipe.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/crash.h>
#include <quick-lint-js/port/have.h>
#include <quick-lint-js/port/kqueue.h>
#include <quick-lint-js/port/thread.h>
#include <quick-lint-js/util/synchronized.h>
#include <thread>
#include <vector>

#if QLJS_HAVE_POLL
#include <poll.h>
#endif

#if defined(_WIN32)
#include <WinSock2.h>
#include <mswsock.h>
#include <quick-lint-js/port/windows-error.h>
#endif

using ::testing::ElementsAreArray;
using namespace std::literals::chrono_literals;

namespace quick_lint_js {
namespace {
void write_full_message(Platform_File_Ref, String8_View);

// Methods fail the test and stop the event loop by default.
struct Failing_Event_Loop_Pipe_Read_Delegate
    : public Event_Loop_Pipe_Read_Delegate {
  void on_pipe_read_data(Event_Loop_Base*, Platform_File_Ref,
                         String8_View data) override {
    ADD_FAILURE() << "pipe should not receive data; got \"" << out_string8(data)
                  << "\"";
  }

  void on_pipe_read_end(Event_Loop_Base*, Platform_File_Ref) override {
    ADD_FAILURE() << "pipe should not receive read-end-of-file";
  }

  void on_pipe_read_error(Event_Loop_Base*, Platform_File_Ref,
                          Platform_File_IO_Error error) override {
    ADD_FAILURE() << "pipe should not receive a read error; got "
                  << error.to_string();
  }
};

struct Failing_Event_Loop_Pipe_Write_Delegate
    : public Event_Loop_Pipe_Write_Delegate {
  void on_pipe_write_ready(Event_Loop_Base*, Platform_File_Ref) override {
    ADD_FAILURE() << "pipe should not be write-ready";
  }

  void on_pipe_write_end(Event_Loop_Base*, Platform_File_Ref) override {
    ADD_FAILURE() << "pipe should not receive write-end-of-file";
  }
};

#if defined(_WIN32)
class Failing_Event_Loop_Custom_Windows_IO_Completion_Delegate
    : public Event_Loop_Custom_Windows_IO_Completion_Delegate {
  void on_custom_windows_io_completion(Event_Loop_Base*,
                                       ::ULONG_PTR completion_key, ::DWORD,
                                       ::OVERLAPPED*) override {
    ADD_FAILURE() << "unexpected I/O completion; completion_key="
                  << completion_key;
  }

  void on_custom_windows_io_completion_error(Event_Loop_Base*,
                                             ::ULONG_PTR completion_key,
                                             ::DWORD error, ::DWORD,
                                             ::OVERLAPPED*) override {
    ADD_FAILURE() << "unexpected I/O error; completion_key=" << completion_key
                  << "; error=" << windows_error_message(error);
  }
};
#endif

#if QLJS_HAVE_KQUEUE
void kqueue_add_changes(POSIX_FD_File_Ref kqueue_fd,
                        Span<const struct ::kevent> changes) {
  int kevent_rc = ::kevent(kqueue_fd.get(),
                           /*changelist=*/changes.data(),
                           /*nchanges=*/narrow_cast<int>(changes.size()),
                           /*eventlist=*/nullptr,
                           /*nevents=*/0,
                           /*timeout=*/nullptr);
  ASSERT_NE(kevent_rc, -1) << std::strerror(errno);
  ASSERT_EQ(kevent_rc, 0);
}
#endif

struct Event_Loop_Factory {
  const char* name;
  std::shared_ptr<Event_Loop_Base> (*make)();

  static std::string get_name(
      const ::testing::TestParamInfo<Event_Loop_Factory>& info) {
    return info.param.name;
  }
};

class Test_Event_Loop : public ::testing::TestWithParam<Event_Loop_Factory> {
 public:
  std::shared_ptr<Event_Loop_Base> loop = this->make_event_loop();

 private:
  std::shared_ptr<Event_Loop_Base> make_event_loop() {
    const Event_Loop_Factory& factory = this->GetParam();
    return factory.make();
  }
};

TEST_P(Test_Event_Loop, event_loop_stops_with_nothing_added) {
  this->loop->run();
  // The event loop should terminate.
}

TEST_P(Test_Event_Loop,
       event_loop_stops_with_nothing_added_with_keep_alive_and_un_keep_alive) {
  this->loop->keep_alive();
  this->loop->un_keep_alive();
  this->loop->run();
  // The event loop should terminate.
}

TEST_P(Test_Event_Loop, event_loop_can_be_stopped_by_another_thread_SLOW) {
  this->loop->keep_alive();

  std::atomic<bool> stop_thread_requested_stop = false;
  Thread stop_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    stop_thread_requested_stop = true;
    this->loop->un_keep_alive();
  });

  this->loop->run();
  // The event loop should terminate.

  EXPECT_TRUE(stop_thread_requested_stop);
  stop_thread.join();
}

TEST_P(Test_Event_Loop, pipe_read_callback_fires_if_data_was_already_ready) {
  Pipe_FDs pipe = make_pipe();
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  pipe.reader.set_pipe_non_blocking();
#endif
  write_full_message(pipe.writer.ref(), u8"hello"_sv);

  struct Reader : public Failing_Event_Loop_Pipe_Read_Delegate {
    void on_pipe_read_data(Event_Loop_Base* l, Platform_File_Ref,
                           String8_View data) override {
      this->read_calls.emplace_back(data);
      l->stop_event_loop_testing_only();
    }

    std::vector<String8> read_calls;
  };
  Reader r;
  this->loop->register_pipe_read(pipe.reader.ref(), &r);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_THAT(r.read_calls, ElementsAreArray({u8"hello"_sv}));
}

// See NOTE[Event_Loop-stop].
TEST_P(Test_Event_Loop,
       stopping_event_loop_prevents_callback_of_same_pipe_from_being_called) {
  // Cause on_pipe_read_data to be called once.
  Pipe_FDs pipe = make_pipe();
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  pipe.reader.set_pipe_non_blocking();
#endif
  write_full_message(pipe.writer.ref(), u8"x"_sv);

  struct Reader : public Failing_Event_Loop_Pipe_Read_Delegate {
    void on_pipe_read_data(Event_Loop_Base* l, Platform_File_Ref,
                           String8_View data) override {
      this->read_calls.emplace_back(data);

      // Request that the event loop is stopped.
      l->un_keep_alive();
      // If we did not request that the event loop be stopped, writing would
      // cause on_pipe_read_data to be called again. Because the event loop will
      // be stopped, however, on_pipe_read_data shouldn't be called again.
      write_full_message(this->pipe_writer, u8"y"_sv);
    }

    Platform_File_Ref pipe_reader;
    Platform_File_Ref pipe_writer;
    std::vector<String8> read_calls;
  };
  Reader r;
  r.pipe_reader = pipe.reader.ref();
  r.pipe_writer = pipe.writer.ref();
  this->loop->register_pipe_read(pipe.reader.ref(), &r);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_THAT(r.read_calls, ElementsAreArray({u8"x"_sv}))
      << "on_pipe_read_data should have been called once, then the event loop "
         "should have stopped immediately";
}

TEST_P(Test_Event_Loop,
       pipe_read_callback_fires_if_data_becomes_ready_asynchronously_SLOW) {
  Pipe_FDs pipe = make_pipe();
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  pipe.reader.set_pipe_non_blocking();
#endif

  std::atomic<bool> wrote_to_pipe = false;
  Thread writer_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    wrote_to_pipe = true;
    write_full_message(pipe.writer.ref(), u8"hello"_sv);
  });

  struct Reader : public Failing_Event_Loop_Pipe_Read_Delegate {
    void on_pipe_read_data(Event_Loop_Base* l, Platform_File_Ref,
                           String8_View data) override {
      this->read_calls.emplace_back(data);
      l->stop_event_loop_testing_only();
    }

    std::vector<String8> read_calls;
  };
  Reader r;
  this->loop->register_pipe_read(pipe.reader.ref(), &r);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_TRUE(wrote_to_pipe);
  EXPECT_THAT(r.read_calls, ElementsAreArray({u8"hello"_sv}));

  writer_thread.join();
}

TEST_P(Test_Event_Loop, pipe_read_callback_fires_if_end_of_file_on_start) {
  Pipe_FDs pipe = make_pipe();
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  pipe.reader.set_pipe_non_blocking();
#endif
  pipe.writer.close();

  struct Reader : public Failing_Event_Loop_Pipe_Read_Delegate {
    void on_pipe_read_end(Event_Loop_Base* l, Platform_File_Ref) override {
      this->end_call_count += 1;
      l->stop_event_loop_testing_only();
    }

    int end_call_count = 0;
  };
  Reader r;
  this->loop->register_pipe_read(pipe.reader.ref(), &r);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_EQ(r.end_call_count, 1);
}

TEST_P(Test_Event_Loop,
       pipe_read_callback_fires_on_asynchronous_end_of_file_SLOW) {
  Pipe_FDs pipe = make_pipe();
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  pipe.reader.set_pipe_non_blocking();
#endif

  std::atomic<bool> closed_pipe = false;
  Thread writer_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    closed_pipe = true;
    pipe.writer.close();
  });

  struct Reader : public Failing_Event_Loop_Pipe_Read_Delegate {
    void on_pipe_read_end(Event_Loop_Base* l, Platform_File_Ref) override {
      this->end_call_count += 1;
      l->stop_event_loop_testing_only();
    }

    int end_call_count = 0;
  };
  Reader r;
  this->loop->register_pipe_read(pipe.reader.ref(), &r);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_TRUE(closed_pipe);
  EXPECT_EQ(r.end_call_count, 1);

  writer_thread.join();
}

TEST_P(Test_Event_Loop, pipe_read_callback_is_not_called_before_run_SLOW) {
  Pipe_FDs pipe = make_pipe();
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  pipe.reader.set_pipe_non_blocking();
#endif
  // Cause on_pipe_read_end to be called.
  pipe.writer.close();

  struct Reader : public Failing_Event_Loop_Pipe_Read_Delegate {
    void on_pipe_read_end(Event_Loop_Base* l, Platform_File_Ref) override {
      this->on_pipe_read_call_count += 1;
      EXPECT_TRUE(this->run_called)
          << "callback should not be called before run()";
      l->stop_event_loop_testing_only();
    }

    std::atomic<int> on_pipe_read_call_count = false;
    std::atomic<bool> run_called = false;
  };
  Reader r;
  this->loop->register_pipe_read(pipe.reader.ref(), &r);
  this->loop->keep_alive();

  std::this_thread::sleep_for(10ms);
  EXPECT_EQ(r.on_pipe_read_call_count, 0)
      << "callback should not be called before run()";
  r.run_called = true;
  this->loop->run();
  EXPECT_EQ(r.on_pipe_read_call_count, 1)
      << "callback should have been called during run()";
}

TEST_P(
    Test_Event_Loop,
    registering_pipe_read_on_separate_thread_calls_callback_if_it_should_SLOW) {
  Pipe_FDs pipe = make_pipe();
#if QLJS_EVENT_LOOP2_READ_PIPE_NON_BLOCKING
  pipe.reader.set_pipe_non_blocking();
#endif

  struct Delegate : public Failing_Event_Loop_Pipe_Read_Delegate {
    void on_pipe_read_data(Event_Loop_Base* l, Platform_File_Ref,
                           String8_View data) override {
      EXPECT_TRUE(this->register_thread_registered)
          << "on_pipe_write_ready should be called after register_thread "
             "registers";
      this->read_calls.emplace_back(data);
      l->stop_event_loop_testing_only();
    }

    std::atomic<bool> register_thread_registered = false;
    std::vector<String8> read_calls;
  };
  Delegate delegate;
  this->loop->keep_alive();

  Thread register_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    delegate.register_thread_registered = true;
    this->loop->register_pipe_read(pipe.reader.ref(), &delegate);
    // Cause on_pipe_read_data to be called.
    write_full_message(pipe.writer.ref(), u8"x"_sv);
  });

  this->loop->run();

  EXPECT_THAT(delegate.read_calls, ElementsAreArray({u8"x"_sv}));
  EXPECT_TRUE(delegate.register_thread_registered);

  register_thread.join();
}

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(Test_Event_Loop,
       pipe_write_callback_fires_immediately_if_write_can_not_block) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  struct Writer : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_ready(Event_Loop_Base* l,
                             Platform_File_Ref file) override {
      this->ready_call_count += 1;

      Result<std::size_t, Platform_File_IO_Error> write_result =
          file.write(u8"done", 4);
      EXPECT_TRUE(write_result.ok()) << "write() should not fail with EAGAIN";
      if (write_result.ok()) {
        EXPECT_EQ(*write_result, 4);
      }

      l->stop_event_loop_testing_only();
    }

    int ready_call_count = 0;
  };
  Writer w;
  this->loop->register_pipe_write(pipe.writer.ref(), &w);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_EQ(w.ready_call_count, 1);
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(Test_Event_Loop, pipe_write_callback_fires_after_data_is_read_SLOW) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  // Fill the writer's buffer. A subsequent write will block or fail.
  String8 xs(pipe.writer.get_pipe_buffer_size(), u8'x');
  write_full_message(pipe.writer.ref(), xs);
  if (pipe.writer.is_pipe_non_blocking()) {
    // Make sure the buffer is indeed full.
    Result<std::size_t, Platform_File_IO_Error> write_result =
        pipe.writer.write(u8"done", 4);
    EXPECT_FALSE(write_result.ok())
        << "write() should fail after filling the buffer";
    if (!write_result.ok()) {
#if QLJS_HAVE_UNISTD_H
      EXPECT_THAT(write_result.error().error,
                  ::testing::AnyOf(EAGAIN, EWOULDBLOCK));
#endif
    }
  }

  static std::atomic<bool> read_from_pipe;
  read_from_pipe = false;
  Thread reader_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    read_from_pipe = true;
    // Read the data to unblock the writer.
    String8 buffer(xs.size(), '_');
    pipe.reader.read(buffer.data(), narrow_cast<int>(buffer.size()));
  });

  struct Writer : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_ready(Event_Loop_Base* l,
                             Platform_File_Ref file) override {
      EXPECT_TRUE(read_from_pipe) << "reader thread should have called read() "
                                     "before a write could be ready";
      this->ready_call_count += 1;

      Result<std::size_t, Platform_File_IO_Error> write_result =
          file.write(u8"done", 4);
      EXPECT_TRUE(write_result.ok());
      if (write_result.ok()) {
        EXPECT_EQ(*write_result, 4);
      }

      l->stop_event_loop_testing_only();
    }

    int ready_call_count = 0;
  };
  Writer w;
  this->loop->register_pipe_write(pipe.writer.ref(), &w);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_TRUE(read_from_pipe);
  EXPECT_EQ(w.ready_call_count, 1);

  reader_thread.join();
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(Test_Event_Loop,
       disabled_pipe_write_callback_does_not_fire_if_write_can_not_block_SLOW) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  struct Delegate : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_ready(Event_Loop_Base*, Platform_File_Ref) override {
      ADD_FAILURE()
          << "pipe should be writable but callback should not be called";
    }
  };
  Delegate delegate;
  this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->disable_pipe_write(pipe.writer.ref());

  Thread stop_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);
    // Stop the event loop.
    this->loop->un_keep_alive();
  });

  this->loop->run();

  stop_thread.join();
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(Test_Event_Loop,
       disabled_pipe_write_callback_does_not_fire_if_read_end_is_closed_SLOW) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  // Make on_pipe_write_end be called if the pipe_write would be enabled.
  pipe.reader.close();

  struct Delegate : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_end(Event_Loop_Base*, Platform_File_Ref) override {
      ADD_FAILURE() << "pipe should end but callback should not be called";
    }
  };
  Delegate delegate;
  this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->disable_pipe_write(pipe.writer.ref());

  Thread stop_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    this->loop->stop_event_loop_testing_only();
  });

  this->loop->run();

  stop_thread.join();
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(
    Test_Event_Loop,
    reenabling_pipe_write_during_loop_calls_callback_if_write_can_not_block) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  // Add an arbitrary event to the event loop so that we can run code when the
  // event loop is turning.
  Pipe_FDs start_pipe = make_pipe();
  start_pipe.reader.set_pipe_non_blocking();
  write_full_message(start_pipe.writer.ref(), u8"x"_sv);

  struct Delegate : public Failing_Event_Loop_Pipe_Read_Delegate,
                    public Failing_Event_Loop_Pipe_Write_Delegate {
    explicit Delegate(Platform_File_Ref pipe_writer)
        : pipe_writer(pipe_writer) {}

    void on_pipe_read_data(Event_Loop_Base* l, Platform_File_Ref,
                           String8_View) override {
      // Should be called promptly because data is available.

      {
        Lock_Ptr<Shared_State> state = this->state_.lock();
        EXPECT_EQ(state->pipe_read_data_call_count, 0);
        EXPECT_EQ(state->pipe_write_ready_call_count, 0);
        state->pipe_read_data_call_count += 1;
      }

      l->enable_pipe_write(this->pipe_writer);
    }

    void on_pipe_write_ready(Event_Loop_Base* l, Platform_File_Ref) override {
      // Should be called after Delegate::on_pipe_read_data enables.
      {
        Lock_Ptr<Shared_State> state = this->state_.lock();
        EXPECT_EQ(state->pipe_read_data_call_count, 1);
        EXPECT_EQ(state->pipe_write_ready_call_count, 0);
        state->pipe_write_ready_call_count += 1;
      }

      l->stop_event_loop_testing_only();
    }

    struct Shared_State {
      int pipe_read_data_call_count = 0;
      int pipe_write_ready_call_count = 0;
    };

    Platform_File_Ref pipe_writer;
    Synchronized<Shared_State> state_;
  };
  Delegate delegate(pipe.writer.ref());
  this->loop->register_pipe_read(start_pipe.reader.ref(), &delegate);
  this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->disable_pipe_write(pipe.writer.ref());

  this->loop->run();

  EXPECT_EQ(delegate.state_.lock()->pipe_read_data_call_count, 1);
  EXPECT_EQ(delegate.state_.lock()->pipe_write_ready_call_count, 1);
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(
    Test_Event_Loop,
    reenabling_pipe_write_callback_from_thread_fires_if_read_end_is_closed_SLOW) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  // Make on_pipe_write_end be called when the pipe_write is enabled.
  pipe.reader.close();

  struct Delegate : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_end(Event_Loop_Base* l, Platform_File_Ref) override {
      EXPECT_TRUE(this->thread_enabled_pipe_write);

      l->stop_event_loop_testing_only();
    }

    std::atomic<bool> thread_enabled_pipe_write = false;
  };
  Delegate delegate;
  this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->disable_pipe_write(pipe.writer.ref());

  Thread stop_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    delegate.thread_enabled_pipe_write = true;
    this->loop->enable_pipe_write(pipe.writer.ref());
  });

  this->loop->run();

  stop_thread.join();
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(Test_Event_Loop, disabling_pipe_write_again_has_no_effect_SLOW) {
  // This test's implementation is like
  // disabled_pipe_write_callback_does_not_fire_if_write_can_not_block_SLOW.

  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  struct Delegate : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_ready(Event_Loop_Base*, Platform_File_Ref) override {
      ADD_FAILURE()
          << "pipe should be writable but callback should not be called";
    }
  };
  Delegate delegate;
  this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->disable_pipe_write(pipe.writer.ref());
  // Disable again. This should leave the pipe_write registration disabled.
  this->loop->disable_pipe_write(pipe.writer.ref());

  Thread stop_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);
    this->loop->stop_event_loop_testing_only();
  });

  this->loop->run();

  stop_thread.join();
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(Test_Event_Loop, enabling_pipe_write_again_has_no_effect_SLOW) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  struct Delegate : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_ready(Event_Loop_Base* loop,
                             Platform_File_Ref) override {
      this->pipe_write_ready_call_count += 1;
      loop->stop_event_loop_testing_only();
    }

    std::atomic<int> pipe_write_ready_call_count = 0;
  };
  Delegate delegate;
  this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->disable_pipe_write(pipe.writer.ref());

  Thread enable_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);
    EXPECT_EQ(delegate.pipe_write_ready_call_count, 0)
        << "event loop should not call on_pipe_write_ready while the "
           "registration is disabled";

    this->loop->enable_pipe_write(pipe.writer.ref());
    // Enable again. This should leave the pipe_write registration enabled.
    this->loop->enable_pipe_write(pipe.writer.ref());
  });

  this->loop->run();

  enable_thread.join();
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(
    Test_Event_Loop,
    registering_pipe_write_during_loop_calls_callback_if_write_can_not_block) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  // Add an arbitrary event to the event loop so that we can run code when the
  // event loop is turning.
  Pipe_FDs start_pipe = make_pipe();
  start_pipe.reader.set_pipe_non_blocking();
  write_full_message(start_pipe.writer.ref(), u8"x"_sv);

  struct Delegate : public Failing_Event_Loop_Pipe_Read_Delegate,
                    public Failing_Event_Loop_Pipe_Write_Delegate {
    explicit Delegate(Platform_File_Ref pipe_writer)
        : pipe_writer(pipe_writer) {}

    void on_pipe_read_data(Event_Loop_Base* l, Platform_File_Ref,
                           String8_View) override {
      // Should be called promptly because data is available.

      EXPECT_EQ(this->pipe_read_data_call_count, 0);
      EXPECT_EQ(this->pipe_write_ready_call_count, 0);
      this->pipe_read_data_call_count += 1;

      l->register_pipe_write(this->pipe_writer, this);
    }

    void on_pipe_write_ready(Event_Loop_Base* l, Platform_File_Ref) override {
      // Should be called after Delegate::on_pipe_read_data registers.
      EXPECT_EQ(this->pipe_read_data_call_count, 1);
      EXPECT_EQ(this->pipe_write_ready_call_count, 0);
      this->pipe_write_ready_call_count += 1;

      l->stop_event_loop_testing_only();
    }

    Platform_File_Ref pipe_writer;
    int pipe_read_data_call_count = 0;
    int pipe_write_ready_call_count = 0;
  };
  Delegate delegate(pipe.writer.ref());
  this->loop->register_pipe_read(start_pipe.reader.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_EQ(delegate.pipe_read_data_call_count, 1);
  EXPECT_EQ(delegate.pipe_write_ready_call_count, 1);
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(
    Test_Event_Loop,
    registering_pipe_write_on_separate_thread_calls_callback_if_it_should_SLOW) {
  Pipe_FDs pipe = make_pipe();
  pipe.writer.set_pipe_non_blocking();

  struct Delegate : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_ready(Event_Loop_Base* l, Platform_File_Ref) override {
      EXPECT_TRUE(this->register_thread_registered)
          << "on_pipe_write_ready should be called after register_thread "
             "registers";
      this->pipe_write_ready_call_count += 1;
      l->stop_event_loop_testing_only();
    }

    std::atomic<bool> register_thread_registered = false;
    int pipe_write_ready_call_count = 0;
  };
  Delegate delegate;
  this->loop->keep_alive();

  Thread register_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    delegate.register_thread_registered = true;
    this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  });

  this->loop->run();

  EXPECT_EQ(delegate.pipe_write_ready_call_count, 1);
  EXPECT_TRUE(delegate.register_thread_registered);

  register_thread.join();
}
#endif

#if QLJS_EVENT_LOOP2_PIPE_WRITE
TEST_P(Test_Event_Loop, pipe_write_callback_is_called_if_reader_is_closed) {
  Pipe_FDs pipe = make_pipe();
  pipe.reader.close();
  pipe.writer.set_pipe_non_blocking();

  struct Delegate : public Failing_Event_Loop_Pipe_Write_Delegate {
    void on_pipe_write_end(Event_Loop_Base* l, Platform_File_Ref) override {
      this->pipe_write_end_call_count += 1;
      l->stop_event_loop_testing_only();
    }

    int pipe_write_end_call_count = 0;
  };
  Delegate delegate;
  this->loop->register_pipe_write(pipe.writer.ref(), &delegate);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_EQ(delegate.pipe_write_end_call_count, 1);
}
#endif

#if QLJS_HAVE_KQUEUE
class Test_Event_Loop_Kqueue : public ::testing::Test {
 public:
  std::unique_ptr<Event_Loop_Kqueue> loop =
      std::make_unique<Event_Loop_Kqueue>();
};

TEST_F(
    Test_Event_Loop_Kqueue,
    custom_kqueue_callback_is_called_with_events_occurring_before_event_loop) {
  static constexpr std::uintptr_t timer_id = 1;

  struct Delegate : public Event_Loop_Custom_Kqueue_Delegate {
    void on_custom_kqueue_events(Event_Loop_Base* l,
                                 Span<struct ::kevent> events) override {
      this->custom_kqueue_events_call_count += 1;
      for (const struct ::kevent& event : events) {
        this->timer_events.push_back(event.ident);
      }
      l->stop_event_loop_testing_only();
    }

    int custom_kqueue_events_call_count = 0;
    std::vector<std::uintptr_t> timer_events;
  };
  Delegate delegate;
  Event_Loop_Kqueue::Kqueue_Udata udata =
      this->loop->register_custom_kqueue(&delegate);
  this->loop->keep_alive();

  Fixed_Vector<struct ::kevent, 1> changes;
  EV_SET(&changes.emplace_back(), timer_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
         NOTE_CRITICAL, 0, udata);
  kqueue_add_changes(this->loop->kqueue_fd(),
                     Span<const struct ::kevent>(changes));

  this->loop->run();

  EXPECT_THAT(delegate.timer_events, ElementsAreArray({timer_id}));
  EXPECT_EQ(delegate.custom_kqueue_events_call_count, 1);
}

TEST_F(
    Test_Event_Loop_Kqueue,
    custom_kqueue_callback_is_called_with_events_occurring_during_event_loop) {
  // Timer 1 fires immediately. Timer 2 is registered after timer 1 fires. Timer
  // 3 is registered after timer 2 fires.
  static constexpr std::uintptr_t timer_1_id = 1;
  static constexpr std::uintptr_t timer_2_id = 2;
  static constexpr std::uintptr_t timer_3_id = 3;

  struct Delegate : public Event_Loop_Custom_Kqueue_Delegate {
    void on_custom_kqueue_events(Event_Loop_Base* l,
                                 Span<struct ::kevent> events) override {
      this->custom_kqueue_events_call_count += 1;

      std::vector<std::uintptr_t> current_timer_events;
      for (const struct ::kevent& event : events) {
        current_timer_events.push_back(event.ident);
        this->all_timer_events.push_back(event.ident);
      }

      Fixed_Vector<struct ::kevent, 1> changes;

      switch (this->custom_kqueue_events_call_count) {
      case 1: {
        EXPECT_THAT(current_timer_events, ElementsAreArray({timer_1_id}))
            << "first call to on_custom_kqueue_events should include only "
               "timer 1";
        EV_SET(&changes.emplace_back(), timer_2_id, EVFILT_TIMER,
               EV_ADD | EV_ONESHOT, NOTE_CRITICAL, 0, this->udata);
        break;
      }

      case 2: {
        EXPECT_THAT(current_timer_events, ElementsAreArray({timer_2_id}))
            << "second call to on_custom_kqueue_events should include only "
               "timer 2";
        EV_SET(&changes.emplace_back(), timer_3_id, EVFILT_TIMER,
               EV_ADD | EV_ONESHOT, NOTE_CRITICAL, 0, this->udata);
        break;
      }

      case 3:
        EXPECT_THAT(current_timer_events, ElementsAreArray({timer_3_id}))
            << "third call to on_custom_kqueue_events should include only "
               "timer 3";
        l->stop_event_loop_testing_only();
        break;

      case 4:
      default:
        ADD_FAILURE() << "expected exactly 3 calls to on_custom_kqueue_events";
        l->stop_event_loop_testing_only();
        break;
      }

      if (!changes.empty()) {
        kqueue_add_changes(this->kqueue_fd,
                           Span<const struct ::kevent>(changes));
      }
    }

    Event_Loop_Kqueue::Kqueue_Udata udata;
    POSIX_FD_File_Ref kqueue_fd;
    int custom_kqueue_events_call_count = 0;
    std::vector<std::uintptr_t> all_timer_events;
  };
  Delegate delegate;
  Event_Loop_Kqueue::Kqueue_Udata udata =
      this->loop->register_custom_kqueue(&delegate);
  delegate.udata = udata;
  delegate.kqueue_fd = this->loop->kqueue_fd();
  this->loop->keep_alive();

  Fixed_Vector<struct ::kevent, 1> changes;
  EV_SET(&changes.emplace_back(), timer_1_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
         NOTE_CRITICAL, 0, udata);
  kqueue_add_changes(this->loop->kqueue_fd(),
                     Span<const struct ::kevent>(changes));

  this->loop->run();

  EXPECT_THAT(delegate.all_timer_events,
              ElementsAreArray({timer_1_id, timer_2_id, timer_3_id}));
  EXPECT_EQ(delegate.custom_kqueue_events_call_count, 3);
}

#if defined(__FreeBSD__)
// FIXME(strager): Timers don't seem to be reliably batched on FreeBSD.
// Sometimes we only get two timers, not all three, in a batch.
TEST_F(Test_Event_Loop_Kqueue,
       DISABLED_custom_kqueue_events_are_batched_into_one_callback)
#else
TEST_F(Test_Event_Loop_Kqueue,
       custom_kqueue_events_are_batched_into_one_callback)
#endif
{
  // All of these timers fire immediately, thus their events should be returned
  // by the same call to kqueue(). Because they are registered with the same
  // udata, the events should be batched into one on_custom_kqueue_events call.
  static constexpr std::uintptr_t timer_1_id = 1;
  static constexpr std::uintptr_t timer_2_id = 2;
  static constexpr std::uintptr_t timer_3_id = 3;

  struct Delegate : public Event_Loop_Custom_Kqueue_Delegate {
    void on_custom_kqueue_events(Event_Loop_Base* l,
                                 Span<struct ::kevent> events) override {
      this->custom_kqueue_events_call_count += 1;

      for (const struct ::kevent& event : events) {
        switch (event.ident) {
        case timer_1_id:
          this->timer_1_event_count += 1;
          break;
        case timer_2_id:
          this->timer_2_event_count += 1;
          break;
        case timer_3_id:
          this->timer_3_event_count += 1;
          break;
        }
      }

      l->stop_event_loop_testing_only();
    }

    int custom_kqueue_events_call_count = 0;
    int timer_1_event_count = 0;
    int timer_2_event_count = 0;
    int timer_3_event_count = 0;
  };
  Delegate delegate;
  Event_Loop_Kqueue::Kqueue_Udata udata =
      this->loop->register_custom_kqueue(&delegate);
  this->loop->keep_alive();

  Fixed_Vector<struct ::kevent, 3> changes;
  EV_SET(&changes.emplace_back(), timer_1_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
         NOTE_CRITICAL, 0, udata);
  EV_SET(&changes.emplace_back(), timer_2_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
         NOTE_CRITICAL, 0, udata);
  EV_SET(&changes.emplace_back(), timer_3_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
         NOTE_CRITICAL, 0, udata);
  kqueue_add_changes(this->loop->kqueue_fd(),
                     Span<const struct ::kevent>(changes));

  this->loop->run();

  EXPECT_EQ(delegate.custom_kqueue_events_call_count, 1)
      << "all events should be batched into one call to "
         "on_custom_kqueue_events";
  EXPECT_EQ(delegate.timer_1_event_count, 1);
  EXPECT_EQ(delegate.timer_2_event_count, 1);
  EXPECT_EQ(delegate.timer_3_event_count, 1);
}

TEST_F(
    Test_Event_Loop_Kqueue,
    custom_kqueue_events_for_same_delegate_but_different_registers_are_not_batched) {
  // All of these timers fire immediately, thus their events should be returned
  // by the same call to kqueue(). However, because they are registered with
  // different udatas, the events should not be batched into one
  // on_custom_kqueue_events call.
  static constexpr std::uintptr_t timer_1_id = 100;
  static constexpr std::uintptr_t timer_2_id = 200;

  struct Delegate : public Event_Loop_Custom_Kqueue_Delegate {
    void on_custom_kqueue_events(Event_Loop_Base* l,
                                 Span<struct ::kevent> events) override {
      Lock_Ptr<Shared_State> state = this->state_.lock();
      state->custom_kqueue_events_call_count += 1;

      EXPECT_EQ(events.size(), 1) << "events should not be batched";
      for (const struct ::kevent& event : events) {
        switch (event.ident) {
        case timer_1_id:
          state->timer_1_event_count += 1;
          break;
        case timer_2_id:
          state->timer_2_event_count += 1;
          break;
        }
      }

      if (state->timer_1_event_count > 0 && state->timer_2_event_count > 0) {
        l->stop_event_loop_testing_only();
      }
    }

    struct Shared_State {
      int custom_kqueue_events_call_count = 0;
      int timer_1_event_count = 0;
      int timer_2_event_count = 0;
    };

    Synchronized<Shared_State> state_;
  };
  Delegate delegate;
  Event_Loop_Kqueue::Kqueue_Udata timer_1_udata =
      this->loop->register_custom_kqueue(&delegate);
  Event_Loop_Kqueue::Kqueue_Udata timer_2_udata =
      this->loop->register_custom_kqueue(&delegate);
  this->loop->keep_alive();

  Fixed_Vector<struct ::kevent, 2> changes;
  EV_SET(&changes.emplace_back(), timer_1_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
         NOTE_CRITICAL, 0, timer_1_udata);
  EV_SET(&changes.emplace_back(), timer_2_id, EVFILT_TIMER, EV_ADD | EV_ONESHOT,
         NOTE_CRITICAL, 0, timer_2_udata);
  kqueue_add_changes(this->loop->kqueue_fd(),
                     Span<const struct ::kevent>(changes));

  this->loop->run();

  EXPECT_EQ(delegate.state_.lock()->timer_1_event_count, 1);
  EXPECT_EQ(delegate.state_.lock()->timer_2_event_count, 1);
  EXPECT_EQ(delegate.state_.lock()->custom_kqueue_events_call_count, 2);
}
#endif

#if QLJS_HAVE_POLL
class Test_Event_Loop_Poll : public ::testing::Test {
 public:
  std::unique_ptr<Event_Loop_Poll> loop = std::make_unique<Event_Loop_Poll>();
};

TEST_F(Test_Event_Loop_Poll, custom_poll_callback_is_called) {
  Pipe_FDs pipe = make_pipe();
  // Make data available to the reader (POLLIN) and send end-of-file (POLLHUP).
  write_full_message(pipe.writer.ref(), u8"hello"_sv);
  pipe.writer.close();
  short expected_revents = POLLIN | POLLHUP;

  struct Delegate : public Event_Loop_Custom_Poll_Delegate {
    void on_custom_poll_event(Event_Loop_Base* l, Platform_File_Ref,
                              short revents) override {
      this->revents.push_back(revents);
      l->stop_event_loop_testing_only();
    }

    std::vector<short> revents;
  };
  Delegate delegate;
  this->loop->register_custom_poll(pipe.reader.ref(), POLLIN, &delegate);
  this->loop->keep_alive();

  this->loop->run();

  EXPECT_THAT(delegate.revents, ElementsAreArray({expected_revents}));
}
#endif

#if defined(_WIN32)
class Test_Event_Loop_Windows : public ::testing::Test {
 public:
  std::unique_ptr<Event_Loop_Windows> loop =
      std::make_unique<Event_Loop_Windows>();
};

TEST_F(
    Test_Event_Loop_Windows,
    custom_windows_io_completion_callback_is_called_with_post_queued_completion_status_events_occurring_during_event_loop_SLOW) {
  struct Delegate
      : public Failing_Event_Loop_Custom_Windows_IO_Completion_Delegate {
    void on_custom_windows_io_completion(Event_Loop_Base* l, ::ULONG_PTR,
                                         ::DWORD number_of_bytes_transferred,
                                         ::OVERLAPPED* overlapped) override {
      this->custom_windows_io_completion_call_count += 1;

      EXPECT_TRUE(this->post_thread_called_post)
          << "event should not occur before post_thread posts";

      EXPECT_EQ(overlapped, &this->dummy_overlapped);
      EXPECT_EQ(number_of_bytes_transferred, 42);

      l->stop_event_loop_testing_only();
    }

    std::atomic<int> custom_windows_io_completion_call_count = 0;
    std::atomic<bool> post_thread_called_post = false;

    ::OVERLAPPED dummy_overlapped = {};
  };
  Delegate delegate;
  ::ULONG_PTR completion_key =
      this->loop->register_custom_windows_io_completion(&delegate);
  this->loop->keep_alive();

  Thread post_thread([&]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);
    EXPECT_EQ(delegate.custom_windows_io_completion_call_count, 0)
        << "event should not occur before post_thread posts";

    delegate.post_thread_called_post = true;
    ASSERT_TRUE(::PostQueuedCompletionStatus(
        this->loop->windows_io_completion_port().get(), 42, completion_key,
        &delegate.dummy_overlapped))
        << windows_error_message(::GetLastError());
  });

  this->loop->run();

  EXPECT_EQ(delegate.custom_windows_io_completion_call_count, 1);
  EXPECT_TRUE(delegate.post_thread_called_post);

  post_thread.join();
}

void initialize_winsock() {
  ::WSADATA wsa_data;
  int rc = ::WSAStartup(MAKEWORD(2, 2), &wsa_data);
  if (rc != NO_ERROR) {
    ADD_FAILURE() << "WSAStartup failed";
    QLJS_CRASH_ALLOWING_CORE_DUMP();
  }
}

struct Test_TCP4_Server {
  explicit Test_TCP4_Server() = default;

  Test_TCP4_Server(const Test_TCP4_Server&) = delete;
  Test_TCP4_Server& operator=(const Test_TCP4_Server&) = delete;

  void create_server_sockets(Windows_Handle_File_Ref io_completion_port,
                             ::ULONG_PTR completion_key) {
    this->listen_socket = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    ASSERT_NE(this->listen_socket, INVALID_SOCKET) << ::WSAGetLastError();
    ASSERT_NE(::CreateIoCompletionPort(
                  reinterpret_cast<::HANDLE>(this->listen_socket),
                  io_completion_port.get(), completion_key, 0),
              nullptr)
        << windows_error_message(::GetLastError());
    ::sockaddr_in server_bind_address;
    server_bind_address.sin_family = AF_INET;
    server_bind_address.sin_addr.s_addr = ::htonl(INADDR_LOOPBACK);
    server_bind_address.sin_port = 0;
    ASSERT_NE(::bind(this->listen_socket,
                     reinterpret_cast<::SOCKADDR*>(&server_bind_address),
                     sizeof(server_bind_address)),
              SOCKET_ERROR)
        << ::WSAGetLastError();
    ASSERT_NE(::listen(listen_socket, 1), SOCKET_ERROR) << ::WSAGetLastError();

    int server_address_size = sizeof(this->server_address);
    ASSERT_EQ(
        ::getsockname(this->listen_socket,
                      reinterpret_cast<::sockaddr*>(&this->server_address),
                      &server_address_size),
        0)
        << ::WSAGetLastError();
    ASSERT_EQ(server_address_size, sizeof(this->server_address));

    this->accept_socket = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    ASSERT_NE(this->accept_socket, INVALID_SOCKET) << ::WSAGetLastError();
  }

  ~Test_TCP4_Server() {
    EXPECT_NE(::closesocket(this->listen_socket), SOCKET_ERROR)
        << ::WSAGetLastError();
    EXPECT_NE(::closesocket(this->accept_socket), SOCKET_ERROR)
        << ::WSAGetLastError();
  }

  ::SOCKET listen_socket = INVALID_SOCKET;
  ::SOCKET accept_socket = INVALID_SOCKET;
  ::sockaddr_in server_address;
};

TEST_F(
    Test_Event_Loop_Windows,
    custom_windows_io_completion_callback_is_called_with_winsock_events_occurring_during_event_loop_SLOW) {
  // This test exercises asynchronous I/O completion using AcceptEx. We create a
  // TCP server on localhost and register AcceptEx with the event loop. We then
  // create a thread which tries to connect to the server. The client connection
  // will cause AcceptEx to queue an event onto the I/O completion port, causing
  // on_custom_windows_io_completion to be called.
  //
  // FIXME(strager): This test might fail if IPv4 loopback is disabled.

  static constexpr ::DWORD accept_buffer_data_size = 0;
  // The + 16 are required by AcceptEx. See
  // https://learn.microsoft.com/en-us/windows/win32/api/mswsock/nf-mswsock-acceptex
  static constexpr ::DWORD accept_buffer_local_address_size =
      sizeof(::sockaddr_in) + 16;
  static constexpr ::DWORD accept_buffer_remote_address_size =
      sizeof(::sockaddr_in) + 16;
  struct Delegate
      : public Failing_Event_Loop_Custom_Windows_IO_Completion_Delegate {
    void on_custom_windows_io_completion(Event_Loop_Base* l, ::ULONG_PTR,
                                         ::DWORD number_of_bytes_transferred,
                                         ::OVERLAPPED* overlapped) override {
      this->custom_windows_io_completion_call_count += 1;

      EXPECT_TRUE(this->client_thread_called_connect)
          << "server accept should not complete before client connects";

      EXPECT_EQ(overlapped, &this->accept_overlapped);
      EXPECT_EQ(number_of_bytes_transferred, 0);

      l->stop_event_loop_testing_only();
    }

    std::atomic<int> custom_windows_io_completion_call_count = 0;
    std::atomic<bool> client_thread_called_connect = false;

    Test_TCP4_Server server;

    std::array<char, accept_buffer_data_size +
                         accept_buffer_local_address_size +
                         accept_buffer_remote_address_size>
        accept_buffer;
    ::OVERLAPPED accept_overlapped = {};
  };
  Delegate delegate;
  ::ULONG_PTR completion_key =
      this->loop->register_custom_windows_io_completion(&delegate);
  this->loop->keep_alive();

  initialize_winsock();
  delegate.server.create_server_sockets(
      this->loop->windows_io_completion_port(), completion_key);
  ::DWORD bytes_received_synchronously;  // Unused.
  ::BOOL accepted = ::AcceptEx(
      /*sListenSocket=*/delegate.server.listen_socket,
      /*sAcceptSocket=*/delegate.server.accept_socket,
      /*lpOutputBuffer=*/delegate.accept_buffer.data(),
      /*dwReceiveDataLength=*/accept_buffer_data_size,
      /*dwLocalAddressLength=*/accept_buffer_local_address_size,
      /*dwRemoteAddressLength=*/accept_buffer_remote_address_size,
      /*lpdwBytesReceived=*/&bytes_received_synchronously,
      /*lpOverlapped=*/&delegate.accept_overlapped);
  ASSERT_FALSE(accepted) << "::AcceptEx should not succeed synchrnously";
  EXPECT_EQ(::WSAGetLastError(), ERROR_IO_PENDING)
      << "::AcceptEx should not fail";

  Thread client_thread([&delegate]() -> void {
    // Wait for the event loop to start.
    std::this_thread::sleep_for(10ms);

    ::SOCKET client_socket = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    ASSERT_NE(client_socket, INVALID_SOCKET) << ::WSAGetLastError();

    delegate.client_thread_called_connect = true;
    EXPECT_EQ(delegate.custom_windows_io_completion_call_count, 0)
        << "server accept should not complete before client connects";

    ASSERT_NE(::connect(client_socket,
                        reinterpret_cast<::SOCKADDR*>(
                            &delegate.server.server_address),
                        sizeof(delegate.server.server_address)),
              SOCKET_ERROR)
        << ::WSAGetLastError();

    ASSERT_NE(::closesocket(client_socket), SOCKET_ERROR)
        << ::WSAGetLastError();
  });

  this->loop->run();

  EXPECT_EQ(delegate.custom_windows_io_completion_call_count, 1);
  EXPECT_TRUE(delegate.client_thread_called_connect);

  client_thread.join();
}
#endif

void write_full_message(Platform_File_Ref file, String8_View message) {
  auto write_result = file.write_full(message.data(), message.size());
  EXPECT_TRUE(write_result.ok()) << write_result.error_to_string();
}

#define QLJS_EVENT_LOOP_FACTORY(Event_Loop_Type)       \
  Event_Loop_Factory {                                 \
    .name = #Event_Loop_Type,                          \
    .make = []() -> std::shared_ptr<Event_Loop_Base> { \
      return std::make_shared<Event_Loop_Type>();      \
    },                                                 \
  }

::testing::internal::ParamGenerator<Event_Loop_Factory> event_loop_factories =
    ::testing::ValuesIn({
#if QLJS_HAVE_KQUEUE
      QLJS_EVENT_LOOP_FACTORY(Event_Loop_Kqueue),
#endif
#if QLJS_HAVE_POLL
          QLJS_EVENT_LOOP_FACTORY(Event_Loop_Poll),
#endif
#if defined(_WIN32)
          QLJS_EVENT_LOOP_FACTORY(Event_Loop_Windows),
#endif
    });
INSTANTIATE_TEST_SUITE_P(, Test_Event_Loop, event_loop_factories,
                         Event_Loop_Factory::get_name);
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
