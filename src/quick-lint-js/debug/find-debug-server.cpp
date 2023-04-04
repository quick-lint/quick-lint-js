// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/debug/find-debug-server.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/io/file-handle.h>
#include <quick-lint-js/io/temporary-directory.h>
#include <quick-lint-js/logging/log.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/integer.h>
#include <quick-lint-js/util/narrow-cast.h>
#include <string_view>
#include <vector>

#if defined(__linux__)
#include <dirent.h>
#include <fcntl.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

#if defined(__APPLE__)
#include <libproc.h>
#include <pthread.h>
#endif

#if defined(__FreeBSD__)
#include <fcntl.h>
#include <kvm.h>
#include <limits.h>
#include <pthread.h>
#include <pthread_np.h>
#include <sys/jail.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <sys/user.h>
#endif

#if defined(_WIN32)
#include <psapi.h>
#include <quick-lint-js/port/windows-error.h>
#include <quick-lint-js/port/windows.h>
#include <tlhelp32.h>
#endif

#if defined(__APPLE__) && !defined(PROC_PIDLISTTHREADIDS)
#define PROC_PIDLISTTHREADIDS 28
#endif

using namespace std::literals::string_view_literals;

// NOTE[find-debug-server]: In order for quick-lint-js to find running instances
// of quick-lint-js debug servers, we have two separate but related tasks:
//
// 1. Make each instance findable
// 2. Find each instance
//
// There are various operating system facilities for implementing this feature.
//
// We choose a quirky one: thread names.
//
// 1. To make an instance findable, we name the debug server thread something
//    relatively unique, such as "quick-lint-js debug server port=12345". We
//    include the port number so the finder can create a URL for the user.
//
// 2. To find instances, we scan the operating system's process table to
//    enumerate all threads for all processes. If a thread's name matches the
//    pattern we expect (e.g. starts with "quick-lint-js debug server port="),
//    we parse the port number and remember its process ID.
//
// Benefits:
//
// * If we have multiple debug servers in a process, they will each be detected
//   separately.
// * If the debug server thread exits (including if the process crashes), the
//   instance automatically unregisters itself.
// * Finding instances has a very high certainty. False positives would only
//   occur with a malicious package.
//   * False positives are more likely on Linux where thread names are shorter.
//
// Drawbacks:
//
// * Scanning the process table is something malware would do. Virus scanners
//   might be skeptical.
// * Scanning the process table might be slow. This is tolerable because we only
//   scan the process table when requested by the user, and a user will rarely
//   request it.
//
// Other options considered:
//
// * Database of running instances stored in the filesystem. This is ugly
//   because cleaning up this state in a robust way is tricky. Also, users might
//   delete the database files by mistake.
//
// * Scanning for listening TCP ports. This would find quick-lint-js debug
//   server instances, but also other processes, so there would be a lot of
//   false positives.
//
// * Dedicated master server process to keep track of running instances.
//   Communication would be done with a UNIX domain socket (UDS) or Windows
//   named pipes. If this master process dies, the list of running quick-lint-js
//   instances is lost. Users will probably be skeptical of this master process
//   running in the background.
//
// * Scanning the process table for processes named 'quick-lint-js'. This would
//   fail to find instances of quick-lint-js which loaded as a DLL, such as the
//   Visual Studio Code extension.
//
// * Scanning the process table for processes which have the quick-lint-js
//   executable file open. This would fail to find instances of quick-lint-js
//   which loaded as a DLL, and also fail to find instances of different
//   versions of quick-lint-js.

namespace quick_lint_js {
namespace {
// NOTE(strager): thread_name_prefix is used by find_debug_servers to search for
// quick-lint-js processes. As such, it should not be changed between
// quick-lint-js versions, else newer versions cannot find older versions (or
// vice versa).
#if defined(__linux__)
// Thread names are short on Linux, so keep this prefix short.
constexpr std::string_view thread_name_prefix = "quick-lint"sv;
constexpr std::size_t max_thread_name_length = 15;
#define QLJS_CAN_FIND_DEBUG_SERVERS 1
#endif
#if defined(__APPLE__)
constexpr std::string_view thread_name_prefix =
    "quick-lint-js debug server port="sv;
constexpr std::size_t max_thread_name_length = MAXTHREADNAMESIZE - 1;
#define QLJS_CAN_FIND_DEBUG_SERVERS 1
#endif
#if defined(__FreeBSD__)
constexpr std::string_view thread_name_prefix = "quick-lint"sv;
// NOTE(Nico): This seems to be the limit defined in the kernel. It is extremely
//             small so we choose a very short prefix like on Linux. (see
//             sys/sys/proc.h:300, struct thread). Then we find another limit
//             TDNAMLEN which is defined in sys/user.h and using the struct
//             kinfo_proc. The following assert assures that TDNAMLEN isn't
//             bigger than what we use here.
constexpr std::size_t max_thread_name_length = MAXCOMLEN;
static_assert(max_thread_name_length >= TDNAMLEN,
              "sys/user.h defines TDNAMLEN to be bigger than MAXCOMLEN. "
              "Your headers are broken.");
#define QLJS_CAN_FIND_DEBUG_SERVERS 1
#endif
#if defined(_WIN32)
constexpr std::wstring_view thread_name_prefix =
    L"quick-lint-js debug server port="sv;
// Thread names on Windows can seemingly be any length. Pick a reasonable limit
// for ourselves.
constexpr std::size_t max_thread_name_length = 256;
#define QLJS_CAN_FIND_DEBUG_SERVERS 1
#endif

#if !defined(QLJS_CAN_FIND_DEBUG_SERVERS)
#define QLJS_CAN_FIND_DEBUG_SERVERS 0
#endif

#if QLJS_CAN_FIND_DEBUG_SERVERS
using thread_name_char_type = decltype(thread_name_prefix)::value_type;

static_assert(thread_name_prefix.size() +
                      integer_string_length<std::uint16_t> <=
                  max_thread_name_length,
              "thread_name_prefix is too long");

std::optional<std::uint16_t> parse_port_number_from_thread_name(
    std::basic_string_view<thread_name_char_type> thread_name) {
  if (!starts_with(thread_name, thread_name_prefix)) {
    return std::nullopt;
  }
  std::basic_string_view<thread_name_char_type> port_string =
      thread_name.substr(thread_name_prefix.size());
  std::uint16_t port;
  if (parse_integer_exact(port_string, port) != parse_integer_exact_error::ok) {
    return std::nullopt;
  }
  return port;
}
#endif
}

#if QLJS_FEATURE_DEBUG_SERVER
void register_current_thread_as_debug_server_thread(std::uint16_t port_number) {
  std::array<thread_name_char_type, max_thread_name_length + 1> name;
  thread_name_char_type* out = std::copy(thread_name_prefix.begin(),
                                         thread_name_prefix.end(), name.data());
  out = write_integer(port_number, out);
  *out++ = '\0';
  QLJS_ASSERT((out - name.data()) <= narrow_cast<std::ptrdiff_t>(name.size()));

#if defined(__linux__)
  int rc = ::prctl(PR_SET_NAME, reinterpret_cast<std::uintptr_t>(name.data()),
                   0, 0, 0);
  if (rc != 0) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: %s\n",
        __func__, std::strerror(errno));
    return;
  }
#endif
#if defined(__APPLE__)
  int rc = ::pthread_setname_np(name.data());
  if (rc != 0) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: %s\n",
        __func__, std::strerror(errno));
    return;
  }
#endif
#if defined(__FreeBSD__)
  int rc = ::pthread_setname_np(::pthread_self(), name.data());
  if (rc != 0) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: %s\n",
        __func__, std::strerror(rc));
    return;
  }
#endif
#if defined(_WIN32)
  ::HRESULT rc = ::SetThreadDescription(::GetCurrentThread(), name.data());
  if (FAILED(rc)) {
    QLJS_DEBUG_LOG(
        "%s: ignoring failure to set thread name for debug server thread: "
        "%#08lx\n",
        __func__, rc);
    return;
  }
#endif
}
#endif

#if defined(__linux__)
template <class Callback>
void enumerate_all_process_thread_names(Callback&& callback) {
  static constexpr char func[] = "enumerate_all_process_thread_names";

  std::string path = "/proc/";
  std::size_t path_size_without_process_id = path.size();

  auto visit_proc_entry = [&](const char* proc_entry_name) -> void {
    if (!isdigit(proc_entry_name[0])) {
      return;
    }
    path.resize(path_size_without_process_id);
    path.append(proc_entry_name);
    path.append("/task/");
    std::size_t path_size_without_task_id = path.size();

    auto visit_task_entry = [&](const char* task_entry_name) -> void {
      if (!isdigit(task_entry_name[0])) {
        return;
      }

      path.resize(path_size_without_task_id);
      path.append(task_entry_name);
      path.append("/comm");

      posix_fd_file thread_name_file(::open(path.c_str(), O_RDONLY));
      if (!thread_name_file.valid()) {
        switch (errno) {
        case ENOENT:
          // The task probably died. Ignore.
          return;

        default:
          QLJS_DEBUG_LOG("%s: ignoring failure to get open %s: %s\n", func,
                         path.c_str(), std::strerror(errno));
          return;
        }
      }
      char thread_name_buffer[max_thread_name_length + 1];
      file_read_result read_result =
          thread_name_file.read(thread_name_buffer, sizeof(thread_name_buffer));
      if (!read_result.ok() || read_result.at_end_of_file()) {
        QLJS_DEBUG_LOG("%s: ignoring failure to get read %s: %s\n", func,
                       path.c_str(), std::strerror(errno));
        return;
      }
      std::string_view thread_name(
          thread_name_buffer,
          narrow_cast<std::size_t>(read_result.bytes_read()));
      if (ends_with(thread_name, '\n')) {
        thread_name.remove_suffix(1);
      }
      thread_name_file.close();

      callback(
          /*process_id_string=*/std::string_view(proc_entry_name),
          /*thread_name=*/thread_name);
    };
    result<void, platform_file_io_error> list_task =
        list_directory(path.c_str(), visit_task_entry);
    if (!list_task.ok()) {
      path.resize(path_size_without_task_id);
      QLJS_DEBUG_LOG("%s: ignoring failure to read %s: %s\n", func,
                     path.c_str(), list_task.error_to_string().c_str());
    }
  };
  result<void, platform_file_io_error> list_proc =
      list_directory(path.c_str(), visit_proc_entry);
  if (!list_proc.ok()) {
    QLJS_DEBUG_LOG("%s: ignoring failure to read /proc: %s\n", func,
                   list_proc.error_to_string().c_str());
  }
}
#endif

#if defined(__linux__)
std::vector<found_debug_server> find_debug_servers() {
  std::vector<found_debug_server> debug_servers;

  enumerate_all_process_thread_names(
      [&](std::string_view process_id_string, std::string_view thread_name) {
        if (std::optional<std::uint16_t> port_number =
                parse_port_number_from_thread_name(thread_name)) {
          std::uint64_t process_id;
          parse_integer_exact_error parse_error =
              parse_integer_exact(process_id_string, process_id);
          QLJS_ASSERT(parse_error == parse_integer_exact_error::ok);
          if (parse_error == parse_integer_exact_error::ok) {
            debug_servers.push_back(found_debug_server{
                .process_id = process_id,
                .port_number = *port_number,
            });
          }
        }
      });

  return debug_servers;
}
#endif

#if defined(__APPLE__)
template <class Callback>
void enumerate_all_process_threads(Callback&& callback) {
  // Kernel code uses sizeof(int), not sizeof(::pid_t):
  // https://github.com/apple/darwin-xnu/blob/8f02f2a044b9bb1ad951987ef5bab20ec9486310/bsd/kern/proc_info.c#L339
  //
  // ::pid_t and int are the same, but let's be certain:
  static_assert(sizeof(int) == sizeof(::pid_t));

  int process_id_buffer_size =
      ::proc_listpids(PROC_ALL_PIDS, /*typeinfo=*/0, nullptr, 0);
  if (process_id_buffer_size == -1) {
    QLJS_DEBUG_LOG("%s: ignoring failure to get process ID count: %s\n",
                   __func__, std::strerror(errno));
    return;
  }
  QLJS_ASSERT(
      narrow_cast<std::size_t>(process_id_buffer_size) % sizeof(::pid_t) == 0);

  // NOTE(strager): It's okay if our buffer is to small. We miss out on some
  // processes, but they were just created anyway. Harmless race condition.
  std::vector<::pid_t> process_ids(
      narrow_cast<std::size_t>(process_id_buffer_size) / sizeof(::pid_t));
  process_id_buffer_size =
      ::proc_listpids(PROC_ALL_PIDS, 0, process_ids.data(),
                      narrow_cast<int>(process_ids.size() * sizeof(::pid_t)));
  if (process_id_buffer_size == -1) {
    QLJS_DEBUG_LOG("%s: ignoring failure to get process IDs: %s\n", __func__,
                   std::strerror(errno));
    return;
  }
  process_ids.resize(narrow_cast<std::size_t>(process_id_buffer_size) /
                     sizeof(int));

  std::vector<std::uint64_t> thread_ids;
  constexpr std::size_t initial_thread_ids_buffer_count = 128;  // Arbitrary.
  for (::pid_t process_id : process_ids) {
    thread_ids.resize(initial_thread_ids_buffer_count);
  load_threads:
    int thread_ids_buffer_size = ::proc_pidinfo(
        process_id, PROC_PIDLISTTHREADIDS,
        /*arg=*/0, thread_ids.data(),
        narrow_cast<int>(thread_ids.size() * sizeof(std::uint64_t)));
    if (thread_ids_buffer_size == -1) {
      QLJS_DEBUG_LOG(
          "%s: ignoring failure to get thread IDs for process %d: %s\n",
          __func__, process_id, std::strerror(errno));
      continue;
    }
    QLJS_ASSERT(narrow_cast<std::size_t>(thread_ids_buffer_size) %
                    sizeof(std::uint64_t) ==
                0);
    std::size_t thread_count =
        narrow_cast<std::size_t>(thread_ids_buffer_size) /
        sizeof(std::uint64_t);
    if (thread_count == thread_ids.size()) {
      // We can't tell if we read exactly all the threads or if there are more
      // threads. Assume there are more threads.
      thread_ids.resize(thread_ids.size() * 2);
      goto load_threads;
    }
    thread_ids.resize(thread_count);

    for (std::uint64_t thread_id : thread_ids) {
      callback(process_id, thread_id);
    }
  }
}
#endif

#if defined(__APPLE__)
std::vector<found_debug_server> find_debug_servers() {
  static constexpr char func[] = "find_debug_servers";

  std::vector<found_debug_server> debug_servers;
  enumerate_all_process_threads([&](::pid_t process_id,
                                    std::uint64_t thread_id) -> void {
    ::proc_threadinfo thread_info;
    int rc =
        ::proc_pidinfo(process_id, PROC_PIDTHREADID64INFO,
                       /*arg=*/thread_id, &thread_info, sizeof(thread_info));
    if (rc == -1) {
      QLJS_DEBUG_LOG(
          "%s: ignoring failure to get name of thread %llu in process %d: %s\n",
          func, narrow_cast<unsigned long long>(thread_id), process_id,
          std::strerror(errno));
      return;
    }
    QLJS_ASSERT(rc == sizeof(thread_info));

    std::string_view thread_name(
        thread_info.pth_name,
        ::strnlen(thread_info.pth_name, MAXTHREADNAMESIZE));
    if (std::optional<std::uint16_t> port_number =
            parse_port_number_from_thread_name(thread_name)) {
      debug_servers.push_back(found_debug_server{
          .process_id = narrow_cast<std::uint64_t>(process_id),
          .port_number = *port_number,
      });
    }
  });
  return debug_servers;
}
#endif

#if defined(__FreeBSD__)
std::vector<found_debug_server> find_debug_servers() {
  static constexpr char func[] = "find_debug_servers";

  // NOTE(Nico): On FreeBSD we can just fetch the list of all active LWPs by
  // querying all active processes from the kvm library. This is not very
  // efficient but it works.
  //
  // There is one potential pitfall with this approach though:
  //
  // When a quick-lint-js instance is running inside a jail then searching from
  // the host will list processes running inside the jail too (unless the
  // sysadmin disabled it for security reasons). However since the jail is
  // likely to be listening on a different IP address than the host the URL
  // printed by quick-lint does not match the one of the jail. This is why we
  // first query our own Jail ID (JID), then compare it to a given processes's
  // JID and skip the process if it doesn't match our own JID.

  char errbuf[_POSIX2_LINE_MAX] = {};
  int p_size, own_jid, rc;
  size_t own_jid_size;
  ::kinfo_proc* p;
  ::kvm_t* kd;
  std::vector<found_debug_server> debug_servers;

  // Query our own jail id
  own_jid_size = sizeof own_jid;
  rc = ::sysctlbyname("security.jail.param.jid", &own_jid, &own_jid_size,
                      nullptr, 0);
  if (rc < 0) {
    QLJS_DEBUG_LOG("%s: ignoring failure to query own jail id: %s\n", func,
                   ::strerror(errno));
    return {};
  }

  // Open KVM access device
  kd = ::kvm_openfiles(/* execfile=*/nullptr, /* corefile=*/"/dev/null",
                       /* swapfile=*/nullptr, /* flags=*/O_RDONLY, errbuf);
  if (kd == nullptr) {
    QLJS_DEBUG_LOG("%s: ignoring failure to open kvm device: %s\n", func,
                   errbuf);
    goto error_open_kvm;
  }

  // Read in process list
  p = ::kvm_getprocs(kd, KERN_PROC_ALL, 0, &p_size);
  if (p == nullptr) {
    QLJS_DEBUG_LOG("%s: ignoring failure to get process list: %s\n", func,
                   ::strerror(errno));
    goto error_get_procs;
  }

  // Search for threads with the correct name
  for (int ti = 0; ti < p_size; ++ti) {
    if (p[ti].ki_jid != own_jid)
      continue;  // not our own jail. potential IP address mismatch.

    std::string_view thread_name(
        p[ti].ki_tdname,
        ::strnlen(p[ti].ki_tdname, max_thread_name_length - 1));
    if (std::optional<std::uint16_t> port_number =
            parse_port_number_from_thread_name(thread_name)) {
      debug_servers.push_back(found_debug_server{
          .process_id = narrow_cast<std::uint64_t>(p[ti].ki_pid),
          .port_number = *port_number,
      });
    }
  }

error_get_procs:
  ::kvm_close(kd);
error_open_kvm:
  return debug_servers;
}
#endif  // __FreeBSD__

#if defined(_WIN32)
template <class Callback>
void enumerate_all_process_threads(Callback&& callback) {
  windows_handle_file thread_snapshot(
      ::CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0));
  if (!thread_snapshot.valid()) {
    QLJS_DEBUG_LOG("%s: ignoring failure to get thread list: %s\n", __func__,
                   windows_last_error_message().c_str());
    return;
  }

  ::THREADENTRY32 thread;
  thread.dwSize = sizeof(thread);
  if (!::Thread32First(thread_snapshot.get(), &thread)) {
    // If Thread32First fails with ERROR_NO_MORE_FILES, then we couldn't even
    // find the current thread. That makes no sense.
    QLJS_ASSERT(::GetLastError() != ERROR_NO_MORE_FILES);
    QLJS_DEBUG_LOG("%s: ignoring failure to start enumerating threads: %s\n",
                   __func__, windows_last_error_message().c_str());
    return;
  }
  for (;;) {
    constexpr DWORD minimum_thread_struct_size =
        offsetof(::THREADENTRY32, th32OwnerProcessID) +
        sizeof(thread.th32OwnerProcessID);
    if (thread.dwSize >= minimum_thread_struct_size &&
        thread.th32OwnerProcessID != 0) {
      callback(thread.th32OwnerProcessID, thread.th32ThreadID);
    }

    thread.dwSize = sizeof(thread);
    if (!::Thread32Next(thread_snapshot.get(), &thread)) {
      if (::GetLastError() != ERROR_NO_MORE_FILES) {
        QLJS_DEBUG_LOG("%s: ignoring failure while enumerating threads: %s\n",
                       __func__, windows_last_error_message().c_str());
      }
      break;
    }
  }
}
#endif

#if defined(_WIN32)
std::vector<found_debug_server> find_debug_servers() {
  static constexpr char func[] = "find_debug_servers";

  std::vector<found_debug_server> debug_servers;
  enumerate_all_process_threads([&](::DWORD process_id,
                                    ::DWORD thread_id) -> void {
    windows_handle_file thread_handle(
        ::OpenThread(THREAD_QUERY_LIMITED_INFORMATION,
                     /*bInheritHandle=*/false, thread_id));
    if (!thread_handle.valid()) {
      switch (::GetLastError()) {
      case ERROR_ACCESS_DENIED:
        return;

      case ERROR_INVALID_PARAMETER:
        // The thread or process probably died before we could open the thread.
        // Ignore.
        return;

      default:
        QLJS_DEBUG_LOG(
            "%s: ignoring failure while opening thread %lu of process %lu: "
            "%s\n",
            func, thread_id, process_id, windows_last_error_message().c_str());
        return;
      }
    }

    wchar_t* thread_name = nullptr;
    ::HRESULT rc = ::GetThreadDescription(thread_handle.get(), &thread_name);
    if (FAILED(rc)) {
      QLJS_DEBUG_LOG(
          "%s: ignoring failure to get thread name of thread %lu of process "
          "%lu: "
          "%#08lx\n",
          func, thread_id, process_id, rc);
      return;
    }

    if (std::optional<std::uint16_t> port_number =
            parse_port_number_from_thread_name(
                std::wstring_view(thread_name))) {
      debug_servers.push_back(found_debug_server{
          .process_id = process_id,
          .port_number = *port_number,
      });
    }

    ::LocalFree(thread_name);
  });
  return debug_servers;
}
#endif

#if !QLJS_CAN_FIND_DEBUG_SERVERS
std::vector<found_debug_server> find_debug_servers() {
#warning "--debug-apps is not supported on this platform"
  return std::vector<found_debug_server>();
}
#endif
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
