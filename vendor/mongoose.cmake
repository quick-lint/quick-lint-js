# Copyright (C) 2020  Matthew "strager" Glazar
# See end of file for extended copyright information.

add_library(
  mongoose
  STATIC
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_azurertos.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_esp32.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_esp8266.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_freertos.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_newlib.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_rp2040.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_rtx.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_tirtos.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_unix.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_win32.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/arch_zephyr.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/base64.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/base64.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/config.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/dns.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/dns.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/event.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/event.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/fmt.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/fmt.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/fs.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/fs.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/fs_fat.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/fs_packed.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/fs_posix.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/http.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/http.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/iobuf.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/iobuf.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/json.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/json.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/license.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/log.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/log.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/md5.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/md5.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/mqtt.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/mqtt.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/net.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/net.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/net_ft.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/net_lwip.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/net_rl.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/rpc.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/rpc.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/sha1.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/sha1.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/sntp.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/sntp.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/sock.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/ssi.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/ssi.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/str.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/str.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/timer.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/timer.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/tls.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/tls_dummy.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/tls_mbed.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/tls_mbed.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/tls_openssl.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/tls_openssl.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/url.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/url.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/util.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/util.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/version.h"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/ws.c"
  "${CMAKE_CURRENT_LIST_DIR}/mongoose/src/ws.h"
)
target_compile_definitions(
  mongoose
  PUBLIC
  MG_ENABLE_IPV6=1
  MG_ENABLE_LOG=1
  MG_ENABLE_MBEDTLS=0
  MG_ENABLE_OPENSSL=0
  MG_ENABLE_SOCKET=1
  MG_ENABLE_SSI=0
)
target_include_directories(
  mongoose
  SYSTEM
  PUBLIC
  # Users should include our <mongoose.h>.
  "${CMAKE_CURRENT_LIST_DIR}"
)
quick_lint_js_optimize_target_for_code_size(mongoose)

if (WIN32)
  target_link_libraries(mongoose PRIVATE wsock32 ws2_32)
endif ()

# quick-lint-js finds bugs in JavaScript programs.
# Copyright (C) 2020  Matthew "strager" Glazar
#
# This file is part of quick-lint-js.
#
# quick-lint-js is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# quick-lint-js is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
