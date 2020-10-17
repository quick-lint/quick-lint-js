// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <benchmark/benchmark.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/gmo.h>
#include <quick-lint-js/translation-data.h>
#include <quick-lint-js/translation.h>

namespace quick_lint_js {
namespace {
void benchmark_translate_from_source_code(benchmark::State &state) {
  translatable_messages messages;
  messages.use_messages_from_source_code();
  for (auto _ : state) {
    ::benchmark::DoNotOptimize(
        messages.translate("variable assigned before its declaration"));
    ::benchmark::DoNotOptimize(messages.translate("variable declared here"));
    ::benchmark::DoNotOptimize(messages.translate(
        "~~~ invalid string, do not use outside benchmark ~~~"));
  }
}
BENCHMARK(benchmark_translate_from_source_code);

void benchmark_translate_from_translation_hit(benchmark::State &state) {
  static constexpr const char *messages_to_translate[] = {
      "variable assigned before its declaration",
      "variable declared here",
  };

  translatable_messages messages;
  bool have_translation =
      messages.use_messages_from_locale("en@loud", gmo_files);
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const char *message : messages_to_translate) {
    // Messages should be translated.
    QLJS_ALWAYS_ASSERT(std::strcmp(messages.translate(message), message) != 0);
  }

  for (auto _ : state) {
    for (const char *message : messages_to_translate) {
      ::benchmark::DoNotOptimize(messages.translate(message));
    }
  }
}
BENCHMARK(benchmark_translate_from_translation_hit);

void benchmark_translate_from_translation_miss(benchmark::State &state) {
  static constexpr const char *messages_to_translate[] = {
      "~~~ invalid string, do not use outside benchmark ~~~",
      "another invalid string, do not use outside benchmark",
  };

  translatable_messages messages;
  bool have_translation =
      messages.use_messages_from_locale("en@loud", gmo_files);
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const char *message : messages_to_translate) {
    // Messages should not be translated.
    QLJS_ALWAYS_ASSERT(std::strcmp(messages.translate(message), message) == 0);
  }

  for (auto _ : state) {
    for (const char *message : messages_to_translate) {
      ::benchmark::DoNotOptimize(messages.translate(message));
    }
  }
}
BENCHMARK(benchmark_translate_from_translation_miss);

void benchmark_load_translations(benchmark::State &state, const char *locale) {
  for (auto _ : state) {
    translatable_messages messages;
    bool have_translation =
        messages.use_messages_from_locale(locale, gmo_files);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(messages);
  }
}
BENCHMARK_CAPTURE(benchmark_load_translations, c, "C");
BENCHMARK_CAPTURE(benchmark_load_translations, en, "en");
BENCHMARK_CAPTURE(benchmark_load_translations, en_loud, "en@loud");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us, "en_US");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us_loud, "en_US@loud");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us_utf8, "en_US.utf8");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us_utf8_loud,
                  "en_US.utf8@loud");
BENCHMARK_CAPTURE(benchmark_load_translations, posix, "POSIX");
}
}
