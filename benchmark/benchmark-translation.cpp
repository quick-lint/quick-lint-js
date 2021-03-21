// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

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
    ::benchmark::DoNotOptimize(messages.translate(
        "variable assigned before its declaration"_gmo_message));
    ::benchmark::DoNotOptimize(
        messages.translate("variable declared here"_gmo_message));
    ::benchmark::DoNotOptimize(messages.translate(
        "~~~ invalid string, do not use outside benchmark ~~~"_gmo_message));
  }
}
BENCHMARK(benchmark_translate_from_source_code);

void benchmark_translate_from_translation_hit(benchmark::State &state) {
  static constexpr gmo_message messages_to_translate[] = {
      "variable assigned before its declaration"_gmo_message,
      "variable declared here"_gmo_message,
  };

  translatable_messages messages;
  bool have_translation =
      messages.use_messages_from_locale("en@loud", gmo_files);
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const gmo_message &message : messages_to_translate) {
    // Messages should be translated.
    QLJS_ALWAYS_ASSERT(messages.translate(message) != message.message);
  }

  for (auto _ : state) {
    for (const gmo_message &message : messages_to_translate) {
      ::benchmark::DoNotOptimize(messages.translate(message));
    }
  }
}
BENCHMARK(benchmark_translate_from_translation_hit);

void benchmark_translate_from_translation_miss(benchmark::State &state) {
  static constexpr gmo_message messages_to_translate[] = {
      "~~~ invalid string, do not use outside benchmark ~~~"_gmo_message,
      "another invalid string, do not use outside benchmark"_gmo_message,
  };

  translatable_messages messages;
  bool have_translation =
      messages.use_messages_from_locale("en@loud", gmo_files);
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const gmo_message &message : messages_to_translate) {
    // Messages should not be translated.
    QLJS_ALWAYS_ASSERT(messages.translate(message) == message.message);
  }

  for (auto _ : state) {
    for (const gmo_message &message : messages_to_translate) {
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

void benchmark_load_translations_and_find_hit(benchmark::State &state) {
  static constexpr gmo_message message_to_translate =
      "variable assigned before its declaration"_gmo_message;

  const char *locale = "en@loud";
  {
    // Message should be translated.
    translatable_messages messages;
    bool have_translation =
        messages.use_messages_from_locale(locale, gmo_files);
    QLJS_ALWAYS_ASSERT(have_translation);
    QLJS_ALWAYS_ASSERT(messages.translate(message_to_translate) !=
                       message_to_translate.message);
  }

  for (auto _ : state) {
    translatable_messages messages;
    bool have_translation =
        messages.use_messages_from_locale(locale, gmo_files);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(messages.translate(message_to_translate));
  }
}
BENCHMARK(benchmark_load_translations_and_find_hit);

void benchmark_load_translations_and_find_miss(benchmark::State &state) {
  static constexpr gmo_message message_to_translate =
      "~~~ invalid string, do not use outside benchmark ~~~"_gmo_message;

  const char *locale = "en@loud";
  {
    // Message should not be translated.
    translatable_messages messages;
    bool have_translation =
        messages.use_messages_from_locale(locale, gmo_files);
    QLJS_ALWAYS_ASSERT(have_translation);
    QLJS_ALWAYS_ASSERT(messages.translate(message_to_translate) ==
                       message_to_translate.message);
  }

  for (auto _ : state) {
    translatable_messages messages;
    bool have_translation =
        messages.use_messages_from_locale(locale, gmo_files);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(messages.translate(message_to_translate));
  }
}
BENCHMARK(benchmark_load_translations_and_find_miss);
}
}

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
