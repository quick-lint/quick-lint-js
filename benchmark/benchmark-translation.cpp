// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/translation.h>

namespace quick_lint_js {
namespace {
void benchmark_translate_from_source_code(benchmark::State &state) {
  translatable_messages messages;
  messages.use_messages_from_source_code();
  for (auto _ : state) {
    ::benchmark::DoNotOptimize(messages.translate(
        QLJS_TRANSLATABLE("variable assigned before its declaration")));
    ::benchmark::DoNotOptimize(
        messages.translate(QLJS_TRANSLATABLE("variable declared here")));
    ::benchmark::DoNotOptimize(messages.translate(QLJS_TRANSLATABLE(
        "~~~ invalid string, do not use outside benchmark ~~~")));
  }
}
BENCHMARK(benchmark_translate_from_source_code);

struct translatable_message_with_original {
  translatable_message translatable;
  const char *original;
};

void benchmark_translate_from_translation_hit(benchmark::State &state) {
  static constexpr translatable_message_with_original messages_to_translate[] =
      {
          {QLJS_TRANSLATABLE("variable assigned before its declaration"),
           "variable assigned before its declaration"},
          {QLJS_TRANSLATABLE("variable declared here"),
           "variable declared here"},
      };

  translatable_messages messages;
  bool have_translation = messages.use_messages_from_locale("en@loud");
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const translatable_message_with_original &message :
       messages_to_translate) {
    // Messages should be translated.
    QLJS_ALWAYS_ASSERT(std::strcmp(messages.translate(message.translatable),
                                   message.original) != 0);
  }

  for (auto _ : state) {
    for (const translatable_message_with_original &message :
         messages_to_translate) {
      ::benchmark::DoNotOptimize(messages.translate(message.translatable));
    }
  }
}
BENCHMARK(benchmark_translate_from_translation_hit);

void benchmark_translate_from_translation_miss(benchmark::State &state) {
  static constexpr translatable_message_with_original messages_to_translate[] =
      {
          {QLJS_TRANSLATABLE(
               "~~~ invalid string, do not use outside benchmark ~~~"),
           "~~~ invalid string, do not use outside benchmark ~~~"},
          {QLJS_TRANSLATABLE(
               "another invalid string, do not use outside benchmark"),
           "another invalid string, do not use outside benchmark"},
      };

  translatable_messages messages;
  bool have_translation = messages.use_messages_from_locale("en@loud");
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const translatable_message_with_original &message :
       messages_to_translate) {
    // Messages should not be translated.
    QLJS_ALWAYS_ASSERT(std::strcmp(messages.translate(message.translatable),
                                   message.original) == 0);
  }

  for (auto _ : state) {
    for (const translatable_message_with_original &message :
         messages_to_translate) {
      ::benchmark::DoNotOptimize(messages.translate(message.translatable));
    }
  }
}
BENCHMARK(benchmark_translate_from_translation_miss);

void benchmark_load_translations(benchmark::State &state, const char *locale) {
  for (auto _ : state) {
    translatable_messages messages;
    bool have_translation = messages.use_messages_from_locale(locale);
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
  static constexpr translatable_message message_to_translate =
      QLJS_TRANSLATABLE("variable assigned before its declaration");
  static constexpr const char *untranslated_message =
      "variable assigned before its declaration";

  const char *locale = "en@loud";
  {
    // Message should be translated.
    translatable_messages messages;
    bool have_translation = messages.use_messages_from_locale(locale);
    QLJS_ALWAYS_ASSERT(have_translation);
    QLJS_ALWAYS_ASSERT(std::strcmp(messages.translate(message_to_translate),
                                   untranslated_message) != 0);
  }

  for (auto _ : state) {
    translatable_messages messages;
    bool have_translation = messages.use_messages_from_locale(locale);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(messages.translate(message_to_translate));
  }
}
BENCHMARK(benchmark_load_translations_and_find_hit);

void benchmark_load_translations_and_find_miss(benchmark::State &state) {
  static constexpr translatable_message message_to_translate =
      QLJS_TRANSLATABLE("~~~ invalid string, do not use outside benchmark ~~~");
  static constexpr const char *untranslated_message =
      "~~~ invalid string, do not use outside benchmark ~~~";

  const char *locale = "en@loud";
  {
    // Message should not be translated.
    translatable_messages messages;
    bool have_translation = messages.use_messages_from_locale(locale);
    QLJS_ALWAYS_ASSERT(have_translation);
    QLJS_ALWAYS_ASSERT(std::strcmp(messages.translate(message_to_translate),
                                   untranslated_message) == 0);
  }

  for (auto _ : state) {
    translatable_messages messages;
    bool have_translation = messages.use_messages_from_locale(locale);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(messages.translate(message_to_translate));
  }
}
BENCHMARK(benchmark_load_translations_and_find_miss);
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
