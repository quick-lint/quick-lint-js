// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <benchmark/benchmark.h>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/i18n/translation.h>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
void benchmark_translate_from_source_code(benchmark::State &state) {
  translator t;
  t.use_messages_from_source_code();
  for (auto _ : state) {
    ::benchmark::DoNotOptimize(t.translate(
        QLJS_TRANSLATABLE("variable assigned before its declaration")));
    ::benchmark::DoNotOptimize(
        t.translate(QLJS_TRANSLATABLE("variable declared here")));
    ::benchmark::DoNotOptimize(t.translate(QLJS_TRANSLATABLE(
        "~~~ invalid string, do not use outside benchmark ~~~")));
  }
}
BENCHMARK(benchmark_translate_from_source_code);

struct translatable_message_with_original {
  translatable_message translatable;
  string8_view original;
};

void benchmark_translate_from_translation_hit(benchmark::State &state) {
  static constexpr translatable_message_with_original messages_to_translate[] =
      {
          {QLJS_TRANSLATABLE("variable assigned before its declaration"),
           u8"variable assigned before its declaration"_sv},
          {QLJS_TRANSLATABLE("variable declared here"),
           u8"variable declared here"_sv},
      };

  translator t;
  bool have_translation = t.use_messages_from_locale("en_US@snarky");
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const translatable_message_with_original &message :
       messages_to_translate) {
    // Messages should be translated.
    QLJS_ALWAYS_ASSERT(t.translate(message.translatable) != message.original);
  }

  for (auto _ : state) {
    for (const translatable_message_with_original &message :
         messages_to_translate) {
      ::benchmark::DoNotOptimize(t.translate(message.translatable));
    }
  }
}
BENCHMARK(benchmark_translate_from_translation_hit);

void benchmark_translate_from_translation_miss(benchmark::State &state) {
  static constexpr translatable_message_with_original messages_to_translate[] =
      {
          {QLJS_TRANSLATABLE(
               "~~~ invalid string, do not use outside benchmark ~~~"),
           u8"~~~ invalid string, do not use outside benchmark ~~~"_sv},
          {QLJS_TRANSLATABLE(
               "another invalid string, do not use outside benchmark"),
           u8"another invalid string, do not use outside benchmark"_sv},
      };

  translator t;
  bool have_translation = t.use_messages_from_locale("en_US@snarky");
  QLJS_ALWAYS_ASSERT(have_translation);
  for (const translatable_message_with_original &message :
       messages_to_translate) {
    // Messages should not be translated.
    QLJS_ALWAYS_ASSERT(t.translate(message.translatable) == message.original);
  }

  for (auto _ : state) {
    for (const translatable_message_with_original &message :
         messages_to_translate) {
      ::benchmark::DoNotOptimize(t.translate(message.translatable));
    }
  }
}
BENCHMARK(benchmark_translate_from_translation_miss);

void benchmark_load_translations(benchmark::State &state, const char *locale) {
  for (auto _ : state) {
    translator t;
    bool have_translation = t.use_messages_from_locale(locale);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(t);
  }
}
BENCHMARK_CAPTURE(benchmark_load_translations, c, "C");
BENCHMARK_CAPTURE(benchmark_load_translations, en, "en");
BENCHMARK_CAPTURE(benchmark_load_translations, en_snarky, "en_US@snarky");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us, "en_US");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us_snarky, "en_US@snarky");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us_utf8, "en_US.utf8");
BENCHMARK_CAPTURE(benchmark_load_translations, en_us_utf8_snarky,
                  "en_US.utf8@snarky");
BENCHMARK_CAPTURE(benchmark_load_translations, posix, "POSIX");

void benchmark_load_translations_and_find_hit(benchmark::State &state) {
  static constexpr translatable_message message_to_translate =
      QLJS_TRANSLATABLE("variable assigned before its declaration");
  static constexpr string8_view untranslated_message =
      u8"variable assigned before its declaration"_sv;

  const char *locale = "en_US@snarky";
  {
    // Message should be translated.
    translator t;
    bool have_translation = t.use_messages_from_locale(locale);
    QLJS_ALWAYS_ASSERT(have_translation);
    QLJS_ALWAYS_ASSERT(t.translate(message_to_translate) !=
                       untranslated_message);
  }

  for (auto _ : state) {
    translator t;
    bool have_translation = t.use_messages_from_locale(locale);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(t.translate(message_to_translate));
  }
}
BENCHMARK(benchmark_load_translations_and_find_hit);

void benchmark_load_translations_and_find_miss(benchmark::State &state) {
  static constexpr translatable_message message_to_translate =
      QLJS_TRANSLATABLE("~~~ invalid string, do not use outside benchmark ~~~");
  static constexpr string8_view untranslated_message =
      u8"~~~ invalid string, do not use outside benchmark ~~~"_sv;

  const char *locale = "en_US@snarky";
  {
    // Message should not be translated.
    translator t;
    bool have_translation = t.use_messages_from_locale(locale);
    QLJS_ALWAYS_ASSERT(have_translation);
    QLJS_ALWAYS_ASSERT(t.translate(message_to_translate) ==
                       untranslated_message);
  }

  for (auto _ : state) {
    translator t;
    bool have_translation = t.use_messages_from_locale(locale);
    ::benchmark::DoNotOptimize(have_translation);
    ::benchmark::DoNotOptimize(t.translate(message_to_translate));
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
