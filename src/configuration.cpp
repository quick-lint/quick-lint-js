// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <boost/json/parse.hpp>
#include <boost/json/value.hpp>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/global-variables.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>
#include <system_error>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
source_code_span span_of_json_value(::simdjson::fallback::ondemand::value&);
}

configuration::configuration() { this->reset(); }

const global_declared_variable_set& configuration::globals() noexcept {
  if (!this->did_add_globals_from_groups_) {
    this->build_globals_from_groups();
  }
  return this->globals_;
}

void configuration::reset_global_groups() {
  for (bool& enabled : this->enabled_global_groups_) {
    enabled = false;
  }
}

bool configuration::add_global_group(string8_view group_name) {
  for (std::size_t i = 0; i < this->enabled_global_groups_.size(); ++i) {
    if (group_name == global_groups[i].name) {
      this->enabled_global_groups_[i] = true;
      return true;
    }
  }
  return false;
}

void configuration::add_global_variable(
    global_declared_variable global_variable) {
  this->globals_.add_global_variable(global_variable);
}

void configuration::remove_global_variable(string8_view name) {
  this->globals_to_remove_.emplace_back(name);
}

void configuration::load_from_json(padded_string_view json,
                                   error_reporter* reporter) {
  std::error_code error;
  ::boost::json::value root = ::boost::json::parse(
      to_string_view(json.string_view()),
      error);
  if (error) {
    this->report_json_error(json, reporter);
    return;
  }

  if (!root.is_object()) {
    // TODO(strager): Report a more helpful error.
    this->report_json_error(json, reporter);
    return;
  }
  ::boost::json::object& root_object = root.as_object();

  if (::boost::json::value *global_groups_value = root_object.if_contains("global-groups")) {
    if (!this->load_global_groups_from_json(*global_groups_value, reporter)) {
      this->report_json_error(json, reporter);
      return;
    }
  }

  if (::boost::json::value *globals_value = root_object.if_contains("globals")) {
    if (::boost::json::object *globals_object = globals_value->if_object()) {
      if (!this->load_globals_from_json(*globals_object, reporter)) {
        this->report_json_error(json, reporter);
        return;
      }
    } else {
      /*@@@
      reporter->report(error_config_globals_type_mismatch{
          .value = span_of_json_value(v),
      });
      */
    }
  }
}

void configuration::reset() {
  // TODO(strager): Make this more efficient by avoiding reallocations.
  this->globals_ = global_declared_variable_set();
  this->globals_to_remove_.clear();
  this->did_add_globals_from_groups_ = false;
  for (bool& enabled : this->enabled_global_groups_) {
    enabled = true;
  }
  this->string_allocator_.memory_resource()->release();
}

bool configuration::load_global_groups_from_json(
    ::boost::json::value& global_groups_value,
    error_reporter* reporter) {
  if (bool* global_groups_bool_value = global_groups_value.if_bool()) {
    if (*global_groups_bool_value) {
      // Do nothing.
    } else {
      reset_global_groups();
    }
  } else if (::boost::json::array* global_groups_array_value = global_groups_value.if_array()) {
    this->reset_global_groups();
    for (::boost::json::value& 
             global_group_value : *global_groups_array_value) {
      if (::boost::json::string* global_group_string_value = global_group_value.if_string()) {
        this->add_global_group(to_string8_view(
              std::string_view(*global_group_string_value)));
      } else {
        /*@@@
        reporter->report(error_config_global_groups_group_type_mismatch{
            .group = span_of_json_value(global_group_value),
        });
        */
      }
    }
  } else {
    /*@@@
    reporter->report(error_config_global_groups_type_mismatch{
        .value = span_of_json_value(global_groups_value),
    });
    */
  }
  return true;
}

bool configuration::load_globals_from_json(
    ::boost::json::object& globals_value, error_reporter* reporter) {
  for (boost::json::key_value_pair& global_field : globals_value) {
    string8_view global_name = this->save_string(global_field.key());

    ::boost::json::value& descriptor = global_field.value();
    if (bool* descriptor_bool = descriptor.if_bool()) {
      if (*descriptor_bool) {
        add_global_variable(global_declared_variable{
            .name = global_name,
            .is_writable = true,
            .is_shadowable = true,
        });
      } else {
        remove_global_variable(global_name);
      }
    } else if (::boost::json::object* descriptor_object = descriptor.if_object()) {
      bool is_shadowable;
      bool is_writable;
      bool ok = true;
      if (!this->get_bool_or_default<
              error_config_globals_descriptor_shadowable_type_mismatch>(
              descriptor_object->if_contains("shadowable"), &is_shadowable, true,
              reporter)) {
        ok = false;
      }
      if (!this->get_bool_or_default<
              error_config_globals_descriptor_writable_type_mismatch>(
              descriptor_object->if_contains("writable"), &is_writable, true, reporter)) {
        ok = false;
      }
      this->add_global_variable(global_declared_variable{
          .name = global_name,
          .is_writable = is_writable,
          .is_shadowable = is_shadowable,
      });
      if (!ok) {
        return false;
      }
    } else {
      /*@@@
      reporter->report(error_config_globals_descriptor_type_mismatch{
          .descriptor = span_of_json_value(descriptor),
      });
      */
    }
  }
  return true;
}

string8_view configuration::save_string(std::string_view s) {
  string8_view s8 = to_string8_view(s);
  char8* out_begin =
      string_allocator_.allocate_uninitialized_array<char8>(s8.size());
  std::uninitialized_copy(s8.begin(), s8.end(), out_begin);
  return string8_view(out_begin, s8.size());
}

bool configuration::should_remove_global_variable(string8_view name) {
  return std::find(this->globals_to_remove_.begin(),
                   this->globals_to_remove_.end(),
                   name) != this->globals_to_remove_.end();
}

[[gnu::noinline]] void configuration::build_globals_from_groups() {
  QLJS_ASSERT(!this->did_add_globals_from_groups_);

  auto iterate_globals = [](const char8* globals, auto&& func) -> void {
    for (const char8* it = globals; *it != '\0';) {
      string8_view global(it);
      func(global);
      it += global.size() + 1;
    }
  };

  auto add_globals = [&]([[maybe_unused]] const global_group& group,
                         const char8* group_globals, bool shadowable,
                         bool writable,
                         std::int16_t expected_globals_count) -> void {
    if (!group_globals) {
      QLJS_ASSERT(expected_globals_count == 0);
      return;
    }

    this->globals_.reserve_more_global_variables(expected_globals_count,
                                                 /*is_shadowable=*/shadowable,
                                                 /*is_writable=*/writable);
    iterate_globals(group_globals, [&](string8_view global) {
      if (!this->should_remove_global_variable(global)) {
        this->globals_.add_global_variable(global_declared_variable{
            .name = global,
            .is_writable = writable,
            .is_shadowable = shadowable,
        });
      }
    });

#if !NDEBUG
    int actual_globals_count = 0;
    iterate_globals(group_globals,
                    [&](string8_view) { actual_globals_count += 1; });
    if (actual_globals_count != expected_globals_count) {
      std::ptrdiff_t group_index = &group - global_groups;
      // clang-format off
      const char* prefix =
          group.globals == group_globals ? "" :
          group.non_shadowable_globals == group_globals ? "non_shadowable_" :
          group.non_writable_globals == group_globals ? "non_writable_" :
          "???";
      // clang-format on
      std::fprintf(
          stderr,
          "fatal: global_groups (global-variables.cpp) is out of date.\n"
          "       For global_groups[%zd] (.name=%s),\n"
          "       .%sglobals contains %d strings, but\n"
          "       .%sglobals_count is %d.\n",
          group_index, reinterpret_cast<const char*>(group.name), prefix,
          actual_globals_count, prefix,
          narrow_cast<int>(expected_globals_count));
      QLJS_ASSERT(false);
    }
#endif
  };
  for (std::size_t i = 0; i < this->enabled_global_groups_.size(); ++i) {
    bool enabled = this->enabled_global_groups_[i];
    if (enabled) {
      const global_group& group = global_groups[i];
      add_globals(group, group.globals, true, true, group.globals_count);
      add_globals(group, group.non_shadowable_globals, false, true,
                  group.non_shadowable_globals_count);
      add_globals(group, group.non_writable_globals, true, false,
                  group.non_writable_globals_count);
    }
  }

  this->did_add_globals_from_groups_ = true;
}

template <class Error>
bool configuration::get_bool_or_default(
    ::boost::json::value* value, bool* out,
    bool default_value, error_reporter* reporter) {
  if (value) {
    if (bool* bool_value = value->if_bool()) {
      *out = *bool_value;
      return true;
    } else {
      /*@@@
      reporter->report(Error{span_of_json_value(v)});
      */
      *out = default_value;
      return true;
    }
  } else {
    *out = default_value;
    return true;
  }
}

void configuration::report_json_error(padded_string_view json,
                                      error_reporter* reporter) {
  // TODO(strager): Produce better error messages. simdjson provides no location
  // information for errors:
  // https://github.com/simdjson/simdjson/issues/237
  reporter->report(error_config_json_syntax_error{
      .where = source_code_span(json.data(), json.data()),
  });
}

namespace {
string8_view remove_trailing_json_whitespace(string8_view sv) {
  QLJS_ASSERT(!sv.empty());
  // According to RFC 8259, whitespace characters are U+0009, U+000A, U+000D,
  // and U+0020.
  std::size_t last_character_index =
      sv.find_last_not_of(u8"\u0009\u000a\u000d\u0020"sv);
  QLJS_ASSERT(last_character_index != sv.npos);
  return sv.substr(0, last_character_index + 1);
}

source_code_span span_of_json_value(
    ::simdjson::fallback::ondemand::value& value) {
  string8_view sv = to_string8_view(value.raw_json_token());
  sv = remove_trailing_json_whitespace(sv);
  return source_code_span(sv.data(), sv.data() + sv.size());
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
