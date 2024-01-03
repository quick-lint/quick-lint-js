// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <array>
#include <quick-lint-js/configuration/configuration.h>
#include <quick-lint-js/diag/diag-list.h>
#include <quick-lint-js/fe/global-declared-variable-set.h>
#include <quick-lint-js/fe/global-variables.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/algorithm.h>
#include <quick-lint-js/util/cast.h>
#include <simdjson.h>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
Source_Code_Span span_of_json_value(::simdjson::ondemand::value&);

// Returns false on parse error, and true otherwise.
template <class Error>
static bool get_bool_or_default(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value, bool* out,
    bool default_value, Diag_List*);
}

Configuration::Configuration() { this->reset(); }

const Global_Declared_Variable_Set& Configuration::globals() {
  if (!this->did_add_globals_from_groups_) {
    this->build_globals_from_groups();
  }
  return this->globals_;
}

void Configuration::reset_global_groups() {
  for (bool& enabled : this->enabled_global_groups_) {
    enabled = false;
  }
}

bool Configuration::add_global_group(String8_View group_name) {
  if (group_name == u8"literally-anything"_sv) {
    this->literally_anything_global_group_enabled_ = true;
    return true;
  }
  for (std::size_t i = 0; i < this->enabled_global_groups_.size(); ++i) {
    if (group_name == global_groups[i].name) {
      this->enabled_global_groups_[i] = true;
      return true;
    }
  }
  return false;
}

void Configuration::add_global_variable(
    Global_Declared_Variable global_variable) {
  this->remove_global_variable(global_variable.name);
  this->globals_.add_global_variable(global_variable);
}

void Configuration::remove_global_variable(String8_View name) {
  this->globals_to_remove_.emplace_back(name);
}

void Configuration::load_from_json(Padded_String_View json,
                                   Diag_List* out_diags) {
  ::simdjson::ondemand::parser json_parser;
  ::simdjson::ondemand::document document;
  ::simdjson::error_code parse_error =
      json_parser
          .iterate(reinterpret_cast<const char*>(json.data()),
                   narrow_cast<std::size_t>(json.size()),
                   narrow_cast<std::size_t>(json.padded_size()))
          .get(document);
  if (parse_error != ::simdjson::SUCCESS) {
    this->report_json_error(json, out_diags);
    return;
  }

  ::simdjson::ondemand::value global_groups_value;
  switch (document["global-groups"].get(global_groups_value)) {
  case ::simdjson::error_code::SUCCESS:
    if (!this->load_global_groups_from_json(global_groups_value, out_diags)) {
      this->report_json_error(json, out_diags);
      return;
    }
    break;

  case ::simdjson::error_code::NO_SUCH_FIELD:
    break;

  default:
    this->report_json_error(json, out_diags);
    return;
  }

  auto globals = document["globals"];
  ::simdjson::ondemand::object globals_value;
  switch (globals.get(globals_value)) {
  case ::simdjson::error_code::SUCCESS:
    if (!this->load_globals_from_json(globals_value, out_diags)) {
      this->report_json_error(json, out_diags);
      return;
    }
    break;

  case ::simdjson::error_code::INCORRECT_TYPE: {
    // Either "globals" has the wrong type or there is a syntax error. simdjson
    // gives us INCORRECT_TYPE in both cases.
    ::simdjson::ondemand::value v;
    if (globals.get(v) == ::simdjson::SUCCESS &&
        v.type().error() == ::simdjson::SUCCESS) {
      out_diags->add(Diag_Config_Globals_Type_Mismatch{
          .value = span_of_json_value(v),
      });
    } else {
      this->report_json_error(json, out_diags);
      return;
    }
    break;
  }

  case ::simdjson::error_code::NO_SUCH_FIELD:
    break;

  default:
    this->report_json_error(json, out_diags);
    return;
  }
}

void Configuration::reset() {
  this->globals_.clear();
  this->globals_to_remove_.clear();
  this->did_add_globals_from_groups_ = false;
  for (bool& enabled : this->enabled_global_groups_) {
    enabled = true;
  }
  this->literally_anything_global_group_enabled_ = false;
  this->allocator_.release();
}

bool Configuration::load_global_groups_from_json(
    ::simdjson::ondemand::value& global_groups_value, Diag_List* out_diags) {
  ::simdjson::ondemand::json_type global_groups_value_type;
  if (global_groups_value.type().get(global_groups_value_type) !=
      ::simdjson::SUCCESS) {
    return false;
  }
  switch (global_groups_value_type) {
  case ::simdjson::ondemand::json_type::boolean: {
    bool global_groups_bool_value;
    if (global_groups_value.get_bool().get(global_groups_bool_value) !=
        ::simdjson::SUCCESS) {
      return false;
    }
    if (global_groups_bool_value) {
      // Do nothing.
    } else {
      reset_global_groups();
    }
    break;
  }

  case ::simdjson::ondemand::json_type::array: {
    ::simdjson::ondemand::array global_groups_array_value;
    if (global_groups_value.get(global_groups_array_value) !=
        ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }

    this->reset_global_groups();
    for (::simdjson::simdjson_result<::simdjson::ondemand::value>
             global_group_value_or_error : global_groups_array_value) {
      ::simdjson::ondemand::value global_group_value;
      if (global_group_value_or_error.get(global_group_value) !=
          ::simdjson::SUCCESS) {
        return false;
      }
      std::string_view global_group_string_value;
      switch (global_group_value.get(global_group_string_value)) {
      case ::simdjson::SUCCESS:
        this->add_global_group(to_string8_view(global_group_string_value));
        break;

      case ::simdjson::INCORRECT_TYPE:
        // If simdjson gives us an INCORRECT_TYPE error, it's possible that we
        // reached the end of the file. Check whether this is an incorrect
        // type or malformed JSON.
        if (global_group_value.type().error() != ::simdjson::SUCCESS) {
          return false;
        }
        out_diags->add(Diag_Config_Global_Groups_Group_Type_Mismatch{
            .group = span_of_json_value(global_group_value),
        });
        break;

      default:
        return false;
      }
    }
    break;
  }

  case ::simdjson::ondemand::json_type::null:
  case ::simdjson::ondemand::json_type::number:
  case ::simdjson::ondemand::json_type::object:
  case ::simdjson::ondemand::json_type::string:
    out_diags->add(Diag_Config_Global_Groups_Type_Mismatch{
        .value = span_of_json_value(global_groups_value),
    });
    break;
  }
  return true;
}

bool Configuration::load_globals_from_json(
    ::simdjson::ondemand::object& globals_value, Diag_List* out_diags) {
  for (simdjson::simdjson_result<::simdjson::ondemand::field> global_field :
       globals_value) {
    std::string_view key;
    if (global_field.unescaped_key().get(key) != ::simdjson::SUCCESS) {
      return false;
    }
    String8_View global_name = this->save_string(key);

    ::simdjson::ondemand::value descriptor;
    if (global_field.value().get(descriptor) != ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }
    ::simdjson::ondemand::json_type descriptor_type;
    if (descriptor.type().get(descriptor_type) != ::simdjson::SUCCESS) {
      return false;
    }
    switch (descriptor_type) {
    case ::simdjson::ondemand::json_type::boolean: {
      bool descriptor_bool;
      if (descriptor.get(descriptor_bool) != ::simdjson::SUCCESS) {
        return false;
      }
      if (descriptor_bool) {
        add_global_variable(Global_Declared_Variable{
            .name = global_name,
            .is_writable = true,
            .is_shadowable = true,
            .is_type_only = false,
        });
      } else {
        remove_global_variable(global_name);
      }
      break;
    }

    case ::simdjson::ondemand::json_type::object: {
      ::simdjson::ondemand::object descriptor_object;
      if (descriptor.get(descriptor_object) != ::simdjson::SUCCESS) {
        QLJS_UNIMPLEMENTED();
      }

      bool is_shadowable;
      bool is_writable;
      bool ok = true;
      if (!get_bool_or_default<
              Diag_Config_Globals_Descriptor_Shadowable_Type_Mismatch>(
              descriptor_object["shadowable"], &is_shadowable, true,
              out_diags)) {
        ok = false;
      }
      if (!get_bool_or_default<
              Diag_Config_Globals_Descriptor_Writable_Type_Mismatch>(
              descriptor_object["writable"], &is_writable, true, out_diags)) {
        ok = false;
      }
      this->add_global_variable(Global_Declared_Variable{
          .name = global_name,
          .is_writable = is_writable,
          .is_shadowable = is_shadowable,
          .is_type_only = false,
      });
      if (!ok) {
        return false;
      }

      break;
    }

    default:
      out_diags->add(Diag_Config_Globals_Descriptor_Type_Mismatch{
          .descriptor = span_of_json_value(descriptor),
      });
      break;
    }
  }
  return true;
}

String8_View Configuration::save_string(std::string_view s) {
  String8_View s8 = to_string8_view(s);
  // TODO(strager): Use Linked_Bump_Allocator::new_objects_copy.
  Span<Char8> out =
      this->allocator_.allocate_uninitialized_span<Char8>(s8.size());
  std::uninitialized_copy(s8.begin(), s8.end(), out.data());
  return String8_View(out.data(), narrow_cast<std::size_t>(out.size()));
}

bool Configuration::should_remove_global_variable(String8_View name) {
  return contains(this->globals_to_remove_, name);
}

[[gnu::noinline]] void Configuration::build_globals_from_groups() {
  QLJS_ASSERT(!this->did_add_globals_from_groups_);

  if (this->literally_anything_global_group_enabled_) {
    this->globals_.add_literally_everything();
  }

  auto iterate_globals = [](const Char8* globals, auto&& func) -> void {
    for (const Char8* it = globals; *it != '\0';) {
      String8_View global(it);
      func(global);
      it += global.size() + 1;
    }
  };

  auto add_globals = [&]([[maybe_unused]] const Global_Group& group,
                         const Char8* group_globals, bool shadowable,
                         bool writable, bool type_only,
                         std::int16_t expected_globals_count) -> void {
    if (!group_globals) {
      QLJS_ASSERT(expected_globals_count == 0);
      return;
    }

    this->globals_.reserve_more_global_variables(
        narrow_cast<std::size_t>(expected_globals_count),
        /*is_shadowable=*/shadowable,
        /*is_writable=*/writable);
    iterate_globals(group_globals, [&](String8_View global) {
      if (!this->should_remove_global_variable(global)) {
        this->globals_.add_global_variable(Global_Declared_Variable{
            .name = global,
            .is_writable = writable,
            .is_shadowable = shadowable,
            .is_type_only = type_only,
        });
      }
    });

#if !(defined(NDEBUG) && NDEBUG)
    int actual_globals_count = 0;
    iterate_globals(group_globals,
                    [&](String8_View) { actual_globals_count += 1; });
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
      const Global_Group& group = global_groups[i];
      add_globals(group, group.globals, true, true, false, group.globals_count);
      add_globals(group, group.non_shadowable_globals, false, true, false,
                  group.non_shadowable_globals_count);
      add_globals(group, group.non_writable_globals, true, false, false,
                  group.non_writable_globals_count);
      add_globals(group, group.type_only_globals, true, false, true,
                  group.type_only_globals_count);
    }
  }

  this->did_add_globals_from_groups_ = true;
}

void Configuration::report_json_error(Padded_String_View json,
                                      Diag_List* out_diags) {
  // TODO(strager): Produce better error messages. simdjson provides no location
  // information for errors:
  // https://github.com/simdjson/simdjson/issues/237
  out_diags->add(Diag_Config_Json_Syntax_Error{
      .where = Source_Code_Span::unit(json.data()),
  });
}

namespace {
String8_View remove_trailing_json_whitespace(String8_View sv) {
  QLJS_ASSERT(!sv.empty());
  // According to RFC 8259, whitespace characters are U+0009, U+000A, U+000D,
  // and U+0020.
  std::size_t last_character_index =
      sv.find_last_not_of(u8"\u0009\u000a\u000d\u0020"_sv);
  QLJS_ASSERT(last_character_index != sv.npos);
  return sv.substr(0, last_character_index + 1);
}

Source_Code_Span span_of_json_value(::simdjson::ondemand::value& value) {
  String8_View sv = to_string8_view(value.raw_json_token());
  sv = remove_trailing_json_whitespace(sv);
  return Source_Code_Span(sv.data(), sv.data() + sv.size());
}

template <class Error>
bool get_bool_or_default(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value, bool* out,
    bool default_value, Diag_List* out_diags) {
  ::simdjson::ondemand::value v;
  ::simdjson::error_code error = value.get(v);
  switch (error) {
  case ::simdjson::SUCCESS:
    if (v.get(*out) != ::simdjson::SUCCESS) {
      out_diags->add(Error{span_of_json_value(v)});
      *out = default_value;
    }
    return true;

  default:
    *out = default_value;
    return false;

  case ::simdjson::NO_SUCH_FIELD:
    *out = default_value;
    return true;
  }
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
