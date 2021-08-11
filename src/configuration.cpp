// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
#include <quick-lint-js/error-reporter.h>
#include <quick-lint-js/global-variables.h>
#include <quick-lint-js/lint.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/warning.h>
#include <simdjson.h>
#include <vector>

QLJS_WARNING_IGNORE_GCC("-Wuseless-cast")

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
namespace {
source_code_span span_of_json_value(::simdjson::fallback::ondemand::value&);
}

const global_declared_variable_set& configuration::globals() noexcept {
  if (this->did_add_globals_from_groups_) {
    return this->globals_;
  }

  if (this->add_global_group_browser_) {
    for (const char8** it = global_variables_browser; *it; ++it) {
      string8_view global(*it);
      if (!this->should_remove_global_variable(global)) {
        this->globals_.add_variable(global);
      }
    }
  }

  if (this->add_global_group_ecmascript_) {
    const char8* writable_global_variables[] = {
        // ECMA-262 18.1 Value Properties of the Global Object
        u8"globalThis",

        // ECMA-262 18.2 Function Properties of the Global Object
        u8"decodeURI",
        u8"decodeURIComponent",
        u8"encodeURI",
        u8"encodeURIComponent",
        u8"eval",
        u8"isFinite",
        u8"isNaN",
        u8"parseFloat",
        u8"parseInt",

        // ECMA-262 18.3 Constructor Properties of the Global Object
        u8"Array",
        u8"ArrayBuffer",
        u8"BigInt",
        u8"BigInt64Array",
        u8"BigUint64Array",
        u8"Boolean",
        u8"DataView",
        u8"Date",
        u8"Error",
        u8"EvalError",
        u8"Float32Array",
        u8"Float64Array",
        u8"Function",
        u8"Int16Array",
        u8"Int32Array",
        u8"Int8Array",
        u8"Map",
        u8"Number",
        u8"Object",
        u8"Promise",
        u8"Proxy",
        u8"RangeError",
        u8"ReferenceError",
        u8"RegExp",
        u8"Set",
        u8"SharedArrayBuffer",
        u8"String",
        u8"Symbol",
        u8"SyntaxError",
        u8"TypeError",
        u8"URIError",
        u8"Uint16Array",
        u8"Uint32Array",
        u8"Uint8Array",
        u8"Uint8ClampedArray",
        u8"WeakMap",
        u8"WeakSet",

        // ECMA-262 18.4 Other Properties of the Global Object
        u8"Atomics",
        u8"JSON",
        u8"Math",
        u8"Reflect",
    };
    for (string8_view global_variable : writable_global_variables) {
      if (!this->should_remove_global_variable(global_variable)) {
        this->globals_.add_variable(global_variable);
      }
    }

    const char8* non_writable_global_variables[] = {
        // ECMA-262 18.1 Value Properties of the Global Object
        u8"Infinity",
        u8"NaN",
        u8"undefined",
    };
    for (string8_view global_variable : non_writable_global_variables) {
      if (!this->should_remove_global_variable(global_variable)) {
        global_declared_variable* var =
            this->globals_.add_variable(global_variable);
        var->is_writable = false;
      }
    }
  }

  if (this->add_global_group_node_js_) {
    const char8* writable_global_variables[] = {
        u8"Buffer",          u8"GLOBAL",       u8"Intl",
        u8"TextDecoder",     u8"TextEncoder",  u8"URL",
        u8"URLSearchParams", u8"WebAssembly",  u8"clearImmediate",
        u8"clearInterval",   u8"clearTimeout", u8"console",
        u8"escape",          u8"global",       u8"process",
        u8"queueMicrotask",  u8"root",         u8"setImmediate",
        u8"setInterval",     u8"setTimeout",   u8"unescape",
    };
    for (string8_view global_variable : writable_global_variables) {
      if (!this->should_remove_global_variable(global_variable)) {
        this->globals_.add_variable(global_variable);
      }
    }

    const char8* writable_module_variables[] = {
        u8"__dirname", u8"__filename", u8"exports", u8"module", u8"require",
    };
    for (string8_view module_variable : writable_module_variables) {
      if (!this->should_remove_global_variable(module_variable)) {
        global_declared_variable* var =
            this->globals_.add_variable(module_variable);
        var->is_shadowable = false;
      }
    }
  }

  this->did_add_globals_from_groups_ = true;
  return this->globals_;
}

void configuration::reset_global_groups() {
  this->add_global_group_browser_ = false;
  this->add_global_group_node_js_ = false;
  this->add_global_group_ecmascript_ = false;
}

bool configuration::add_global_group(string8_view group_name) {
  if (group_name == u8"browser"sv) {
    this->add_global_group_browser_ = true;
    return true;
  }
  if (group_name == u8"ecmascript"sv) {
    this->add_global_group_ecmascript_ = true;
    return true;
  }
  if (group_name == u8"node.js"sv) {
    this->add_global_group_node_js_ = true;
    return true;
  }
  return false;
}

global_declared_variable* configuration::add_global_variable(
    string8_view name) {
  return this->globals_.add_variable(name);
}

void configuration::remove_global_variable(string8_view name) {
  this->globals_to_remove_.emplace_back(name);
}

void configuration::load_from_json(padded_string_view json,
                                   error_reporter* reporter) {
  ::simdjson::ondemand::parser json_parser;
  ::simdjson::ondemand::document document;
  ::simdjson::error_code parse_error =
      json_parser
          .iterate(reinterpret_cast<const char*>(json.data()),
                   narrow_cast<std::size_t>(json.size()),
                   narrow_cast<std::size_t>(json.padded_size()))
          .get(document);
  if (parse_error != ::simdjson::error_code::SUCCESS) {
    this->report_json_error(json, reporter);
    return;
  }

  ::simdjson::ondemand::value global_groups_value;
  switch (document["global-groups"].get(global_groups_value)) {
  case ::simdjson::error_code::SUCCESS:
    if (!this->load_global_groups_from_json(global_groups_value, reporter)) {
      this->report_json_error(json, reporter);
      return;
    }
    break;

  case ::simdjson::error_code::NO_SUCH_FIELD:
    break;

  default:
    this->report_json_error(json, reporter);
    return;
  }

  auto globals = document["globals"];
  ::simdjson::ondemand::object globals_value;
  switch (globals.get(globals_value)) {
  case ::simdjson::error_code::SUCCESS:
    if (!this->load_globals_from_json(globals_value, reporter)) {
      this->report_json_error(json, reporter);
      return;
    }
    break;

  case ::simdjson::error_code::INCORRECT_TYPE: {
    // Either "globals" has the wrong type or there is a syntax error. simdjson
    // gives us INCORRECT_TYPE in both cases.
    ::simdjson::ondemand::value v;
    if (globals.get(v) == ::simdjson::SUCCESS &&
        v.type().error() == ::simdjson::SUCCESS) {
      reporter->report(error_config_globals_type_mismatch{
          .value = span_of_json_value(v),
      });
    } else {
      this->report_json_error(json, reporter);
      return;
    }
    break;
  }

  case ::simdjson::error_code::NO_SUCH_FIELD:
    break;

  default:
    this->report_json_error(json, reporter);
    return;
  }
}

void configuration::reset() {
  // TODO(strager): Make this more efficient by avoiding reallocations.
  this->globals_ = global_declared_variable_set();
  this->globals_to_remove_.clear();
  this->add_global_group_node_js_ = true;
  this->add_global_group_ecmascript_ = true;
  this->string_allocator_.memory_resource()->release();
}

bool configuration::load_global_groups_from_json(
    ::simdjson::ondemand::value& global_groups_value,
    error_reporter* reporter) {
  ::simdjson::fallback::ondemand::json_type global_groups_value_type;
  if (global_groups_value.type().get(global_groups_value_type) !=
      ::simdjson::SUCCESS) {
    return false;
  }
  switch (global_groups_value_type) {
  case ::simdjson::fallback::ondemand::json_type::boolean: {
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

  case ::simdjson::fallback::ondemand::json_type::array: {
    ::simdjson::fallback::ondemand::array global_groups_array_value;
    if (global_groups_value.get(global_groups_array_value) !=
        ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }

    this->reset_global_groups();
    for (::simdjson::simdjson_result<::simdjson::fallback::ondemand::value>
             global_group_value_or_error : global_groups_array_value) {
      ::simdjson::fallback::ondemand::value global_group_value;
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
        reporter->report(error_config_global_groups_group_type_mismatch{
            .group = span_of_json_value(global_group_value),
        });
        break;

      default:
        return false;
      }
    }
    break;
  }

  case ::simdjson::fallback::ondemand::json_type::null:
  case ::simdjson::fallback::ondemand::json_type::number:
  case ::simdjson::fallback::ondemand::json_type::object:
  case ::simdjson::fallback::ondemand::json_type::string:
    reporter->report(error_config_global_groups_type_mismatch{
        .value = span_of_json_value(global_groups_value),
    });
    break;
  }
  return true;
}

bool configuration::load_globals_from_json(
    ::simdjson::ondemand::object& globals_value, error_reporter* reporter) {
  for (simdjson::simdjson_result<::simdjson::fallback::ondemand::field>
           global_field : globals_value) {
    std::string_view key;
    if (global_field.unescaped_key().get(key) != ::simdjson::SUCCESS) {
      return false;
    }
    string8_view global_name = this->save_string(key);

    ::simdjson::fallback::ondemand::value descriptor;
    if (global_field.value().get(descriptor) != ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }
    ::simdjson::fallback::ondemand::json_type descriptor_type;
    if (descriptor.type().get(descriptor_type) != ::simdjson::SUCCESS) {
      return false;
    }
    switch (descriptor_type) {
    case ::simdjson::fallback::ondemand::json_type::boolean: {
      bool descriptor_bool;
      if (descriptor.get(descriptor_bool) != ::simdjson::SUCCESS) {
        return false;
      }
      if (descriptor_bool) {
        add_global_variable(global_name);
      } else {
        remove_global_variable(global_name);
      }
      break;
    }

    case ::simdjson::fallback::ondemand::json_type::object: {
      ::simdjson::fallback::ondemand::object descriptor_object;
      if (descriptor.get(descriptor_object) != ::simdjson::SUCCESS) {
        QLJS_UNIMPLEMENTED();
      }

      global_declared_variable* var = add_global_variable(global_name);
      if (!this->get_bool_or_default<
              error_config_globals_descriptor_shadowable_type_mismatch>(
              descriptor_object["shadowable"], &var->is_shadowable, true,
              reporter)) {
        return false;
      }
      if (!this->get_bool_or_default<
              error_config_globals_descriptor_writable_type_mismatch>(
              descriptor_object["writable"], &var->is_writable, true,
              reporter)) {
        return false;
      }

      break;
    }

    default:
      reporter->report(error_config_globals_descriptor_type_mismatch{
          .descriptor = span_of_json_value(descriptor),
      });
      break;
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

template <class Error>
bool configuration::get_bool_or_default(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value, bool* out,
    bool default_value, error_reporter* reporter) {
  ::simdjson::fallback::ondemand::value v;
  ::simdjson::error_code error = value.get(v);
  switch (error) {
  case ::simdjson::SUCCESS: {
    ::simdjson::fallback::ondemand::json_type type;
    if (v.type().get(type) != ::simdjson::SUCCESS) {
      return false;
    }
    if (type != ::simdjson::fallback::ondemand::json_type::boolean) {
      reporter->report(Error{span_of_json_value(v)});
      *out = default_value;
      return true;
    }
    if (v.get(*out) != ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }
    return true;
  }

  default:
    return false;

  case ::simdjson::NO_SUCH_FIELD:
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
