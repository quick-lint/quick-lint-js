// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <algorithm>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/configuration.h>
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
::simdjson::error_code get_bool_or_default(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&&, bool* out,
    bool default_value);
}

const global_declared_variable_set& configuration::globals() noexcept {
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

  return this->globals_;
}

const std::optional<canonical_path>& configuration::config_file_path() const {
  return this->config_file_path_;
}

void configuration::reset_global_groups() {
  this->add_global_group_node_js_ = false;
  this->add_global_group_ecmascript_ = false;
}

bool configuration::add_global_group(string8_view group_name) {
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

void configuration::load_from_json(padded_string_view json) {
  ::simdjson::ondemand::parser json_parser;
  ::simdjson::ondemand::document document;
  ::simdjson::error_code parse_error =
      json_parser
          .iterate(reinterpret_cast<const char*>(json.data()),
                   narrow_cast<std::size_t>(json.size()),
                   narrow_cast<std::size_t>(json.padded_size()))
          .get(document);
  if (parse_error != ::simdjson::error_code::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }

  ::simdjson::ondemand::value global_groups_value;
  switch (document["global-groups"].get(global_groups_value)) {
  case ::simdjson::error_code::SUCCESS:
    this->load_global_groups_from_json(global_groups_value);
    break;

  case ::simdjson::error_code::NO_SUCH_FIELD:
    break;

  default:
    QLJS_UNIMPLEMENTED();
    break;
  }

  ::simdjson::ondemand::object globals_value;
  switch (document["globals"].get(globals_value)) {
  case ::simdjson::error_code::SUCCESS:
    this->load_globals_from_json(globals_value);
    break;

  case ::simdjson::error_code::NO_SUCH_FIELD:
    break;

  default:
    QLJS_UNIMPLEMENTED();
    break;
  }
}

void configuration::set_config_file_path(canonical_path&& path) {
  this->config_file_path_ = std::move(path);
}

void configuration::set_config_file_path(const canonical_path& path) {
  this->config_file_path_ = path;
}

void configuration::reset() {
  // TODO(strager): Make this more efficient by avoiding reallocations.
  this->globals_ = global_declared_variable_set();
  this->globals_to_remove_.clear();
  this->add_global_group_node_js_ = true;
  this->add_global_group_ecmascript_ = true;
  this->string_allocator_.memory_resource()->release();
}

void configuration::load_global_groups_from_json(
    ::simdjson::ondemand::value& global_groups_value) {
  ::simdjson::fallback::ondemand::json_type global_groups_value_type;
  if (global_groups_value.type().get(global_groups_value_type) !=
      ::simdjson::SUCCESS) {
    QLJS_UNIMPLEMENTED();
  }
  switch (global_groups_value_type) {
  case ::simdjson::fallback::ondemand::json_type::boolean: {
    bool global_groups_bool_value;
    if (global_groups_value.get_bool().get(global_groups_bool_value) !=
        ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
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
             global_group_value : global_groups_array_value) {
      std::string_view global_group_string_value;
      if (global_group_value.get(global_group_string_value) !=
          ::simdjson::SUCCESS) {
        QLJS_UNIMPLEMENTED();
      }
      this->add_global_group(to_string8_view(global_group_string_value));
    }
    break;
  }

  default:
    QLJS_UNIMPLEMENTED();
    break;
  }
}

void configuration::load_globals_from_json(
    ::simdjson::ondemand::object& globals_value) {
  for (simdjson::simdjson_result<::simdjson::fallback::ondemand::field>
           global_field : globals_value) {
    std::string_view key;
    if (global_field.unescaped_key().get(key) != ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }
    string8_view global_name = this->save_string(key);

    ::simdjson::fallback::ondemand::value descriptor;
    if (global_field.value().get(descriptor) != ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }
    ::simdjson::fallback::ondemand::json_type descriptor_type;
    if (descriptor.type().get(descriptor_type) != ::simdjson::SUCCESS) {
      QLJS_UNIMPLEMENTED();
    }
    switch (descriptor_type) {
    case ::simdjson::fallback::ondemand::json_type::boolean: {
      bool descriptor_bool;
      if (descriptor.get(descriptor_bool) != ::simdjson::SUCCESS) {
        QLJS_UNIMPLEMENTED();
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
      if (get_bool_or_default(descriptor_object["shadowable"],
                              &var->is_shadowable,
                              true) != ::simdjson::SUCCESS) {
        QLJS_UNIMPLEMENTED();
      }
      if (get_bool_or_default(descriptor_object["writable"], &var->is_writable,
                              true) != ::simdjson::SUCCESS) {
        QLJS_UNIMPLEMENTED();
      }

      break;
    }

    default:
      QLJS_UNIMPLEMENTED();
      break;
    }
  }
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

namespace {
::simdjson::error_code get_bool_or_default(
    ::simdjson::simdjson_result<::simdjson::ondemand::value>&& value, bool* out,
    bool default_value) {
  ::simdjson::error_code error = value.get(*out);
  switch (error) {
  case ::simdjson::SUCCESS:
  default:
    return error;

  case ::simdjson::NO_SUCH_FIELD:
    *out = default_value;
    return ::simdjson::SUCCESS;
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
