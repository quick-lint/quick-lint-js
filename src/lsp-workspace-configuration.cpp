// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#if defined(__EMSCRIPTEN__)
// No LSP on the web.
#else

#include <quick-lint-js/byte-buffer.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/heap-function.h>
#include <quick-lint-js/lsp-endpoint.h>
#include <quick-lint-js/lsp-workspace-configuration.h>
#include <simdjson.h>
#include <string>
#include <vector>

using namespace std::literals::string_view_literals;

namespace quick_lint_js {
void lsp_workspace_configuration::add_item(
    string8_view name, heap_function<void(std::string_view)>&& callback) {
  this->items_.push_back(item{
      .name = name,
      .callback = std::move(callback),
  });
}

void lsp_workspace_configuration::build_request(
    lsp_endpoint_handler::request_id_type request_id,
    byte_buffer& request_json) {
  request_json.append_copy(u8R"--({"id":)--"sv);
  request_json.append_decimal_integer(request_id);
  // clang-format off
  request_json.append_copy(
    u8R"--(,)--"
    u8R"--("method":"workspace/configuration",)--"
    u8R"--("params":{)--"
      u8R"--("items":[)--"sv);
  // clang-format on
  bool need_comma = false;
  for (const item& i : this->items_) {
    if (need_comma) {
      request_json.append_copy(u8',');
    }
    request_json.append_copy(u8R"({"section":")"sv);
    request_json.append_copy(i.name);
    request_json.append_copy(u8R"("})"sv);
    need_comma = true;
  }
  request_json.append_copy(u8R"--(]},"jsonrpc":"2.0"})--"sv);
}

bool lsp_workspace_configuration::process_response(
    ::simdjson::ondemand::value result) {
  ::simdjson::ondemand::array result_array;
  if (result.get_array().get(result_array) != ::simdjson::SUCCESS) {
    return false;
  }

  auto spec_it = this->items_.begin();
  auto spec_end = this->items_.end();
  ::simdjson::ondemand::array_iterator result_it;
  ::simdjson::ondemand::array_iterator result_end;
  if (result_array.begin().get(result_it) != ::simdjson::SUCCESS) {
    return false;
  }
  if (result_array.end().get(result_end) != ::simdjson::SUCCESS) {
    return false;
  }
  for (; spec_it != spec_end && result_it != result_end;
       ++spec_it, ++result_it) {
    ::simdjson::ondemand::value value;
    if ((*result_it).get(value) != ::simdjson::SUCCESS) {
      return false;
    }
    if (!this->set_item(*spec_it, value)) {
      return false;
    }
  }
  return spec_it == spec_end && result_it == result_end;
}

bool lsp_workspace_configuration::process_notification(
    ::simdjson::ondemand::object settings) {
  for (simdjson::simdjson_result< ::simdjson::ondemand::field> setting_field :
       settings) {
    std::string_view name;
    if (setting_field.unescaped_key().get(name) != ::simdjson::SUCCESS) {
      return false;
    }
    ::simdjson::ondemand::value value;
    if (setting_field.value().get(value) != ::simdjson::SUCCESS) {
      return false;
    }
    item* i = this->find_item(to_string8_view(name));
    if (!i) {
      // Ignore unknown settings.
      continue;
    }
    if (!this->set_item(*i, value)) {
      return false;
    }
  }
  return true;
}

lsp_workspace_configuration::item* lsp_workspace_configuration::find_item(
    string8_view name) {
  for (item& i : this->items_) {
    if (i.name == name) {
      return &i;
    }
  }
  return nullptr;
}

bool lsp_workspace_configuration::set_item(item& i,
                                           ::simdjson::ondemand::value value) {
  ::simdjson::ondemand::json_type type;
  if (value.type().get(type) != ::simdjson::SUCCESS) {
    return false;
  }
  switch (type) {
  case ::simdjson::ondemand::json_type::string: {
    std::string_view string_value;
    if (value.get(string_value) != ::simdjson::SUCCESS) {
      return false;
    }
    i.callback(string_value);
    return true;
  }

  case ::simdjson::ondemand::json_type::null:
    i.callback(std::string_view());
    return true;

  default:
    return false;
  }
}
}

#endif

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
