// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_SPY_VISITOR_H
#define QUICK_LINT_JS_SPY_VISITOR_H

#include <iosfwd>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/gtest.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse-visitor.h>
#include <string_view>
#include <vector>

namespace quick_lint_js {
struct spy_visitor final : public diag_collector, public parse_visitor_base {
  std::vector<std::string_view> visits;

  void visit_end_of_module() override {
    this->visits.emplace_back("visit_end_of_module");
  }

  void visit_enter_block_scope() override {
    this->visits.emplace_back("visit_enter_block_scope");
  }

  void visit_enter_with_scope() override {
    this->visits.emplace_back("visit_enter_with_scope");
  }

  void visit_enter_class_scope() override {
    this->visits.emplace_back("visit_enter_class_scope");
  }

  void visit_enter_for_scope() override {
    this->visits.emplace_back("visit_enter_for_scope");
  }

  void visit_enter_function_scope() override {
    this->visits.emplace_back("visit_enter_function_scope");
  }

  void visit_enter_function_scope_body() override {
    this->visits.emplace_back("visit_enter_function_scope_body");
  }

  void visit_enter_named_function_scope(identifier name) override {
    this->enter_named_function_scopes.emplace_back(
        visited_enter_named_function_scope{string8(name.normalized_name())});
    this->visits.emplace_back("visit_enter_named_function_scope");
  }

  struct visited_enter_named_function_scope {
    string8 name;

    bool operator==(const visited_enter_named_function_scope &other) const {
      return this->name == other.name;
    }

    bool operator!=(const visited_enter_named_function_scope &other) const {
      return !(*this == other);
    }
  };
  std::vector<visited_enter_named_function_scope> enter_named_function_scopes;

  void visit_exit_block_scope() override {
    this->visits.emplace_back("visit_exit_block_scope");
  }

  void visit_exit_with_scope() override {
    this->visits.emplace_back("visit_exit_with_scope");
  }

  void visit_exit_class_scope() override {
    this->visits.emplace_back("visit_exit_class_scope");
  }

  void visit_exit_for_scope() override {
    this->visits.emplace_back("visit_exit_for_scope");
  }

  void visit_exit_function_scope() override {
    this->visits.emplace_back("visit_exit_function_scope");
  }

  void visit_keyword_variable_use(identifier name) override {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_keyword_variable_use");
  }

  void visit_property_declaration(std::optional<identifier> name) override {
    if (name.has_value()) {
      this->property_declarations.emplace_back(
          visited_property_declaration{string8(name->normalized_name())});
    } else {
      this->property_declarations.emplace_back(visited_property_declaration());
    }
    this->visits.emplace_back("visit_property_declaration");
  }

  struct visited_property_declaration {
    std::optional<string8> name;

    bool operator==(const visited_property_declaration &other) const {
      return this->name == other.name;
    }

    bool operator!=(const visited_property_declaration &other) const {
      return !(*this == other);
    }
  };
  std::vector<visited_property_declaration> property_declarations;

  void visit_variable_assignment(identifier name) override {
    this->variable_assignments.emplace_back(
        visited_variable_assignment{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_assignment");
  }

  struct visited_variable_assignment {
    string8 name;

    bool operator==(const visited_variable_assignment &other) const {
      return this->name == other.name;
    }

    bool operator!=(const visited_variable_assignment &other) const {
      return !(*this == other);
    }
  };
  std::vector<visited_variable_assignment> variable_assignments;

  void visit_variable_declaration(identifier name, variable_kind kind,
                                  variable_init_kind init_kind) override {
    this->variable_declarations.emplace_back(visited_variable_declaration{
        string8(name.normalized_name()), kind, init_kind});
    this->visits.emplace_back("visit_variable_declaration");
  }

  struct visited_variable_declaration {
    string8 name;
    variable_kind kind;
    variable_init_kind init_kind;

    bool operator==(const visited_variable_declaration &other) const {
      return this->name == other.name && this->kind == other.kind &&
             this->init_kind == other.init_kind;
    }

    bool operator!=(const visited_variable_declaration &other) const {
      return !(*this == other);
    }
  };
  std::vector<visited_variable_declaration> variable_declarations;

  void visit_variable_delete_use(
      identifier name,
      [[maybe_unused]] source_code_span delete_keyword) override {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_delete_use");
  }

  void visit_variable_export_use(identifier name) override {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_export_use");
  }

  void visit_variable_namespace_use(identifier name) override {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_namespace_use");
  }

  void visit_variable_type_use(identifier name) override {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_type_use");
  }

  void visit_variable_typeof_use(identifier name) override {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_typeof_use");
  }

  void visit_variable_use(identifier name) override {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_use");
  }

  struct visited_variable_use {
    string8 name;

    bool operator==(const visited_variable_use &other) const {
      return this->name == other.name;
    }

    bool operator!=(const visited_variable_use &other) const {
      return !(*this == other);
    }
  };
  std::vector<visited_variable_use> variable_uses;
};

void PrintTo(const spy_visitor::visited_property_declaration &, std::ostream *);
void PrintTo(const spy_visitor::visited_variable_assignment &, std::ostream *);
void PrintTo(const spy_visitor::visited_variable_declaration &, std::ostream *);
void PrintTo(const spy_visitor::visited_variable_use &, std::ostream *);
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
