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

#ifndef QUICK_LINT_JS_SPY_VISITOR_H
#define QUICK_LINT_JS_SPY_VISITOR_H

#include <iosfwd>
#include <optional>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/error-collector.h>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse-visitor.h>
#include <string_view>
#include <vector>

// HACK(strager): Improve formatting of googletest diagnostics.
namespace std {
inline void PrintTo(const std::string_view &s, std::ostream *out) { *out << s; }
}

namespace quick_lint_js {
struct spy_visitor : public error_collector {
  std::vector<std::string_view> visits;

  void visit_end_of_module() {
    this->visits.emplace_back("visit_end_of_module");
  }

  void visit_enter_block_scope() {
    this->visits.emplace_back("visit_enter_block_scope");
  }

  void visit_enter_class_scope() {
    this->visits.emplace_back("visit_enter_class_scope");
  }

  void visit_enter_for_scope() {
    this->visits.emplace_back("visit_enter_for_scope");
  }

  void visit_enter_function_scope() {
    this->visits.emplace_back("visit_enter_function_scope");
  }

  void visit_enter_function_scope_body() {
    this->visits.emplace_back("visit_enter_function_scope_body");
  }

  void visit_enter_named_function_scope(identifier name) {
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

  void visit_exit_block_scope() {
    this->visits.emplace_back("visit_exit_block_scope");
  }

  void visit_exit_class_scope() {
    this->visits.emplace_back("visit_exit_class_scope");
  }

  void visit_exit_for_scope() {
    this->visits.emplace_back("visit_exit_for_scope");
  }

  void visit_exit_function_scope() {
    this->visits.emplace_back("visit_exit_function_scope");
  }

  void visit_property_declaration() {
    this->property_declarations.emplace_back(visited_property_declaration());
    this->visits.emplace_back("visit_property_declaration");
  }

  void visit_property_declaration(identifier name) {
    this->property_declarations.emplace_back(
        visited_property_declaration{string8(name.normalized_name())});
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

  void visit_variable_assignment(identifier name) {
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

  void visit_variable_declaration(identifier name, variable_kind kind) {
    this->variable_declarations.emplace_back(
        visited_variable_declaration{string8(name.normalized_name()), kind});
    this->visits.emplace_back("visit_variable_declaration");
  }

  struct visited_variable_declaration {
    string8 name;
    variable_kind kind;

    bool operator==(const visited_variable_declaration &other) const {
      return this->name == other.name && this->kind == other.kind;
    }

    bool operator!=(const visited_variable_declaration &other) const {
      return !(*this == other);
    }
  };
  std::vector<visited_variable_declaration> variable_declarations;

  void visit_variable_export_use(identifier name) {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_export_use");
  }

  void visit_variable_typeof_use(identifier name) {
    this->variable_uses.emplace_back(
        visited_variable_use{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_typeof_use");
  }

  void visit_variable_use(identifier name) {
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

  void visit_variable_use_and_assignment(identifier name) {
    this->variable_use_and_assignments.emplace_back(
        visited_variable_use_and_assignment{string8(name.normalized_name())});
    this->visits.emplace_back("visit_variable_use_and_assignment");
  }

  struct visited_variable_use_and_assignment {
    string8 name;

    bool operator==(const visited_variable_use_and_assignment &other) const {
      return this->name == other.name;
    }

    bool operator!=(const visited_variable_use_and_assignment &other) const {
      return !(*this == other);
    }
  };
  std::vector<visited_variable_use_and_assignment> variable_use_and_assignments;
};
QLJS_STATIC_ASSERT_IS_PARSE_VISITOR(spy_visitor);

void PrintTo(const spy_visitor::visited_variable_assignment &, std::ostream *);
void PrintTo(const spy_visitor::visited_variable_declaration &, std::ostream *);
void PrintTo(const spy_visitor::visited_variable_use &, std::ostream *);
void PrintTo(const spy_visitor::visited_variable_use_and_assignment &,
             std::ostream *);
}

#endif
