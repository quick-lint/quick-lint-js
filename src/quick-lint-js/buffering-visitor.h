// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_BUFFERING_VISITOR_H
#define QUICK_LINT_JS_BUFFERING_VISITOR_H

#include <boost/container/pmr/memory_resource.hpp>
#include <optional>
#include <quick-lint-js/language.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>
#include <utility>
#include <vector>

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_MSVC(26495)  // Variable is uninitialized.

namespace quick_lint_js {
class buffering_visitor {
 public:
  explicit buffering_visitor(boost::container::pmr::memory_resource *memory)
      : visits_("buffering_visitor", memory) {}

  template <QLJS_PARSE_VISITOR Visitor>
  void move_into(Visitor &target) {
    this->copy_into(target);
  }

  template <QLJS_PARSE_VISITOR Visitor>
  void copy_into(Visitor &target) const {
    for (auto &v : this->visits_) {
      switch (v.kind) {
      case visit_kind::end_of_module:
        target.visit_end_of_module();
        break;
      case visit_kind::enter_block_scope:
        target.visit_enter_block_scope();
        break;
      case visit_kind::enter_with_scope:
        target.visit_enter_with_scope();
        break;
      case visit_kind::enter_class_scope:
        target.visit_enter_class_scope();
        break;
      case visit_kind::enter_for_scope:
        target.visit_enter_for_scope();
        break;
      case visit_kind::enter_function_scope:
        target.visit_enter_function_scope();
        break;
      case visit_kind::enter_function_scope_body:
        target.visit_enter_function_scope_body();
        break;
      case visit_kind::enter_named_function_scope:
        target.visit_enter_named_function_scope(v.name);
        break;
      case visit_kind::exit_block_scope:
        target.visit_exit_block_scope();
        break;
      case visit_kind::exit_with_scope:
        target.visit_exit_with_scope();
        break;
      case visit_kind::exit_class_scope:
        target.visit_exit_class_scope();
        break;
      case visit_kind::exit_for_scope:
        target.visit_exit_for_scope();
        break;
      case visit_kind::exit_function_scope:
        target.visit_exit_function_scope();
        break;
      case visit_kind::keyword_variable_use:
        target.visit_keyword_variable_use(v.name);
        break;
      case visit_kind::property_declaration_with_name:
        target.visit_property_declaration(v.name);
        break;
      case visit_kind::property_declaration_without_name:
        target.visit_property_declaration(std::nullopt);
        break;
      case visit_kind::variable_assignment:
        target.visit_variable_assignment(v.name);
        break;
      case visit_kind::variable_delete_use:
        target.visit_variable_delete_use(v.name, v.extra_span);
        break;
      case visit_kind::variable_export_use:
        target.visit_variable_export_use(v.name);
        break;
      case visit_kind::variable_typeof_use:
        target.visit_variable_typeof_use(v.name);
        break;
      case visit_kind::variable_use:
        target.visit_variable_use(v.name);
        break;
      case visit_kind::variable_declaration:
        target.visit_variable_declaration(v.name, v.var_kind);
        break;
      }
    }
  }

  void visit_end_of_module() {
    this->visits_.emplace_back(visit_kind::end_of_module);
  }

  void visit_enter_block_scope() {
    this->visits_.emplace_back(visit_kind::enter_block_scope);
  }

  void visit_enter_with_scope() {
    this->visits_.emplace_back(visit_kind::enter_with_scope);
  }

  void visit_enter_class_scope() {
    this->visits_.emplace_back(visit_kind::enter_class_scope);
  }

  void visit_enter_for_scope() {
    this->visits_.emplace_back(visit_kind::enter_for_scope);
  }

  void visit_enter_function_scope() {
    this->visits_.emplace_back(visit_kind::enter_function_scope);
  }

  void visit_enter_function_scope_body() {
    this->visits_.emplace_back(visit_kind::enter_function_scope_body);
  }

  void visit_enter_named_function_scope(identifier name) {
    this->visits_.emplace_back(visit_kind::enter_named_function_scope, name);
  }

  void visit_exit_block_scope() {
    this->visits_.emplace_back(visit_kind::exit_block_scope);
  }

  void visit_exit_with_scope() {
    this->visits_.emplace_back(visit_kind::exit_with_scope);
  }

  void visit_exit_class_scope() {
    this->visits_.emplace_back(visit_kind::exit_class_scope);
  }

  void visit_exit_for_scope() {
    this->visits_.emplace_back(visit_kind::exit_for_scope);
  }

  void visit_exit_function_scope() {
    this->visits_.emplace_back(visit_kind::exit_function_scope);
  }

  void visit_keyword_variable_use(identifier name) {
    this->visits_.emplace_back(visit_kind::keyword_variable_use, name);
  }

  void visit_property_declaration(std::optional<identifier> name) {
    if (name.has_value()) {
      this->visits_.emplace_back(visit_kind::property_declaration_with_name,
                                 *name);
    } else {
      this->visits_.emplace_back(visit_kind::property_declaration_without_name);
    }
  }

  void visit_variable_assignment(identifier name) {
    this->visits_.emplace_back(visit_kind::variable_assignment, name);
  }

  void visit_variable_declaration(identifier name, variable_kind kind) {
    this->visits_.emplace_back(visit_kind::variable_declaration, name, kind);
  }

  void visit_variable_delete_use(identifier name,
                                 source_code_span delete_keyword) {
    this->visits_.emplace_back(visit_kind::variable_delete_use, name,
                               delete_keyword);
  }

  void visit_variable_export_use(identifier name) {
    this->visits_.emplace_back(visit_kind::variable_export_use, name);
  }

  void visit_variable_typeof_use(identifier name) {
    this->visits_.emplace_back(visit_kind::variable_typeof_use, name);
  }

  void visit_variable_use(identifier name) {
    this->visits_.emplace_back(visit_kind::variable_use, name);
  }

 private:
  enum class visit_kind {
    end_of_module,
    enter_block_scope,
    enter_with_scope,
    enter_class_scope,
    enter_for_scope,
    enter_function_scope,
    enter_function_scope_body,
    enter_named_function_scope,
    exit_block_scope,
    exit_with_scope,
    exit_class_scope,
    exit_for_scope,
    exit_function_scope,
    keyword_variable_use,
    property_declaration_with_name,
    property_declaration_without_name,
    variable_assignment,
    variable_delete_use,
    variable_export_use,
    variable_typeof_use,
    variable_use,
    variable_declaration,
  };

  struct visit {
    explicit visit(visit_kind kind) noexcept : kind(kind) {}

    explicit visit(visit_kind kind, identifier name) noexcept
        : kind(kind), name(name) {}

    explicit visit(visit_kind kind, identifier name,
                   variable_kind var_kind) noexcept
        : kind(kind), name(name), var_kind(var_kind) {}

    explicit visit(visit_kind kind, identifier name,
                   source_code_span extra_span) noexcept
        : kind(kind), name(name), extra_span(extra_span) {}

    visit_kind kind;

    union {
      // enter_named_function_scope, keyword_variable_use, property_declaration,
      // variable_assignment, variable_declaration, variable_use
      identifier name;
      static_assert(std::is_trivially_destructible_v<identifier>);
    };

    union {
      // variable_declaration
      variable_kind var_kind;
      static_assert(std::is_trivially_destructible_v<variable_kind>);

      // variable_delete_use
      source_code_span extra_span;
      static_assert(std::is_trivially_destructible_v<source_code_span>);
    };
  };

  vector<visit> visits_;
};
QLJS_STATIC_ASSERT_IS_PARSE_VISITOR(buffering_visitor);
}

QLJS_WARNING_POP

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
