// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_FE_BUFFERING_VISITOR_H
#define QUICK_LINT_JS_FE_BUFFERING_VISITOR_H

#include <optional>
#include <quick-lint-js/container/linked-vector.h>
#include <quick-lint-js/container/winkable.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/port/memory-resource.h>
#include <quick-lint-js/port/warning.h>
#include <type_traits>
#include <utility>

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_MSVC(26495)  // Variable is uninitialized.

namespace quick_lint_js {
class buffering_visitor final : public parse_visitor_base {
 public:
  explicit buffering_visitor(memory_resource *memory);

  // Copying is usually a bug, so disable copying.
  buffering_visitor(const buffering_visitor &) = delete;
  buffering_visitor &operator=(const buffering_visitor &) = delete;

  buffering_visitor(buffering_visitor &&) = default;
  buffering_visitor &operator=(buffering_visitor &&) = default;

  void move_into(parse_visitor_base &target);
  void copy_into(parse_visitor_base &target) const;

  void visit_end_of_module() override;
  void visit_enter_block_scope() override;
  void visit_enter_with_scope() override;
  void visit_enter_class_scope() override;
  void visit_enter_class_scope_body(
      const std::optional<identifier> &class_name) override;
  void visit_enter_enum_scope() override;
  void visit_enter_for_scope() override;
  void visit_enter_function_scope() override;
  void visit_enter_function_scope_body() override;
  void visit_enter_index_signature_scope() override;
  void visit_enter_interface_scope() override;
  void visit_enter_namespace_scope() override;
  void visit_enter_type_alias_scope() override;
  void visit_enter_named_function_scope(identifier name) override;
  void visit_exit_block_scope() override;
  void visit_exit_with_scope() override;
  void visit_exit_class_scope() override;
  void visit_exit_enum_scope() override;
  void visit_exit_for_scope() override;
  void visit_exit_function_scope() override;
  void visit_exit_index_signature_scope() override;
  void visit_exit_namespace_scope() override;
  void visit_exit_type_alias_scope() override;
  void visit_exit_interface_scope() override;
  void visit_keyword_variable_use(identifier name) override;
  void visit_property_declaration(
      const std::optional<identifier> &name) override;
  void visit_variable_assignment(identifier name) override;
  void visit_variable_declaration(identifier name, variable_kind kind,
                                  variable_init_kind init_kind) override;
  void visit_variable_delete_use(identifier name,
                                 source_code_span delete_keyword) override;
  void visit_variable_export_use(identifier name) override;
  void visit_variable_namespace_use(identifier name) override;
  void visit_variable_type_predicate_use(identifier parameter_name) override;
  void visit_variable_type_use(identifier name) override;
  void visit_variable_typeof_use(identifier name) override;
  void visit_variable_use(identifier name) override;

 private:
  enum class visit_kind {
    end_of_module,
    enter_block_scope,
    enter_with_scope,
    enter_class_scope,
    enter_class_scope_body_with_name,
    enter_class_scope_body_without_name,
    enter_enum_scope,
    enter_for_scope,
    enter_function_scope,
    enter_function_scope_body,
    enter_index_signature_scope,
    enter_interface_scope,
    enter_named_function_scope,
    enter_namespace_scope,
    enter_type_alias_scope,
    exit_block_scope,
    exit_with_scope,
    exit_class_scope,
    exit_enum_scope,
    exit_for_scope,
    exit_function_scope,
    exit_index_signature_scope,
    exit_interface_scope,
    exit_namespace_scope,
    exit_type_alias_scope,
    keyword_variable_use,
    property_declaration_with_name,
    property_declaration_without_name,
    variable_assignment,
    variable_delete_use,
    variable_export_use,
    variable_namespace_use,
    variable_type_predicate_use,
    variable_type_use,
    variable_typeof_use,
    variable_use,
    variable_declaration,
  };

  // These 'add' functions significantly reduces code size by discouraging the
  // inlining of visit::visit and linked_vector<>::emplace_back.
  [[gnu::noinline]] void add(visit_kind kind) {
    this->visits_.emplace_back(kind);
  }

  [[gnu::noinline]] void add(const identifier &name, visit_kind kind) {
    this->visits_.emplace_back(kind, name);
  }

  struct visit {
    explicit visit(visit_kind kind) noexcept : kind(kind) {}

    explicit visit(visit_kind kind, identifier name) noexcept
        : kind(kind), name(name) {}

    explicit visit(visit_kind kind, identifier name, variable_kind var_kind,
                   variable_init_kind init_kind) noexcept
        : kind(kind), name(name), var_decl{var_kind, init_kind} {}

    explicit visit(visit_kind kind, identifier name,
                   source_code_span extra_span) noexcept
        : kind(kind), name(name), extra_span(extra_span) {}

    visit_kind kind;

    union {
      // enter_named_function_scope, keyword_variable_use, property_declaration,
      // variable_assignment, variable_declaration, variable_use
      identifier name;
      static_assert(is_winkable_v<identifier>);
    };

    struct var_decl_data {
      variable_kind var_kind;
      variable_init_kind var_init_kind;
    };
    union {
      // variable_declaration
      var_decl_data var_decl;
      static_assert(is_winkable_v<var_decl_data>);

      // variable_delete_use
      source_code_span extra_span;
      static_assert(is_winkable_v<source_code_span>);
    };
  };

  linked_vector<visit> visits_;
};

template <>
struct is_winkable<buffering_visitor> : std::true_type {};
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
