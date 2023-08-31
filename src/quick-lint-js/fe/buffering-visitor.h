// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

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
class Buffering_Visitor final : public Parse_Visitor_Base {
 public:
  explicit Buffering_Visitor(Memory_Resource *memory);

  // Copying is usually a bug, so disable copying.
  Buffering_Visitor(const Buffering_Visitor &) = delete;
  Buffering_Visitor &operator=(const Buffering_Visitor &) = delete;

  Buffering_Visitor(Buffering_Visitor &&) = default;
  Buffering_Visitor &operator=(Buffering_Visitor &&) = default;

  void move_into(Parse_Visitor_Base &target);
  void copy_into(Parse_Visitor_Base &target) const;

  void visit_end_of_module() override;
  void visit_enter_block_scope() override;
  void visit_enter_with_scope() override;
  void visit_enter_class_construct_scope() override;
  void visit_enter_class_scope() override;
  void visit_enter_class_scope_body(
      const std::optional<Identifier> &class_name) override;
  void visit_enter_conditional_type_scope() override;
  void visit_enter_enum_scope() override;
  void visit_enter_for_scope() override;
  void visit_enter_function_scope() override;
  void visit_enter_function_scope_body() override;
  void visit_enter_index_signature_scope() override;
  void visit_enter_interface_scope() override;
  void visit_enter_namespace_scope() override;
  void visit_enter_type_alias_scope() override;
  void visit_enter_named_function_scope(Identifier name) override;
  void visit_exit_block_scope() override;
  void visit_exit_with_scope() override;
  void visit_exit_class_construct_scope() override;
  void visit_exit_class_scope() override;
  void visit_exit_conditional_type_scope() override;
  void visit_exit_enum_scope() override;
  void visit_exit_for_scope() override;
  void visit_exit_function_scope() override;
  void visit_exit_index_signature_scope() override;
  void visit_exit_namespace_scope() override;
  void visit_exit_type_alias_scope() override;
  void visit_exit_interface_scope() override;
  void visit_keyword_variable_use(Identifier name) override;
  void visit_property_declaration(
      const std::optional<Identifier> &name) override;
  void visit_variable_assignment(Identifier name) override;
  void visit_variable_declaration(Identifier name, Variable_Kind kind,
                                  Variable_Declaration_Flags flags) override;
  void visit_variable_delete_use(Identifier name,
                                 Source_Code_Span delete_keyword) override;
  void visit_variable_export_use(Identifier name) override;
  void visit_variable_namespace_use(Identifier name) override;
  void visit_variable_type_predicate_use(Identifier parameter_name) override;
  void visit_variable_type_use(Identifier name) override;
  void visit_variable_typeof_use(Identifier name) override;
  void visit_variable_use(Identifier name) override;

 private:
  enum class Visit_Kind : unsigned char {
    end_of_module,
    enter_block_scope,
    enter_with_scope,
    enter_class_construct_scope,
    enter_class_scope,
    enter_class_scope_body_with_name,
    enter_class_scope_body_without_name,
    enter_conditional_type_scope,
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
    exit_class_construct_scope,
    exit_class_scope,
    exit_conditional_type_scope,
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
  // inlining of visit::visit and Linked_Vector<>::emplace_back.
  [[gnu::noinline]] void add(Visit_Kind kind) {
    this->visits_.emplace_back(kind);
  }

  [[gnu::noinline]] void add(const Identifier &name, Visit_Kind kind) {
    this->visits_.emplace_back(kind, name);
  }

  struct Visit {
    explicit Visit(Visit_Kind kind) : kind(kind) {}

    explicit Visit(Visit_Kind kind, Identifier name) : kind(kind), name(name) {}

    explicit Visit(Visit_Kind kind, Identifier name, Variable_Kind var_kind,
                   Variable_Declaration_Flags flags)
        : kind(kind), name(name), var_decl{var_kind, flags} {}

    explicit Visit(Visit_Kind kind, Identifier name,
                   Source_Code_Span extra_span)
        : kind(kind), name(name), extra_span(extra_span) {}

    Visit_Kind kind;

    union {
      // enter_class_scope_body_with_name
      // enter_named_function_scope
      // keyword_variable_use
      // property_declaration
      // variable_declaration
      // variable_assignment
      // variable_delete_use
      // variable_export_use
      // variable_namespace_use
      // variable_type_predicate_use
      // variable_type_use
      // variable_typeof_use
      // variable_use
      Identifier name;
      static_assert(is_winkable_v<Identifier>);
    };

    struct Var_Decl_Data {
      Variable_Kind var_kind;
      Variable_Declaration_Flags flags;
    };
    union {
      // variable_declaration
      Var_Decl_Data var_decl;
      static_assert(is_winkable_v<Var_Decl_Data>);

      // variable_delete_use
      Source_Code_Span extra_span;
      static_assert(is_winkable_v<Source_Code_Span>);
    };
  };

  Linked_Vector<Visit> visits_;
};

template <>
struct Is_Winkable<Buffering_Visitor> : std::true_type {};
}

QLJS_WARNING_POP

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
