// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstring>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <quick-lint-js/diag-collector.h>
#include <quick-lint-js/diag-matcher.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/variable-analyzer.h>
#include <quick-lint-js/identifier-support.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/variable-analyzer-support.h>

using ::testing::ElementsAreArray;
using ::testing::IsEmpty;

namespace quick_lint_js {
namespace {
TEST(Test_Variable_Analyzer_Type,
     type_use_does_not_warn_on_predefined_global_classes) {
  test_parse_and_analyze(u8"([]) as Array;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, type_use_after_declaration_is_okay) {
  test_parse_and_analyze(u8"interface I {}  ({}) as I;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class I {}  ({}) as I;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     type_use_in_block_scope_after_declaration_is_okay) {
  test_parse_and_analyze(
      u8"class I {} "_sv
      u8"{"_sv
      u8"  ({}) as I;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"enum I {} "_sv
      u8"{"_sv
      u8"  ({}) as I;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"interface I {} "_sv
      u8"{"_sv
      u8"  ({}) as I;"_sv
      u8"} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, type_use_with_no_declaration_is_an_error) {
  test_parse_and_analyze(u8"({}) as C;"_sv,
                         u8"        ^ Diag_Use_Of_Undeclared_Type.name"_diag,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     type_use_after_declaration_in_block_scope_is_an_error) {
  test_parse_and_analyze(
      u8"{ class C {}  }  ({}) as C;"_sv,
      u8"                         ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"{ enum E {}  }  ({}) as E;"_sv,
      u8"                        ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"{ interface I {}  }  ({}) as I;"_sv,
      u8"                             ^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, type_use_before_declaration_is_okay) {
  test_parse_and_analyze(u8"({}) as I; class I {} "_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  ({}) as I;"_sv
      u8"});"_sv
      u8"class I {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"({}) as I; enum I {} "_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  ({}) as I;"_sv
      u8"});"_sv
      u8"enum I {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(u8"({}) as I; interface I {} "_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  ({}) as I;"_sv
      u8"});"_sv
      u8"interface I {} "_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, type_use_of_import_is_okay) {
  test_parse_and_analyze(
      u8"({}) as I;"_sv
      u8"import {I} from 'module';"_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"import {I} from 'module';"_sv
      u8"({}) as I;"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     generic_parameter_use_before_declaration_in_extends_is_allowed) {
  test_parse_and_analyze(u8"(function< T extends U, U, >() { });"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     generic_parameter_use_before_declaration_in_default_is_not_allowed) {
  test_parse_and_analyze(
      u8"(function< T extends number = U, U, >() { });"_sv,
      u8"                                 ^ Diag_Variable_Used_Before_Declaration.declaration\n"_diag
      u8"                              ^ .use"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, interface_can_be_exported) {
  test_parse_and_analyze(
      u8"interface I {} "_sv
      u8"export {I};"_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"export {I};"_sv
      u8"interface I {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"interface I {} "_sv
      u8"(() => {"_sv
      u8"  export {I};"_sv
      u8"});"_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"(() => {"_sv
      u8"  export {I};"_sv
      u8"});"_sv
      u8"interface I {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"interface I {} "_sv
      u8"(() => {"_sv
      u8"  (() => {"_sv
      u8"    export {I};"_sv
      u8"  });"_sv
      u8"});"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, type_use_does_not_see_non_type_variables) {
  // TODO(strager): Report a more helpful message indicating that 'I' is a
  // function or variable, not a type.
  test_parse_and_analyze(
      u8"((I) => { ({}) as I; });"_sv,
      u8"                  ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"const I = null; ({}) as I;"_sv,
      u8"                        ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function I() {}  ({}) as I;"_sv,
      u8"                         ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function(I) { ({}) as I; });"_sv,
      u8"                       ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"interface _ { [I: string]: I; }"_sv,
      u8"                           ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"let I; ({}) as I;"_sv,
                         u8"               ^ Diag_Use_Of_Undeclared_Type"_diag,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"var I; ({}) as I;"_sv,
                         u8"               ^ Diag_Use_Of_Undeclared_Type"_diag,
                         typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"((I) => { { ({}) as I; } });"_sv,
      u8"                    ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"try {} catch (I) { ({}) as I; }"_sv,
      u8"                           ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"const I = null; { ({}) as I; }"_sv,
      u8"                          ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function I() {}  { ({}) as I; }"_sv,
      u8"                           ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function(I) { { ({}) as I; } });"_sv,
      u8"                         ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"let I; { ({}) as I; }"_sv,
      u8"                 ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"var I; { ({}) as I; }"_sv,
      u8"                 ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"((I) => { (() => { ({}) as I; }); });"_sv,
      u8"                           ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"try {} catch (I) { (() => { ({}) as I; }); }"_sv,
      u8"                                    ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"const I = null; (() => { ({}) as I; });"_sv,
      u8"                                 ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function I() {}  (() => { ({}) as I; });"_sv,
      u8"                                  ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function(I) { (() => { ({}) as I; }); });"_sv,
      u8"                                ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"let I; (() => { ({}) as I; });"_sv,
      u8"                        ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"var I; (() => { ({}) as I; });"_sv,
      u8"                        ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"((I) => { (() => { (() => { ({}) as I; }); }); });"_sv,
      u8"                                    ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"try {} catch (I) { (() => { (() => { ({}) as I; }); }); }"_sv,
      u8"                                             ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"const I = null; (() => { (() => { ({}) as I; }); });"_sv,
      u8"                                          ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"function I() {}  (() => { (() => { ({}) as I; }); });"_sv,
      u8"                                           ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"(function(I) { (() => { (() => { ({}) as I; }); }); });"_sv,
      u8"                                         ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"let I; (() => { (() => { ({}) as I; }); });"_sv,
      u8"                                 ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
  test_parse_and_analyze(
      u8"var I; (() => { (() => { ({}) as I; }); });"_sv,
      u8"                                 ^ Diag_Use_Of_Undeclared_Type"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     interfaces_are_ignored_in_runtime_expressions) {
  using Diags_Matcher =
      testing::Matcher<const std::vector<Diag_Collector::Diag>&>;

  static const Char8 outer_declaration[] = u8"I";
  static const Char8 declaration[] = u8"I";

  static const Char8 assignment[] = u8"I";
  static const Char8 use[] = u8"I";

  static const Padded_String delete_expression(u8"delete I"_sv);
  static const Source_Code_Span delete_keyword_span(
      delete_expression.data(), delete_expression.data() + 6);
  ASSERT_EQ(delete_keyword_span.string_view(), u8"delete"_sv);
  static const Source_Code_Span deleted_variable_span(
      delete_expression.data() + 7, delete_expression.data() + 8);
  ASSERT_EQ(deleted_variable_span.string_view(), u8"I"_sv);

  struct Variable_Visit_Kind {
    const char* description;
    void (*visit)(Variable_Analyzer&);

    // Used when no run-time variable exists with the same name as the
    // If a run-time variable exists with the same name as the interface,
    // 'runtime_var_kind' is set to that variable's kind.
    //
    // If no run-time variable exists with the same name as the interface,
    // 'runtime_var_kind' is nullopt.
    Diags_Matcher (*get_diags_matcher)(
        std::optional<Variable_Kind> runtime_var_kind);
  };

  Variable_Visit_Kind variable_visit_kinds[] = {
      {
          .description = "visit_variable_assignment",
          .visit =
              [](Variable_Analyzer& l) {
                l.visit_variable_assignment(identifier_of(assignment));
              },
          .get_diags_matcher = [](std::optional<Variable_Kind> runtime_var_kind)
              -> Diags_Matcher {
            if (runtime_var_kind.has_value()) {
              if (*runtime_var_kind == Variable_Kind::_const) {
                return ElementsAreArray({
                    DIAG_TYPE_2_SPANS(Diag_Assignment_To_Const_Variable,  //
                                      assignment, span_of(assignment),    //
                                      declaration, span_of(outer_declaration)),
                });
              } else {
                return IsEmpty();
              }
            } else {
              // TODO(strager): Report a more helpful message.
              return ElementsAreArray({
                  DIAG_TYPE_SPAN(Diag_Assignment_To_Undeclared_Variable,
                                 assignment, span_of(assignment)),
              });
            }
          },
      },

      {
          .description = "visit_variable_delete_use",
          .visit =
              [](Variable_Analyzer& l) {
                l.visit_variable_delete_use(Identifier(deleted_variable_span),
                                            delete_keyword_span);
              },
          .get_diags_matcher = [](std::optional<Variable_Kind> runtime_var_kind)
              -> Diags_Matcher {
            if (runtime_var_kind.has_value()) {
              return ElementsAreArray({
                  DIAG_TYPE_OFFSETS(
                      &delete_expression,
                      Diag_Redundant_Delete_Statement_On_Variable,  //
                      delete_expression, 0, u8"delete I"_sv),
              });
            } else {
              return IsEmpty();
            }
          },
      },

      {.description = "visit_variable_use",
       .visit =
           [](Variable_Analyzer& l) {
             l.visit_variable_use(identifier_of(use));
           },
       .get_diags_matcher =
           [](std::optional<Variable_Kind> runtime_var_kind) -> Diags_Matcher {
         if (runtime_var_kind.has_value()) {
           return IsEmpty();
         } else {
           // TODO(strager): Report a more helpful message.
           return ElementsAreArray({
               DIAG_TYPE_SPAN(Diag_Use_Of_Undeclared_Variable, name,
                              span_of(use)),
           });
         }
       }},
  };

  for (Variable_Visit_Kind& visit_kind : variable_visit_kinds) {
    SCOPED_TRACE(visit_kind.description);

    {
      // interface I {}
      // I;              // ERROR
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(declaration),
                                   Variable_Kind::_interface,
                                   Variable_Declaration_Flags::none);
      visit_kind.visit(l);
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(std::nullopt));
    }

    {
      // interface I {}
      // {
      //   I;            // ERROR
      // }
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(declaration),
                                   Variable_Kind::_interface,
                                   Variable_Declaration_Flags::none);
      l.visit_enter_block_scope();
      visit_kind.visit(l);
      l.visit_exit_block_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(std::nullopt));
    }

    {
      // interface I {}
      // (() => {
      //   (() => {
      //     I;            // ERROR
      //   });
      // });
      Diag_Collector v;
      Variable_Analyzer l(&v, &default_globals, javascript_var_options);
      l.visit_variable_declaration(identifier_of(declaration),
                                   Variable_Kind::_interface,
                                   Variable_Declaration_Flags::none);
      l.visit_enter_function_scope();
      l.visit_enter_function_scope_body();
      l.visit_enter_function_scope();
      l.visit_enter_function_scope_body();
      visit_kind.visit(l);
      l.visit_exit_function_scope();
      l.visit_exit_function_scope();
      l.visit_end_of_module();

      EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(std::nullopt));
    }

    for (Variable_Kind outer_kind : {
             Variable_Kind::_arrow_parameter,
             Variable_Kind::_catch,
             Variable_Kind::_const,
             Variable_Kind::_function,
             Variable_Kind::_function_parameter,
             Variable_Kind::_index_signature_parameter,
             Variable_Kind::_let,
             Variable_Kind::_var,
         }) {
      SCOPED_TRACE(outer_kind);

      {
        // let I;
        // {
        //   interface I {}
        //   I;
        // }
        Diag_Collector v;
        Variable_Analyzer l(&v, &default_globals, javascript_var_options);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_enter_block_scope();
        l.visit_variable_declaration(identifier_of(declaration),
                                     Variable_Kind::_interface,
                                     Variable_Declaration_Flags::none);
        visit_kind.visit(l);
        l.visit_exit_block_scope();
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // let I;
        // interface I {}
        // {
        //   I;
        // }
        Diag_Collector v;
        Variable_Analyzer l(&v, &default_globals, javascript_var_options);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_variable_declaration(identifier_of(declaration),
                                     Variable_Kind::_interface,
                                     Variable_Declaration_Flags::none);
        l.visit_enter_block_scope();
        visit_kind.visit(l);
        l.visit_exit_block_scope();
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // let I;
        // interface I {}
        // I;
        Diag_Collector v;
        Variable_Analyzer l(&v, &default_globals, javascript_var_options);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_variable_declaration(identifier_of(declaration),
                                     Variable_Kind::_interface,
                                     Variable_Declaration_Flags::none);
        visit_kind.visit(l);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // interface I {}
        // let I;
        // I;
        Diag_Collector v;
        Variable_Analyzer l(&v, &default_globals, javascript_var_options);
        l.visit_variable_declaration(identifier_of(declaration),
                                     Variable_Kind::_interface,
                                     Variable_Declaration_Flags::none);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind,
                                     Variable_Declaration_Flags::none);
        visit_kind.visit(l);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }

      {
        // (() => {
        //   I;
        // });
        // interface I {}
        // let I;
        Diag_Collector v;
        Variable_Analyzer l(&v, &default_globals, javascript_var_options);
        l.visit_enter_function_scope();
        l.visit_enter_function_scope_body();
        visit_kind.visit(l);
        l.visit_exit_function_scope();
        l.visit_variable_declaration(identifier_of(declaration),
                                     Variable_Kind::_interface,
                                     Variable_Declaration_Flags::none);
        l.visit_variable_declaration(identifier_of(outer_declaration),
                                     outer_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, visit_kind.get_diags_matcher(outer_kind));
      }
    }
  }
}

TEST(Test_Variable_Analyzer_Type, mixing_non_type_and_type_only_is_okay) {
  const Char8 type_declaration[] = u8"C";
  const Char8 non_type_declaration[] = u8"C";

  for (Variable_Kind type_declaration_kind : {Variable_Kind::_interface}) {
    for (Variable_Kind non_type_declaration_kind : {
             Variable_Kind::_arrow_parameter,
             Variable_Kind::_catch,
             Variable_Kind::_const,
             Variable_Kind::_function,
             Variable_Kind::_function_parameter,
             Variable_Kind::_index_signature_parameter,
             Variable_Kind::_let,
             Variable_Kind::_var,
         }) {
      SCOPED_TRACE(type_declaration_kind);
      SCOPED_TRACE(non_type_declaration_kind);

      {
        // interface C {}
        // let C;
        Diag_Collector v;
        Variable_Analyzer l(&v, &default_globals, javascript_var_options);
        l.visit_variable_declaration(identifier_of(type_declaration),
                                     type_declaration_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_variable_declaration(identifier_of(non_type_declaration),
                                     non_type_declaration_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, IsEmpty());
      }

      {
        // let C;
        // interface C {}
        Diag_Collector v;
        Variable_Analyzer l(&v, &default_globals, javascript_var_options);
        l.visit_variable_declaration(identifier_of(non_type_declaration),
                                     non_type_declaration_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_variable_declaration(identifier_of(type_declaration),
                                     type_declaration_kind,
                                     Variable_Declaration_Flags::none);
        l.visit_end_of_module();

        EXPECT_THAT(v.errors, IsEmpty());
      }
    }
  }
}

TEST(Test_Variable_Analyzer_Type,
     interfaces_merge_with_interfaces_and_classes) {
  test_parse_and_analyze(u8"interface C {}  class C {} "_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"interface C {}  interface C {} "_sv, no_diags,
                         typescript_analyze_options, default_globals);
  test_parse_and_analyze(u8"class C {}  interface C {} "_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

// When we import, we don't know whether the imported declaration is type-only
// (interface), runtime-only (function or variable), or mixed (class). We take
// the conservative approach and assume that the user wrote correct code (thus
// we report no diagnostic).
TEST(Test_Variable_Analyzer_Type, mixing_interface_and_import_is_not_an_error) {
  test_parse_and_analyze(
      u8"import {C} from 'module';"_sv
      u8"interface C {} "_sv,
      no_diags, typescript_analyze_options, default_globals);

  test_parse_and_analyze(
      u8"interface C {} "_sv
      u8"import {C} from 'module';"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, interfaces_conflict_with_generic_parameters) {
  test_parse_and_analyze(
      u8"function f<I>() { interface I {}  }"_sv,
      u8"                            ^ Diag_Redeclaration_Of_Variable.redeclaration\n"_diag
      u8"           ^ .original_declaration"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type, type_predicate_finds_function_parameter) {
  test_parse_and_analyze(
      u8"((p): p is any => {"_sv
      u8"});"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     type_predicate_finds_function_parameter_in_function_type) {
  test_parse_and_analyze(u8"let f: (p) => p is any;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     type_predicate_does_not_find_outer_function_parameter) {
  test_parse_and_analyze(
      u8"((outer) => { ((inner): outer is any => { }); });"_sv,
      u8"                        ^^^^^ Diag_Use_Of_Undeclared_Parameter_In_Type_Predicate.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     type_predicate_does_not_find_generic_parameter) {
  test_parse_and_analyze(
      u8"(<T>(p): T is any => { });"_sv,
      u8"         ^ Diag_Use_Of_Undeclared_Parameter_In_Type_Predicate.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     assertion_signature_finds_function_parameter) {
  test_parse_and_analyze(
      u8"((p): asserts p => {"_sv
      u8"});"_sv,
      no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     assertion_signature_does_not_find_outer_function_parameter) {
  test_parse_and_analyze(
      u8"((outer) => { ((inner): asserts outer => { }); });"_sv,
      u8"                                ^^^^^ Diag_Use_Of_Undeclared_Parameter_In_Assertion_Signature.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     assertion_signature_does_not_find_generic_parameter) {
  test_parse_and_analyze(
      u8"(<T>(p): asserts T is String => { });"_sv,
      u8"                 ^ Diag_Use_Of_Undeclared_Parameter_In_Assertion_Signature.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     variables_referenced_in_conditional_type_scope_are_looked_up) {
  test_parse_and_analyze(
      u8"type Derived = null; type Base = null; null as (Derived extends Base ? TrueType : FalseType)"_sv,
      u8"                                                                                  ^^^^^^^^^ Diag_Use_Of_Undeclared_Type.name"_diag,
      u8"                                                                       ^^^^^^^^ Diag_Use_Of_Undeclared_Type.name"_diag,
      typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     infer_variables_in_conditional_type_scope_are_declared) {
  test_parse_and_analyze(u8"null as (any extends infer T ? T : false)"_sv,
                         no_diags, typescript_analyze_options, default_globals);
}

TEST(Test_Variable_Analyzer_Type,
     type_can_use_runtime_variable_before_declaration) {
  test_parse_and_analyze(u8"let x: typeof y; let y: string;"_sv, no_diags,
                         typescript_analyze_options, default_globals);
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
