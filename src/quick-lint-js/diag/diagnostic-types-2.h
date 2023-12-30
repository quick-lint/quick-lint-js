// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <quick-lint-js/diag/diagnostic.h>
#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wunknown-attributes")
QLJS_WARNING_IGNORE_GCC("-Wattributes")

// HACK(strager): Stringify arg names in attributes to work around bugs in
// GCC 8.3 and GCC 9.2.
// TODO(#1069): Remove when we upgrade to a working GCC.
#define ARG(arg_name) #arg_name

#define QLJS_RESERVED_DIAG(code) \
  /* Used by generate-diagnostic-metadata.cpp, not the C++ compiler. */

namespace quick_lint_js {
// Reserved error codes. These codes were used in the past but no longer mean
// anything.
//
// When removing a diagnostic, mark its code as reserved here.
QLJS_RESERVED_DIAG("E0042")
QLJS_RESERVED_DIAG("E0242")
QLJS_RESERVED_DIAG("E0271")
QLJS_RESERVED_DIAG("E0279")
QLJS_RESERVED_DIAG("E0391")
QLJS_RESERVED_DIAG("E0707")

struct Diag_Abstract_Field_Cannot_Have_Initializer {
  [[qljs::diag("E0295", Diagnostic_Severity::error)]]  //
  [[qljs::message("abstract fields cannot have default values",
                  ARG(equal))]]                                           //
  [[qljs::message("field marked abstract here", ARG(abstract_keyword))]]  //
  Source_Code_Span equal;
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Methods_Cannot_Be_Async {
  [[qljs::diag("E0298", Diagnostic_Severity::error)]]  //
  [[qljs::message("abstract methods cannot be marked 'async'",
                  ARG(async_keyword))]]  //
  Source_Code_Span async_keyword;
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Methods_Cannot_Be_Generators {
  [[qljs::diag("E0299", Diagnostic_Severity::error)]]  //
  [[qljs::message("abstract methods cannot be marked as a generator",
                  ARG(star))]]  //
  Source_Code_Span star;
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Property_Not_Allowed_In_Interface {
  [[qljs::diag("E0297", Diagnostic_Severity::error)]]  //
  [[qljs::message("abstract properties are not allowed in interfaces",
                  ARG(abstract_keyword))]]  //
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class {
  [[qljs::diag("E0296", Diagnostic_Severity::error)]]  //
  [[qljs::message("abstract properties are only allowed in abstract classes",
                  ARG(abstract_keyword))]]                               //
  [[qljs::message("class is not marked abstract", ARG(class_keyword))]]  //
  Source_Code_Span abstract_keyword;
  Source_Code_Span class_keyword;
};

struct Diag_Abstract_Methods_Cannot_Contain_Bodies {
  [[qljs::diag("E0294", Diagnostic_Severity::error)]]  //
  [[qljs::message("abstract methods cannot contain a body",
                  ARG(body_start))]]  //
  Source_Code_Span body_start;
};

struct Diag_Adjacent_JSX_Without_Parent {
  [[qljs::diag("E0189", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing '<>' and '</>' to enclose multiple children",
                  ARG(begin))]]                     //
  [[qljs::message("children end here", ARG(end))]]  //
  Source_Code_Span begin;
  Source_Code_Span begin_of_second_element;
  Source_Code_Span end;
};

struct Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses {
  [[qljs::diag("E0255", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing parentheses around parameter",
                  ARG(parameter_and_annotation))]]  //
  [[qljs::message("TypeScript type annotation requires parentheses",
                  ARG(type_colon))]]  //
  Source_Code_Span parameter_and_annotation;
  Source_Code_Span type_colon;
};

struct Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void {
  [[qljs::diag("E0348", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "unexpected '?' in type; use '| void' to make an optional type",
      ARG(question))]]  //
  Source_Code_Span question;
};

struct Diag_Assignment_Before_Variable_Declaration {
  [[qljs::diag("E0001", Diagnostic_Severity::error)]]  //
  [[qljs::message("variable assigned before its declaration",
                  ARG(assignment))]]                             //
  [[qljs::message("variable declared here", ARG(declaration))]]  //
  Source_Code_Span assignment;
  Source_Code_Span declaration;
};

struct Diag_Assignment_Makes_Condition_Constant {
  [[qljs::diag("E0188", Diagnostic_Severity::warning)]]  //
  [[qljs::message("'=' changes variables; to compare, use '===' instead",
                  ARG(assignment_operator))]]  //
  Source_Code_Span assignment_operator;
};

struct Diag_Assignment_To_Const_Global_Variable {
  [[qljs::diag("E0002", Diagnostic_Severity::error)]]                        //
  [[qljs::message("assignment to const global variable", ARG(assignment))]]  //
  Source_Code_Span assignment;
};

struct Diag_Assignment_To_Const_Variable {
  [[qljs::diag("E0003", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot assign to {1:singular}", ARG(assignment),
                  ARG(var_kind))]]  //
  [[qljs::message("{1:headlinese} declared here", ARG(declaration),
                  ARG(var_kind))]]  //
  Source_Code_Span declaration;
  Source_Code_Span assignment;
  Variable_Kind var_kind;
};

struct Diag_Assignment_To_Imported_Variable {
  [[qljs::diag("E0185", Diagnostic_Severity::error)]]                     //
  [[qljs::message("assignment to imported variable", ARG(assignment))]]   //
  [[qljs::message("imported variable declared here", ARG(declaration))]]  //
  Source_Code_Span declaration;
  Source_Code_Span assignment;
  Variable_Kind var_kind;
};

struct Diag_Assignment_To_Const_Variable_Before_Its_Declaration {
  [[qljs::diag("E0004", Diagnostic_Severity::error)]]  //
  [[qljs::message("assignment to const variable before its declaration",
                  ARG(assignment))]]                                   //
  [[qljs::message("const variable declared here", ARG(declaration))]]  //
  Source_Code_Span declaration;
  Source_Code_Span assignment;
  Variable_Kind var_kind;
};

struct Diag_Assignment_To_Undeclared_Variable {
  [[qljs::diag("E0059", Diagnostic_Severity::warning)]]                    //
  [[qljs::message("assignment to undeclared variable", ARG(assignment))]]  //
  Source_Code_Span assignment;
};

struct Diag_Await_Operator_Outside_Async {
  [[qljs::diag("E0162", Diagnostic_Severity::error)]]  //
  [[qljs::message("'await' is only allowed in async functions",
                  ARG(await_operator))]]  //
  Source_Code_Span await_operator;
};

struct Diag_Await_Followed_By_Arrow_Function {
  [[qljs::diag("E0178", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'await' cannot be followed by an arrow function; use 'async' instead",
      ARG(await_operator))]]  //
  Source_Code_Span await_operator;
};

struct Diag_Async_Static_Method {
  [[qljs::diag("E0269", Diagnostic_Severity::error)]]  //
  [[qljs::message("'async static' is not allowed; write 'static async' instead",
                  ARG(async_static))]]  //
  Source_Code_Span async_static;
};

struct Diag_Async_Export_Function {
  [[qljs::diag("E0326", Diagnostic_Severity::error)]]  //
  [[qljs::message("'async export' is not allowed; write 'export async' instead",
                  ARG(async_export))]]  //
  Source_Code_Span async_export;
};

struct Diag_Cyclic_TypeScript_Type_Definition {
  [[qljs::diag("E0384", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot use type directly in its own definition",
                  ARG(use))]]                                            //
  [[qljs::message("type {0} is being defined here", ARG(declaration))]]  //
  Source_Code_Span use;
  Source_Code_Span declaration;
  Variable_Kind kind;
};

struct Diag_DTS_Function_Cannot_Be_Async {
  [[qljs::diag("E0388", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "functions in .d.ts files cannot be async; return a Promise type",
      ARG(async_keyword))]]  //
  Source_Code_Span async_keyword;
};

struct Diag_DTS_Function_Cannot_Be_Generator {
  [[qljs::diag("E0389", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "functions in .d.ts files cannot be generators; return a Generator type",
      ARG(star))]]  //
  Source_Code_Span star;
};

struct Diag_DTS_Function_Cannot_Have_Body {
  [[qljs::diag("E0387", Diagnostic_Severity::error)]]  //
  [[qljs::message("functions in .d.ts files cannot have a body",
                  ARG(body_start))]]  //
  Source_Code_Span body_start;
};

struct Diag_DTS_Missing_Declare_Or_Export {
  [[qljs::diag("E0386", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare' or 'export' is required for {1} in .d.ts files",
                  ARG(expected), ARG(declaring_token))]]  //
  Source_Code_Span expected;
  Source_Code_Span declaring_token;
};

struct Diag_DTS_Non_Declaring_Statement {
  [[qljs::diag("E0390", Diagnostic_Severity::error)]]  //
  [[qljs::message(".d.ts files cannot contain statements, only declarations",
                  ARG(first_statement_token))]]  //
  Source_Code_Span first_statement_token;
};

struct Diag_DTS_Var_Cannot_Have_Initializer {
  [[qljs::diag("E0385", Diagnostic_Severity::error)]]  //
  [[qljs::message("{1} cannot have an initializer in a .d.ts file", ARG(equal),
                  ARG(declaring_token))]]  //
  Source_Code_Span equal;
  Source_Code_Span declaring_token;
};

struct Diag_Declare_Class_Fields_Cannot_Have_Initializers {
  [[qljs::diag("E0335", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare class' fields cannot be initalized",
                  ARG(equal))]]  //
  Source_Code_Span equal;
};

struct Diag_Declare_Class_Methods_Cannot_Be_Async {
  [[qljs::diag("E0338", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare class' methods cannot be marked 'async'",
                  ARG(async_keyword))]]  //
  Source_Code_Span async_keyword;
};

struct Diag_Declare_Class_Methods_Cannot_Be_Generators {
  [[qljs::diag("E0337", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare class' methods cannot be marked as a generator",
                  ARG(star))]]  //
  Source_Code_Span star;
};

struct Diag_Declare_Class_Methods_Cannot_Contain_Bodies {
  [[qljs::diag("E0333", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare class' methods cannot contain a body",
                  ARG(body_start))]]  //
  Source_Code_Span body_start;
};

struct Diag_Declare_Abstract_Class_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0340", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript 'declare abstract class' is not allowed in JavaScript",
      ARG(declare_keyword))]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Class_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0339", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript 'declare class' is not allowed in JavaScript",
                  ARG(declare_keyword))]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Function_Cannot_Be_Async {
  [[qljs::diag("E0354", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare function' cannot be marked 'async'",
                  ARG(async_keyword))]]  //
  Source_Code_Span async_keyword;
};

struct Diag_Declare_Function_Cannot_Be_Generator {
  [[qljs::diag("E0355", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare function' cannot be marked as a generator",
                  ARG(star))]]  //
  Source_Code_Span star;
};

struct Diag_Declare_Function_Cannot_Have_Body {
  [[qljs::diag("E0353", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare function' cannot have a body",
                  ARG(body_start))]]                                  //
  [[qljs::message("'declare function' here", ARG(declare_keyword))]]  //
  Source_Code_Span body_start;
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Function_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0352", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript 'declare function' is not allowed in JavaScript",
                  ARG(declare_keyword))]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace {
  [[qljs::diag("E0358", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'declare' should not be written inside a 'declare namespace'",
      ARG(declare_keyword))]]  //
  [[qljs::message("containing 'declare namespace' starts here",
                  ARG(declare_namespace_declare_keyword))]]  //
  Source_Code_Span declare_keyword;
  Source_Code_Span declare_namespace_declare_keyword;
};

struct Diag_Declare_Namespace_Cannot_Contain_Statement {
  [[qljs::diag("E0357", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'declare namespace' cannot contain statements, only declarations",
      ARG(first_statement_token))]]                          //
  [[qljs::message("'declare' here", ARG(declare_keyword))]]  //
  Source_Code_Span first_statement_token;
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Namespace_Cannot_Import_Module {
  [[qljs::diag("E0362", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot import a module from inside a 'declare namespace'",
                  ARG(importing_keyword))]]                                   //
  [[qljs::message("'declare namespace' starts here", ARG(declare_keyword))]]  //
  Source_Code_Span importing_keyword;
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Var_Cannot_Have_Initializer {
  [[qljs::diag("E0351", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare {1}' cannot have initializer", ARG(equal),
                  ARG(declaring_token))]]  //
  [[qljs::message("'declare {1}' started here", ARG(declare_keyword),
                  ARG(declaring_token))]]  //
  Source_Code_Span equal;
  Source_Code_Span declare_keyword;
  Source_Code_Span declaring_token;
};

struct Diag_Declare_Var_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0350", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript 'declare {1}' is not allowed in JavaScript",
                  ARG(declare_keyword), ARG(declaring_token))]]  //
  Source_Code_Span declare_keyword;
  Source_Code_Span declaring_token;
};

struct Diag_Decorator_After_Class_Member_Modifiers {
  [[qljs::diag("E0408", Diagnostic_Severity::error)]]  //
  [[qljs::message("decorators must appear before '{1}", ARG(decorator_at),
                  ARG(modifier))]]                                     //
  [[qljs::message("write the decorator before here", ARG(modifier))]]  //
  Source_Code_Span decorator_at;
  Source_Code_Span modifier;
};

struct Diag_Decorator_Before_And_After_Export_Keyword {
  [[qljs::diag("E0414", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "decorators may be before 'export' or here, but not in both locations",
      ARG(decorator_at_after))]]  //
  [[qljs::message("a decorator exists here before 'export'",
                  ARG(decorator_at_before))]]  //
  Source_Code_Span decorator_at_before;
  Source_Code_Span decorator_at_after;
};

struct Diag_Decorator_Not_Allowed_On_Class_Static_Block {
  [[qljs::diag("E0407", Diagnostic_Severity::error)]]  //
  [[qljs::message("static blocks cannot have a decorator",
                  ARG(decorator_at))]]  //
  [[qljs::message("static block starts here",
                  ARG(static_keyword))]]  //
  Source_Code_Span decorator_at;
  Source_Code_Span static_keyword;
};

struct Diag_Decorator_In_TypeScript_Interface {
  [[qljs::diag("E0411", Diagnostic_Severity::error)]]  //
  [[qljs::message("decorators are not allowed inside TypeScript interfaces",
                  ARG(decorator_at))]]  //
  Source_Code_Span decorator_at;
};

struct Diag_Decorator_On_Abstract_Class_Member {
  [[qljs::diag("E0412", Diagnostic_Severity::error)]]  //
  [[qljs::message("decorators are not allowed on abstract properties",
                  ARG(decorator_at))]]  //
  [[qljs::message("property declared 'abstract' here",
                  ARG(abstract_keyword))]]  //
  Source_Code_Span decorator_at;
  Source_Code_Span abstract_keyword;
};

struct Diag_Decorator_On_Overload_Signature {
  [[qljs::diag("E0413", Diagnostic_Severity::error)]]  //
  [[qljs::message("decorators must appear after overload signatures",
                  ARG(decorator_at))]]  //
  [[qljs::message("decorator belongs immediately before this overloaded method",
                  ARG(expected_location))]]  //
  Source_Code_Span decorator_at;
  Source_Code_Span expected_location;
};

struct Diag_Function_Async_Function {
  [[qljs::diag("E0327", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'function async' is not allowed; write 'async function' instead",
      ARG(function_async))]]  //
  Source_Code_Span function_async;
};

struct Diag_Big_Int_Literal_Contains_Decimal_Point {
  [[qljs::diag("E0005", Diagnostic_Severity::error)]]                     //
  [[qljs::message("BigInt literal contains decimal point", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Big_Int_Literal_Contains_Exponent {
  [[qljs::diag("E0006", Diagnostic_Severity::error)]]                //
  [[qljs::message("BigInt literal contains exponent", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_C_Style_For_Loop_Is_Missing_Third_Component {
  [[qljs::diag("E0093", Diagnostic_Severity::error)]]  //
  [[qljs::message("C-style for loop is missing its third component",
                  ARG(expected_last_component))]]  //
  Source_Code_Span expected_last_component;
  Source_Code_Span existing_semicolon;
};

struct Diag_Cannot_Assign_To_Loop_Variable_In_For_Of_Or_In_Loop {
  [[qljs::diag("E0173", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot assign to loop variable in for of/in loop",
                  ARG(equal_token))]]  //
  Source_Code_Span equal_token;
};

struct Diag_Cannot_Access_Private_Identifier_Outside_Class {
  [[qljs::diag("E0208", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot access private identifier outside class",
                  ARG(private_identifier))]]  //
  Source_Code_Span private_identifier;
};

struct Diag_Cannot_Assign_To_Variable_Named_Async_In_For_Of_Loop {
  [[qljs::diag("E0082", Diagnostic_Severity::error)]]  //
  [[qljs::message("assigning to 'async' in a for-of loop requires parentheses",
                  ARG(async_identifier))]]  //
  Source_Code_Span async_identifier;
};

struct Diag_Cannot_Declare_Await_In_Async_Function {
  [[qljs::diag("E0069", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot declare 'await' inside async function",
                  ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Declare_Class_Named_Let {
  [[qljs::diag("E0007", Diagnostic_Severity::error)]]            //
  [[qljs::message("classes cannot be named 'let'", ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Declare_Variable_Named_Let_With_Let {
  [[qljs::diag("E0008", Diagnostic_Severity::error)]]  //
  [[qljs::message("let statement cannot declare variables named 'let'",
                  ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Declare_Variable_With_Keyword_Name {
  [[qljs::diag("E0124", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot declare variable named keyword '{0}'",
                  ARG(keyword))]]  //
  Source_Code_Span keyword;
};

struct Diag_Cannot_Declare_Yield_In_Generator_Function {
  [[qljs::diag("E0071", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot declare 'yield' inside generator function",
                  ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Export_Default_Variable {
  [[qljs::diag("E0076", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot declare and export variable with 'export default'",
                  ARG(declaring_token))]]  //
  Source_Code_Span declaring_token;
};

struct Diag_Cannot_Export_Let {
  [[qljs::diag("E0009", Diagnostic_Severity::error)]]                        //
  [[qljs::message("cannot export variable named 'let'", ARG(export_name))]]  //
  Source_Code_Span export_name;
};

struct Diag_Cannot_Export_Variable_Named_Keyword {
  [[qljs::diag("E0144", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot export variable named keyword '{0}'",
                  ARG(export_name))]]  //
  Source_Code_Span export_name;
};

struct Diag_Cannot_Import_Let {
  [[qljs::diag("E0010", Diagnostic_Severity::error)]]         //
  [[qljs::message("cannot import 'let'", ARG(import_name))]]  //
  Source_Code_Span import_name;
};

struct Diag_Cannot_Import_Variable_Named_Keyword {
  [[qljs::diag("E0145", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot import variable named keyword '{0}'",
                  ARG(import_name))]]  //
  Source_Code_Span import_name;
};

struct Diag_Cannot_Import_From_Unquoted_Module {
  [[qljs::diag("E0235", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing quotes around module name '{0}'",
                  ARG(import_name))]]  //
  Source_Code_Span import_name;
};

struct Diag_Cannot_Refer_To_Private_Variable_Without_Object {
  [[qljs::diag("E0155", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "cannot reference private variables without object; use 'this.'",
      ARG(private_identifier))]]  //
  Source_Code_Span private_identifier;
};

struct Diag_Cannot_Update_Variable_During_Declaration {
  [[qljs::diag("E0136", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot update variable with '{0}' while declaring it",
                  ARG(updating_operator))]]  //
  [[qljs::message("remove '{0}' to update an existing variable",
                  ARG(declaring_token))]]  //
  Source_Code_Span declaring_token;
  Source_Code_Span updating_operator;
};

struct Diag_Catch_Without_Try {
  [[qljs::diag("E0117", Diagnostic_Severity::error)]]                      //
  [[qljs::message("unexpected 'catch' without 'try'", ARG(catch_token))]]  //
  Source_Code_Span catch_token;
};

struct Diag_Class_Accessor_On_Getter_Or_Setter {
  [[qljs::diag("E0393", Diagnostic_Severity::error)]]  //
  [[qljs::message("'accessor' keyword is not allowed on getters or setters",
                  ARG(accessor_keyword))]]                     //
  [[qljs::message("'{0}' here", ARG(getter_setter_keyword))]]  //
  Source_Code_Span method_start;
  Source_Code_Span accessor_keyword;
  Source_Code_Span getter_setter_keyword;
};

struct Diag_Class_Accessor_On_Method {
  [[qljs::diag("E0392", Diagnostic_Severity::error)]]  //
  [[qljs::message("'accessor' keyword is not allowed on methods",
                  ARG(accessor_keyword))]]                    //
  [[qljs::message("method starts here", ARG(method_start))]]  //
  Source_Code_Span method_start;
  Source_Code_Span accessor_keyword;
};

struct Diag_Class_Conflicting_Modifiers {
  [[qljs::diag("E0394", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{0}' is not allowed with '{1}'", ARG(second_modifier),
                  ARG(first_modifier))]]                //
  [[qljs::message("'{0}' here", ARG(first_modifier))]]  //
  Source_Code_Span second_modifier;
  Source_Code_Span first_modifier;
};

struct
    Diag_Class_Modifier_Missing_On_Method_With_TypeScript_Overload_Signature {
  [[qljs::diag("E0403", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{0}' is missing on overloaded method",
                  ARG(signature_modifier))]]  //
  [[qljs::message("write '{1}' here or remove it from the overload signature",
                  ARG(missing_method_modifier), ARG(signature_modifier))]]  //
  Source_Code_Span signature_modifier;
  Source_Code_Span missing_method_modifier;
};

struct Diag_Class_Modifier_Missing_On_TypeScript_Overload_Signature {
  [[qljs::diag("E0404", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{1}' is missing on overload signature",
                  ARG(missing_signature_modifier), ARG(method_modifier))]]  //
  [[qljs::message(
      "overload signature must match modifiers on this overload method",
      ARG(method_modifier))]]  //
  Source_Code_Span missing_signature_modifier;
  Source_Code_Span method_modifier;
};

struct Diag_Class_Modifier_Must_Preceed_Other_Modifier {
  [[qljs::diag("E0395", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{0}' must precede '{1}'", ARG(expected_first_modifier),
                  ARG(expected_second_modifier))]]  //
  Source_Code_Span expected_first_modifier;
  Source_Code_Span expected_second_modifier;
};

struct Diag_Class_Modifier_Not_Allowed_On_TypeScript_Overload_Signature {
  [[qljs::diag("E0402", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{0}' is not allowed in TypeScript overload signatures",
                  ARG(modifier))]]  //
  Source_Code_Span modifier;
};

struct Diag_Class_Statement_Not_Allowed_In_Body {
  [[qljs::diag("E0149", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing body for {1:headlinese}", ARG(expected_body),
                  ARG(kind_of_statement))]]  //
  [[qljs::message(
      "a class statement is not allowed as the body of {1:singular}",
      ARG(class_keyword), ARG(kind_of_statement))]]  //
  Statement_Kind kind_of_statement;
  Source_Code_Span expected_body;
  Source_Code_Span class_keyword;
};

struct Diag_Character_Disallowed_In_Identifiers {
  [[qljs::diag("E0011", Diagnostic_Severity::error)]]  //
  [[qljs::message("character is not allowed in identifiers",
                  ARG(character))]]  //
  Source_Code_Span character;
};

struct Diag_Comma_Not_Allowed_After_Spread_Parameter {
  [[qljs::diag("E0070", Diagnostic_Severity::error)]]  //
  [[qljs::message("commas are not allowed after spread parameter",
                  ARG(comma))]]  //
  Source_Code_Span comma;
  Source_Code_Span spread;
};

struct Diag_Comma_Not_Allowed_Before_First_Generic_Parameter {
  [[qljs::diag("E0262", Diagnostic_Severity::error)]]  //
  [[qljs::message("leading commas are not allowed in generic parameter lists",
                  ARG(unexpected_comma))]]  //
  Source_Code_Span unexpected_comma;
};

struct Diag_Comma_Not_Allowed_Between_Class_Methods {
  [[qljs::diag("E0209", Diagnostic_Severity::error)]]  //
  [[qljs::message("commas are not allowed between class methods",
                  ARG(unexpected_comma))]]  //
  Source_Code_Span unexpected_comma;
};

struct Diag_Config_Json_Syntax_Error {
  [[qljs::diag("E0164", Diagnostic_Severity::error)]]  //
  [[qljs::message("JSON syntax error", ARG(where))]]   //
  Source_Code_Span where;
};

struct Diag_Config_Global_Groups_Group_Type_Mismatch {
  [[qljs::diag("E0170", Diagnostic_Severity::error)]]                         //
  [[qljs::message("\"global-groups\" entries must be strings", ARG(group))]]  //
  Source_Code_Span group;
};

struct Diag_Config_Global_Groups_Type_Mismatch {
  [[qljs::diag("E0169", Diagnostic_Severity::error)]]  //
  [[qljs::message("\"global-groups\" must be a boolean or an array",
                  ARG(value))]]  //
  Source_Code_Span value;
};

struct Diag_Config_Globals_Descriptor_Type_Mismatch {
  [[qljs::diag("E0171", Diagnostic_Severity::error)]]  //
  [[qljs::message("\"globals\" descriptor must be a boolean or an object",
                  ARG(descriptor))]]  //
  Source_Code_Span descriptor;
};

struct Diag_Config_Globals_Descriptor_Shadowable_Type_Mismatch {
  [[qljs::diag("E0166", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "\"globals\" descriptor \"shadowable\" property must be a boolean",
      ARG(value))]]  //
  Source_Code_Span value;
};

struct Diag_Config_Globals_Descriptor_Writable_Type_Mismatch {
  [[qljs::diag("E0167", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "\"globals\" descriptor \"writable\" property must be a boolean",
      ARG(value))]]  //
  Source_Code_Span value;
};

struct Diag_Config_Globals_Type_Mismatch {
  [[qljs::diag("E0168", Diagnostic_Severity::error)]]             //
  [[qljs::message("\"globals\" must be an object", ARG(value))]]  //
  Source_Code_Span value;
};

struct Diag_Depth_Limit_Exceeded {
  [[qljs::diag("E0203", Diagnostic_Severity::error)]]    //
  [[qljs::message("depth limit exceeded", ARG(token))]]  //
  Source_Code_Span token;
};

struct Diag_Dot_Not_Allowed_After_Generic_Arguments_In_Type {
  [[qljs::diag("E0259", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'.' is not allowed after generic arguments; write [\"{1}\"] instead",
      ARG(dot), ARG(property_name))]]  //
  Source_Code_Span dot;
  Source_Code_Span property_name;
};

struct Diag_Dot_Dot_Is_Not_An_Operator {
  [[qljs::diag("E0053", Diagnostic_Severity::error)]]                        //
  [[qljs::message("missing property name between '.' and '.'", ARG(dots))]]  //
  Source_Code_Span dots;
};

struct Diag_Duplicated_Cases_In_Switch_Statement {
  [[qljs::diag("E0347", Diagnostic_Severity::warning)]]  //
  [[qljs::message("duplicated case clause in switch statement",
                  ARG(duplicated_switch_case))]]                           //
  [[qljs::message("this case will run instead", ARG(first_switch_case))]]  //
  Source_Code_Span first_switch_case;
  Source_Code_Span duplicated_switch_case;
};

struct Diag_Fallthrough_Without_Comment_In_Switch {
  [[qljs::diag("E0427", Diagnostic_Severity::warning)]]  //
  [
      [qljs::message("missing 'break;' or '// fallthrough' comment between "
                     "statement and 'case'",
                     ARG(end_of_case))]]  //
  Source_Code_Span end_of_case;
};

struct Diag_Else_Has_No_If {
  [[qljs::diag("E0065", Diagnostic_Severity::error)]]                     //
  [[qljs::message("'else' has no corresponding 'if'", ARG(else_token))]]  //
  Source_Code_Span else_token;
};

struct Diag_Equals_Does_Not_Distribute_Over_Or {
  [[qljs::diag("E0190", Diagnostic_Severity::warning)]]  //
  [[qljs::message(
      "missing comparison; '{1}' does not extend to the right side of '{0}'",
      ARG(or_operator), ARG(equals_operator))]]                //
  [[qljs::message("'{0}' found here", ARG(equals_operator))]]  //
  Source_Code_Span or_operator;
  Source_Code_Span equals_operator;
};

struct Diag_Escaped_Character_Disallowed_In_Identifiers {
  [[qljs::diag("E0012", Diagnostic_Severity::error)]]  //
  [[qljs::message("escaped character is not allowed in identifiers",
                  ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Escaped_Code_Point_In_Identifier_Out_Of_Range {
  [[qljs::diag("E0013", Diagnostic_Severity::error)]]                 //
  [[qljs::message("code point out of range", ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Escaped_Code_Point_In_Unicode_Out_Of_Range {
  [[qljs::diag("E0207", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "code point in Unicode escape sequence must not be greater than U+10FFFF",
      ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Escaped_Hyphen_Not_Allowed_In_JSX_Tag {
  [[qljs::diag("E0019", Diagnostic_Severity::error)]]  //
  [[qljs::message("escaping '-' is not allowed in tag names; write '-' instead",
                  ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Extra_Comma_Not_Allowed_Between_Arguments {
  [[qljs::diag("E0068", Diagnostic_Severity::error)]]  //
  [[qljs::message("extra ',' is not allowed between function call arguments",
                  ARG(comma))]]  //
  Source_Code_Span comma;
};

struct Diag_Extra_Comma_Not_Allowed_Between_Enum_Members {
  [[qljs::diag("E0248", Diagnostic_Severity::error)]]  //
  [[qljs::message("extra ',' is not allowed between enum members",
                  ARG(comma))]]  //
  Source_Code_Span comma;
};

struct Diag_Misleading_Comma_Operator_In_Index_Operation {
  [[qljs::diag("E0450", Diagnostic_Severity::warning)]]                     //
  [[qljs::message("misleading use of ',' operator in index", ARG(comma))]]  //
  [[qljs::message("index starts here", ARG(left_square))]]                  //
  Source_Code_Span comma;
  Source_Code_Span left_square;
};

struct Diag_Misleading_Comma_Operator_In_Conditional_Statement {
  [[qljs::diag("E0451", Diagnostic_Severity::warning)]]  //
  [[qljs::message("misleading use of ',' operator in conditional statement",
                  ARG(comma))]]  //
  Source_Code_Span comma;
};

struct Diag_Empty_Paren_After_Control_Statement {
  [[qljs::diag("E0452", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected expression after '('",
                  ARG(expected_expression))]]                               //
  [[qljs::message("'{1}' statement starts here", ARG(token), ARG(token))]]  //
  Source_Code_Span token;
  Source_Code_Span expected_expression;
};

struct Diag_Expected_As_Before_Imported_Namespace_Alias {
  [[qljs::diag("E0126", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected 'as' between '{1}' and '{2}'",
                  ARG(star_through_alias_token), ARG(star_token),
                  ARG(alias))]]  //
  Source_Code_Span star_through_alias_token;
  Source_Code_Span alias;
  Source_Code_Span star_token;
};

struct Diag_Expected_Comma_To_Separate_Object_Literal_Entries {
  [[qljs::diag("E0131", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected ',' between object literal entries",
                  ARG(unexpected_token))]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Expected_Expression_Before_Newline {
  [[qljs::diag("E0014", Diagnostic_Severity::error)]]                  //
  [[qljs::message("expected expression before newline", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Expected_Expression_For_Switch_Case {
  [[qljs::diag("E0140", Diagnostic_Severity::error)]]                     //
  [[qljs::message("expected expression after 'case'", ARG(case_token))]]  //
  Source_Code_Span case_token;
};

struct Diag_Expected_Expression_Before_Semicolon {
  [[qljs::diag("E0015", Diagnostic_Severity::error)]]                    //
  [[qljs::message("expected expression before semicolon", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Expected_From_And_Module_Specifier {
  [[qljs::diag("E0129", Diagnostic_Severity::error)]]                      //
  [[qljs::message("expected 'from \"name_of_module.mjs\"'", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Expected_From_Before_Module_Specifier {
  [[qljs::diag("E0128", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected 'from' before module specifier",
                  ARG(module_specifier))]]  //
  Source_Code_Span module_specifier;
};

struct Diag_Expected_Hex_Digits_In_Unicode_Escape {
  [[qljs::diag("E0016", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected hexadecimal digits in Unicode escape sequence",
                  ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Expected_Left_Curly {
  [[qljs::diag("E0107", Diagnostic_Severity::error)]]           //
  [[qljs::message("expected '{{'", ARG(expected_left_curly))]]  //
  Source_Code_Span expected_left_curly;
};

struct Diag_Expected_Right_Paren_For_Function_Call {
  [[qljs::diag("E0141", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected ')' to close function call",
                  ARG(expected_right_paren))]]                      //
  [[qljs::message("function call started here", ARG(left_paren))]]  //
  Source_Code_Span expected_right_paren;
  Source_Code_Span left_paren;
};

struct Diag_Expected_Parentheses_Around_Do_While_Condition {
  [[qljs::diag("E0084", Diagnostic_Severity::error)]]  //
  [[qljs::message("do-while loop needs parentheses around condition",
                  ARG(condition))]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_Do_While_Condition {
  [[qljs::diag("E0085", Diagnostic_Severity::error)]]  //
  [[qljs::message("do-while loop is missing '{1}' around condition", ARG(where),
                  ARG(token))]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_If_Condition {
  [[qljs::diag("E0017", Diagnostic_Severity::error)]]  //
  [[qljs::message("if statement needs parentheses around condition",
                  ARG(condition))]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_If_Condition {
  [[qljs::diag("E0018", Diagnostic_Severity::error)]]  //
  [[qljs::message("if statement is missing '{1}' around condition", ARG(where),
                  ARG(token))]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_Switch_Condition {
  [[qljs::diag("E0091", Diagnostic_Severity::error)]]  //
  [[qljs::message("switch statement needs parentheses around condition",
                  ARG(condition))]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_Switch_Condition {
  [[qljs::diag("E0092", Diagnostic_Severity::error)]]  //
  [[qljs::message("switch statement is missing '{1}' around condition",
                  ARG(where), ARG(token))]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_While_Condition {
  [[qljs::diag("E0087", Diagnostic_Severity::error)]]  //
  [[qljs::message("while loop needs parentheses around condition",
                  ARG(condition))]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_While_Condition {
  [[qljs::diag("E0088", Diagnostic_Severity::error)]]  //
  [[qljs::message("while loop is missing '{1}' around condition", ARG(where),
                  ARG(token))]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_With_Expression {
  [[qljs::diag("E0089", Diagnostic_Severity::error)]]  //
  [[qljs::message("with statement needs parentheses around expression",
                  ARG(expression))]]  //
  Source_Code_Span expression;
};

struct Diag_Expected_Parenthesis_Around_With_Expression {
  [[qljs::diag("E0090", Diagnostic_Severity::error)]]  //
  [[qljs::message("with statement is missing '{1}' around expression",
                  ARG(where), ARG(token))]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Variable_Name_For_Catch {
  [[qljs::diag("E0135", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected variable name for 'catch'",
                  ARG(unexpected_token))]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Expected_Variable_Name_For_Import_As {
  [[qljs::diag("E0175", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected variable name for 'import'-'as'",
                  ARG(unexpected_token))]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Exporting_Requires_Default {
  [[qljs::diag("E0067", Diagnostic_Severity::error)]]                 //
  [[qljs::message("exporting requires 'default'", ARG(expression))]]  //
  Source_Code_Span expression;
};

struct Diag_Exporting_Requires_Curlies {
  [[qljs::diag("E0066", Diagnostic_Severity::error)]]               //
  [[qljs::message("exporting requires '{{' and '}'", ARG(names))]]  //
  Source_Code_Span names;
};

struct Diag_Exporting_String_Name_Only_Allowed_For_Export_From {
  [[qljs::diag("E0153", Diagnostic_Severity::error)]]  //
  [[qljs::message("forwarding exports are only allowed in export-from",
                  ARG(export_name))]]  //
  Source_Code_Span export_name;
};

struct Diag_Finally_Without_Try {
  [[qljs::diag("E0118", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected 'finally' without 'try'",
                  ARG(finally_token))]]  //
  Source_Code_Span finally_token;
};

struct Diag_Function_Statement_Not_Allowed_In_Body {
  [[qljs::diag("E0148", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing body for {1:headlinese}", ARG(expected_body),
                  ARG(kind_of_statement))]]  //
  [[qljs::message(
      "a function statement is not allowed as the body of {1:singular}",
      ARG(function_keywords), ARG(kind_of_statement))]]  //
  Statement_Kind kind_of_statement;
  Source_Code_Span expected_body;
  Source_Code_Span function_keywords;
};

struct Diag_Generator_Function_Star_Belongs_After_Keyword_Function {
  [[qljs::diag("E0204", Diagnostic_Severity::error)]]  //
  [[qljs::message("generator function '*' belongs after keyword function",
                  ARG(star))]]  //
  Source_Code_Span star;
};

struct Diag_Generator_Function_Star_Belongs_Before_Name {
  [[qljs::diag("E0133", Diagnostic_Severity::error)]]  //
  [[qljs::message("generator function '*' belongs before function name",
                  ARG(star))]]  //
  Source_Code_Span function_name;
  Source_Code_Span star;
};

struct Diag_Getter_Or_Setter_Cannot_Have_TypeScript_Overload_Signature {
  [[qljs::diag("E0401", Diagnostic_Severity::error)]]  //
  [[qljs::message("getters and setters cannot have overload signatures",
                  ARG(get_or_set_token))]]  //
  Source_Code_Span get_or_set_token;
};

struct Diag_Multiple_Commas_In_Generic_Parameter_List {
  [[qljs::diag("E0263", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "only one comma is allowed between or after generic parameters",
      ARG(unexpected_comma))]]  //
  Source_Code_Span unexpected_comma;
};

struct Diag_In_Disallowed_In_C_Style_For_Loop {
  [[qljs::diag("E0108", Diagnostic_Severity::error)]]  //
  [[qljs::message("'in' disallowed in C-style for loop initializer",
                  ARG(in_token))]]  //
  Source_Code_Span in_token;
};

struct Diag_Indexing_Requires_Expression {
  [[qljs::diag("E0075", Diagnostic_Severity::error)]]                 //
  [[qljs::message("indexing requires an expression", ARG(squares))]]  //
  Source_Code_Span squares;
};

struct Diag_Invalid_Expression_Left_Of_Assignment {
  [[qljs::diag("E0020", Diagnostic_Severity::error)]]                     //
  [[qljs::message("invalid expression left of assignment", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Invalid_Hex_Escape_Sequence {
  [[qljs::diag("E0060", Diagnostic_Severity::error)]]  //
  [[qljs::message("invalid hex escape sequence: {0}",
                  ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Invalid_Lone_Literal_In_Object_Literal {
  [[qljs::diag("E0021", Diagnostic_Severity::error)]]                      //
  [[qljs::message("invalid lone literal in object literal", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Invalid_Parameter {
  [[qljs::diag("E0151", Diagnostic_Severity::error)]]              //
  [[qljs::message("invalid function parameter", ARG(parameter))]]  //
  Source_Code_Span parameter;
};

struct Diag_Invalid_Quotes_Around_String_Literal {
  [[qljs::diag("E0197", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{0}' is not allowed for strings; use {1} instead",
                  ARG(opening_quote), ARG(suggested_quote))]]  //
  Source_Code_Span opening_quote;
  Char8 suggested_quote;
};

struct Diag_Invalid_Rhs_For_Dot_Operator {
  [[qljs::diag("E0074", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("'.' operator needs a key name; use + to concatenate "
                  "strings; use [] to access with a dynamic key", ARG(dot))]]  //
  // clang-format on
  Source_Code_Span dot;
};

struct Diag_Invalid_Utf_8_Sequence {
  [[qljs::diag("E0022", Diagnostic_Severity::error)]]         //
  [[qljs::message("invalid UTF-8 sequence", ARG(sequence))]]  //
  Source_Code_Span sequence;
};

struct Diag_JSX_Attribute_Has_Wrong_Capitalization {
  [[qljs::diag("E0192", Diagnostic_Severity::error)]]  //
  [[qljs::message("attribute has wrong capitalization; write '{1}' instead",
                  ARG(attribute_name), ARG(expected_attribute_name))]]  //
  Source_Code_Span attribute_name;
  String8_View expected_attribute_name;
};

struct Diag_JSX_Attribute_Renamed_By_React {
  [[qljs::diag("E0193", Diagnostic_Severity::error)]]  //
  [[qljs::message("misspelled React attribute; write '{1}' instead",
                  ARG(attribute_name), ARG(react_attribute_name))]]  //
  Source_Code_Span attribute_name;
  String8_View react_attribute_name;
};

struct Diag_JSX_Event_Attribute_Should_Be_Camel_Case {
  [[qljs::diag("E0191", Diagnostic_Severity::error)]]  //
  [[qljs::message("event attributes must be camelCase: '{1}'",
                  ARG(attribute_name), ARG(expected_attribute_name))]]  //
  Source_Code_Span attribute_name;
  String8_View expected_attribute_name;
};

struct Diag_JSX_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0177", Diagnostic_Severity::error)]]  //
  [[qljs::message("React/JSX is not allowed in vanilla JavaScript code",
                  ARG(jsx_start))]]  //
  Source_Code_Span jsx_start;
};

struct Diag_JSX_Not_Allowed_In_TypeScript {
  [[qljs::diag("E0306", Diagnostic_Severity::error)]]  //
  [[qljs::message("React/JSX is not allowed in TypeScript code",
                  ARG(jsx_start))]]  //
  Source_Code_Span jsx_start;
};

struct Diag_Keywords_Cannot_Contain_Escape_Sequences {
  [[qljs::diag("E0023", Diagnostic_Severity::error)]]  //
  [[qljs::message("keywords cannot contain escape sequences",
                  ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Label_Named_Await_Not_Allowed_In_Async_Function {
  [[qljs::diag("E0206", Diagnostic_Severity::error)]]  //
  [[qljs::message("label named 'await' not allowed in async function",
                  ARG(await))]]  //
  Source_Code_Span await;
  Source_Code_Span colon;
};

struct Diag_Legacy_Octal_Literal_May_Not_Be_Big_Int {
  [[qljs::diag("E0032", Diagnostic_Severity::error)]]  //
  [[qljs::message("legacy octal literal may not be BigInt",
                  ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Legacy_Octal_Literal_May_Not_Contain_Underscores {
  [[qljs::diag("E0152", Diagnostic_Severity::error)]]  //
  [[qljs::message("legacy octal literals may not contain underscores",
                  ARG(underscores))]]  //
  Source_Code_Span underscores;
};

struct Diag_Let_With_No_Bindings {
  [[qljs::diag("E0024", Diagnostic_Severity::error)]]    //
  [[qljs::message("{0} with no bindings", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Lexical_Declaration_Not_Allowed_In_Body {
  [[qljs::diag("E0150", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing body for {1:headlinese}", ARG(expected_body),
                  ARG(kind_of_statement))]]  //
  [[qljs::message(
      "a lexical declaration is not allowed as the body of {1:singular}",
      ARG(declaring_keyword), ARG(kind_of_statement))]]  //
  Statement_Kind kind_of_statement;
  Source_Code_Span expected_body;
  Source_Code_Span declaring_keyword;
};

struct Diag_Functions_Or_Methods_Should_Not_Have_Arrow_Operator {
  [[qljs::diag("E0174", Diagnostic_Severity::error)]]  //
  [[qljs::message("functions/methods should not have '=>'",
                  ARG(arrow_operator))]]  //
  Source_Code_Span arrow_operator;
};

struct Diag_Methods_Should_Not_Use_Function_Keyword {
  [[qljs::diag("E0072", Diagnostic_Severity::error)]]  //
  [[qljs::message("methods should not use the 'function' keyword",
                  ARG(function_token))]]  //
  Source_Code_Span function_token;
};

struct Diag_Mismatched_JSX_Tags {
  [[qljs::diag("E0187", Diagnostic_Severity::error)]]  //
  [[qljs::message("mismatched JSX tags; expected '</{1}>'",
                  ARG(closing_tag_name), ARG(opening_tag_name_pretty))]]  //
  [[qljs::message("opening '<{1}>' tag here", ARG(opening_tag_name),
                  ARG(opening_tag_name_pretty))]]  //
  Source_Code_Span opening_tag_name;
  Source_Code_Span closing_tag_name;
  String8_View opening_tag_name_pretty;
};

struct Diag_Missing_Array_Close {
  [[qljs::diag("E0157", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing end of array; expected ']'",
                  ARG(expected_right_square))]]              //
  [[qljs::message("array started here", ARG(left_square))]]  //
  Source_Code_Span left_square;
  Source_Code_Span expected_right_square;
};

struct Diag_Missing_Arrow_Operator_In_Arrow_Function {
  [[qljs::diag("E0176", Diagnostic_Severity::error)]]                         //
  [[qljs::message("missing arrow operator for arrow function", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Arrow_Function_Parameter_List {
  [[qljs::diag("E0105", Diagnostic_Severity::error)]]                     //
  [[qljs::message("missing parameters for arrow function", ARG(arrow))]]  //
  Source_Code_Span arrow;
};

struct Diag_Missing_Body_For_Catch_Clause {
  [[qljs::diag("E0119", Diagnostic_Severity::error)]]                   //
  [[qljs::message("missing body for catch clause", ARG(catch_token))]]  //
  Source_Code_Span catch_token;
};

struct Diag_Missing_Body_For_Class {
  [[qljs::diag("E0111", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing body for class",
                  ARG(class_keyword_and_name_and_heritage))]]  //
  Source_Code_Span class_keyword_and_name_and_heritage;
};

struct Diag_Missing_Body_For_Do_While_Statement {
  [[qljs::diag("E0101", Diagnostic_Severity::error)]]                 //
  [[qljs::message("missing body for do-while loop", ARG(do_token))]]  //
  Source_Code_Span do_token;
};

struct Diag_Missing_Body_For_Finally_Clause {
  [[qljs::diag("E0121", Diagnostic_Severity::error)]]                       //
  [[qljs::message("missing body for finally clause", ARG(finally_token))]]  //
  Source_Code_Span finally_token;
};

struct Diag_Missing_Body_For_For_Statement {
  [[qljs::diag("E0094", Diagnostic_Severity::error)]]                    //
  [[qljs::message("missing body for 'for' loop", ARG(for_and_header))]]  //
  Source_Code_Span for_and_header;
};

struct Diag_Missing_Body_For_If_Statement {
  [[qljs::diag("E0064", Diagnostic_Severity::error)]]                       //
  [[qljs::message("missing body for 'if' statement", ARG(expected_body))]]  //
  Source_Code_Span expected_body;
};

struct Diag_Missing_Body_For_Switch_Statement {
  [[qljs::diag("E0106", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing body for 'switch' statement",
                  ARG(switch_and_condition))]]  //
  Source_Code_Span switch_and_condition;
};

struct Diag_Missing_Body_For_Try_Statement {
  [[qljs::diag("E0120", Diagnostic_Severity::error)]]                  //
  [[qljs::message("missing body for try statement", ARG(try_token))]]  //
  Source_Code_Span try_token;
};

struct Diag_Missing_Body_For_TypeScript_Interface {
  [[qljs::diag("E0245", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing body for TypeScript interface",
                  ARG(interface_keyword_and_name_and_heritage))]]  //
  Source_Code_Span interface_keyword_and_name_and_heritage;
};

struct Diag_Missing_Body_For_TypeScript_Namespace {
  [[qljs::diag("E0356", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing body for TypeScript namespace",
                  ARG(expected_body))]]  //
  Source_Code_Span expected_body;
};

struct Diag_Missing_Body_For_While_Statement {
  [[qljs::diag("E0104", Diagnostic_Severity::error)]]                         //
  [[qljs::message("missing body for while loop", ARG(while_and_condition))]]  //
  Source_Code_Span while_and_condition;
};

struct Diag_Missing_Catch_Or_Finally_For_Try_Statement {
  [[qljs::diag("E0122", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing catch or finally clause for try statement",
                  ARG(expected_catch_or_finally))]]               //
  [[qljs::message("try statement starts here", ARG(try_token))]]  //
  Source_Code_Span expected_catch_or_finally;
  Source_Code_Span try_token;
};

struct Diag_Missing_Catch_Variable_Between_Parentheses {
  [[qljs::diag("E0130", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing catch variable name between parentheses",
                  ARG(left_paren_to_right_paren))]]  //
  Source_Code_Span left_paren_to_right_paren;
  Source_Code_Span left_paren;
  Source_Code_Span right_paren;
};

struct Diag_Missing_Comma_Between_Object_Literal_Entries {
  [[qljs::diag("E0025", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing comma between object literal entries",
                  ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Comma_Between_Generic_Parameters {
  [[qljs::diag("E0265", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing comma between generic parameters",
                  ARG(expected_comma))]]  //
  Source_Code_Span expected_comma;
};

struct Diag_Missing_Comma_Between_Variable_Declarations {
  [[qljs::diag("E0132", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing ',' between variable declarations",
                  ARG(expected_comma))]]  //
  Source_Code_Span expected_comma;
};

struct Diag_Missing_Colon_In_Conditional_Expression {
  [[qljs::diag("E0146", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing ':' in conditional expression",
                  ARG(expected_colon))]]                                    //
  [[qljs::message("'?' creates a conditional expression", ARG(question))]]  //
  Source_Code_Span expected_colon;
  Source_Code_Span question;
};

struct Diag_Missing_Condition_For_If_Statement {
  [[qljs::diag("E0138", Diagnostic_Severity::error)]]                       //
  [[qljs::message("missing condition for if statement", ARG(if_keyword))]]  //
  Source_Code_Span if_keyword;
};

struct Diag_Missing_Condition_For_While_Statement {
  [[qljs::diag("E0139", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing condition for while statement",
                  ARG(while_keyword))]]  //
  Source_Code_Span while_keyword;
};

struct Diag_Missing_Condition_For_Switch_Statement {
  [[qljs::diag("E0137", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing condition for switch statement",
                  ARG(switch_keyword))]]  //
  Source_Code_Span switch_keyword;
};

struct Diag_Missing_Dots_For_Attribute_Spread {
  [[qljs::diag("E0186", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing '...' in JSX attribute spread",
                  ARG(expected_dots))]]  //
  Source_Code_Span expected_dots;
};

struct Diag_Missing_Equal_After_Variable {
  [[qljs::diag("E0202", Diagnostic_Severity::error)]]                   //
  [[qljs::message("missing '=' after variable", ARG(expected_equal))]]  //
  Source_Code_Span expected_equal;
};

struct Diag_Missing_Export_For_Function_With_Overload_Signature {
  [[qljs::diag("E0439", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing 'export' keyword for function",
                  ARG(expected_export))]]  //
  [[qljs::message(
      "'export' must be on either all function signatures or none of them",
      ARG(existing_export))]]  //
  Source_Code_Span expected_export;
  Source_Code_Span existing_export;
};

struct Diag_Missing_Expression_Between_Parentheses {
  [[qljs::diag("E0078", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing expression between parentheses",
                  ARG(left_paren_to_right_paren))]]  //
  Source_Code_Span left_paren_to_right_paren;
  Source_Code_Span left_paren;
  Source_Code_Span right_paren;
};

struct Diag_Missing_For_Loop_Header {
  [[qljs::diag("E0125", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing header and body for 'for' loop",
                  ARG(for_token))]]  //
  Source_Code_Span for_token;
};

struct Diag_Missing_For_Loop_Rhs_Or_Components_After_Expression {
  [[qljs::diag("E0097", Diagnostic_Severity::error)]]  //
  [[qljs::message("for loop needs an iterable, or condition and update clauses",
                  ARG(header))]]  //
  [[qljs::message("use 'while' instead to loop until a condition is false",
                  ARG(for_token))]]  //
  Source_Code_Span header;
  Source_Code_Span for_token;
};

struct Diag_Missing_For_Loop_Rhs_Or_Components_After_Declaration {
  [[qljs::diag("E0098", Diagnostic_Severity::error)]]  //
  [[qljs::message("for loop needs an iterable, or condition and update clauses",
                  ARG(header))]]  //
  Source_Code_Span header;
  Source_Code_Span for_token;
};

struct Diag_Missing_Function_Parameter_List {
  [[qljs::diag("E0073", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing function parameter list",
                  ARG(expected_parameter_list))]]  //
  Source_Code_Span expected_parameter_list;
};

struct Diag_Missing_Function_Body {
  [[qljs::diag("E0172", Diagnostic_Severity::error)]]                 //
  [[qljs::message("missing body for function", ARG(expected_body))]]  //
  Source_Code_Span expected_body;
};

struct Diag_Missing_Header_Of_For_Loop {
  [[qljs::diag("E0096", Diagnostic_Severity::error)]]       //
  [[qljs::message("missing for loop header", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Initializer_In_Const_Declaration {
  [[qljs::diag("E0205", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing initializer in const declaration",
                  ARG(variable_name))]]  //
  Source_Code_Span variable_name;
};

struct Diag_Missing_Key_For_Object_Entry {
  [[qljs::diag("E0154", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected expression; missing key for object entry",
                  ARG(expression))]]  //
  Source_Code_Span expression;
};

struct Diag_Missing_Class_Member_After_Decorator {
  [[qljs::diag("E0409", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing class method or field after decorator",
                  ARG(expected_member))]]                        //
  [[qljs::message("decorator starts here", ARG(decorator_at))]]  //
  Source_Code_Span expected_member;
  Source_Code_Span decorator_at;
};

struct Diag_Missing_Class_Method_Name {
  [[qljs::diag("E0229", Diagnostic_Severity::error)]]                     //
  [[qljs::message("missing name for class method", ARG(expected_name))]]  //
  Source_Code_Span expected_name;
};

struct Diag_Missing_Expression_After_Angle_Type_Assertion {
  [[qljs::diag("E0430", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing expression after type assertion",
                  ARG(expected_expression))]]  //
  Source_Code_Span expected_expression;
};

struct Diag_Missing_Name_In_Function_Statement {
  [[qljs::diag("E0061", Diagnostic_Severity::error)]]                  //
  [[qljs::message("missing name in function statement", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Name_In_Class_Statement {
  [[qljs::diag("E0080", Diagnostic_Severity::error)]]             //
  [[qljs::message("missing name of class", ARG(class_keyword))]]  //
  Source_Code_Span class_keyword;
};

struct Diag_Missing_Name_Of_Exported_Class {
  [[qljs::diag("E0081", Diagnostic_Severity::error)]]                      //
  [[qljs::message("missing name of exported class", ARG(class_keyword))]]  //
  Source_Code_Span class_keyword;
};

struct Diag_Missing_Name_Of_Exported_Function {
  [[qljs::diag("E0079", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing name of exported function",
                  ARG(function_keyword))]]  //
  Source_Code_Span function_keyword;
};

struct Diag_Missing_Name_Or_Parentheses_For_Function {
  [[qljs::diag("E0062", Diagnostic_Severity::error)]]                        //
  [[qljs::message("missing name or parentheses for function", ARG(where))]]  //
  Source_Code_Span where;
  Source_Code_Span function;
};

struct Diag_Missing_New_In_Abstract_Constructor_Type {
  [[qljs::diag("E0447", Diagnostic_Severity::error)]]                        //
  [[qljs::message("missing 'new' in constructor type", ARG(expected_new))]]  //
  Source_Code_Span expected_new;
};

struct Diag_Missing_Operand_For_Operator {
  [[qljs::diag("E0026", Diagnostic_Severity::error)]]            //
  [[qljs::message("missing operand for operator", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Parameter_Name {
  [[qljs::diag("E0438", Diagnostic_Severity::error)]]                        //
  [[qljs::message("missing parameter name", ARG(expected_parameter_name))]]  //
  Source_Code_Span expected_parameter_name;
};

struct Diag_Missing_Separator_Between_Object_Type_Entries {
  [[qljs::diag("E0257", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing ',', ';', or newline between object type entries",
                  ARG(expected_separator))]]  //
  Source_Code_Span expected_separator;
};

struct Diag_Redundant_Delete_Statement_On_Variable {
  [[qljs::diag("E0086", Diagnostic_Severity::warning)]]  //
  [[qljs::message("redundant delete statement on variable",
                  ARG(delete_expression))]]  //
  Source_Code_Span delete_expression;
};

struct Diag_Missing_If_After_Else {
  [[qljs::diag("E0184", Diagnostic_Severity::error)]]               //
  [[qljs::message("missing 'if' after 'else'", ARG(expected_if))]]  //
  Source_Code_Span expected_if;
};

struct Diag_Missing_Operator_Between_Expression_And_Arrow_Function {
  [[qljs::diag("E0063", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing operator between expression and arrow function",
                  ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Parentheses_Around_Exponent_With_Unary_Lhs {
  [[qljs::diag("E0195", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing parentheses around operand of '{0}'",
                  ARG(exponent_expression))]]  //
  [[qljs::message(
      "'{0}' operator cannot be used before '**' without parentheses",
      ARG(unary_operator))]]  //
  Source_Code_Span exponent_expression;
  Source_Code_Span unary_operator;
};

struct Diag_Missing_Parentheses_Around_Self_Invoked_Function {
  [[qljs::diag("E0211", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing parentheses around self-invoked function",
                  ARG(invocation))]]                          //
  [[qljs::message("function starts here", ARG(func_start))]]  //
  Source_Code_Span invocation;
  Source_Code_Span func_start;
};

struct Diag_Missing_Parentheses_Around_Unary_Lhs_Of_Exponent {
  [[qljs::diag("E0194", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing parentheses around left-hand side of '**'",
                  ARG(unary_expression))]]  //
  [[qljs::message(
      "'**' operator cannot be used after unary '{1}' without parentheses",
      ARG(exponent_operator), ARG(unary_expression))]]  //
  Source_Code_Span unary_expression;
  Source_Code_Span exponent_operator;
};

struct Diag_Missing_Property_Name_For_Dot_Operator {
  [[qljs::diag("E0142", Diagnostic_Severity::error)]]                      //
  [[qljs::message("missing property name after '.' operator", ARG(dot))]]  //
  Source_Code_Span dot;
};

struct Diag_Missing_Semicolon_After_Abstract_Method {
  [[qljs::diag("E0293", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing semicolon after abstract method",
                  ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Declare_Class_Method {
  [[qljs::diag("E0334", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing semicolon after 'declare class' method",
                  ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Statement {
  [[qljs::diag("E0027", Diagnostic_Severity::error)]]                 //
  [[qljs::message("missing semicolon after statement", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Semicolon_After_TypeScript_Method_Overload_Signature {
  [[qljs::diag("E0406", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing semicolon after method overload signature",
                  ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Field {
  [[qljs::diag("E0223", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing semicolon after field",
                  ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Index_Signature {
  [[qljs::diag("E0226", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing semicolon after index signature",
                  ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Interface_Method {
  [[qljs::diag("E0292", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing semicolon after interface method",
                  ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_Between_For_Loop_Condition_And_Update {
  [[qljs::diag("E0100", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "missing semicolon between condition and update parts of for loop",
      ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_Between_For_Loop_Init_And_Condition {
  [[qljs::diag("E0099", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "missing semicolon between init and condition parts of for loop",
      ARG(expected_semicolon))]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Token_After_Export {
  [[qljs::diag("E0113", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("incomplete export; expected 'export default ...' or "
                  "'export {{name}' or 'export * from ...' or 'export "
                  "class' or 'export function' or 'export let'", ARG(export_token))]]  //
  // clang-format on
  Source_Code_Span export_token;
};

struct Diag_Missing_Type_Between_Intersection_Or_Union {
  [[qljs::diag("E0258", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing type between '{1}' and '{0}'", ARG(right_operator),
                  ARG(left_operator))]]  //
  Source_Code_Span left_operator;
  Source_Code_Span right_operator;
};

// TODO(strager): Make more specific errors, like 'missing type after :',
// 'missing type after keyof', etc.
struct Diag_Missing_TypeScript_Type {
  [[qljs::diag("E0284", Diagnostic_Severity::error)]]               //
  [[qljs::message("missing TypeScript type", ARG(expected_type))]]  //
  Source_Code_Span expected_type;
};

struct Diag_Missing_Value_For_Object_Literal_Entry {
  [[qljs::diag("E0083", Diagnostic_Severity::error)]]               //
  [[qljs::message("missing value for object property", ARG(key))]]  //
  Source_Code_Span key;
};

struct Diag_Missing_Variable_Name_In_Declaration {
  [[qljs::diag("E0123", Diagnostic_Severity::error)]]           //
  [[qljs::message("missing variable name", ARG(equal_token))]]  //
  Source_Code_Span equal_token;
};

struct Diag_Missing_While_And_Condition_For_Do_While_Statement {
  [[qljs::diag("E0103", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing 'while (condition)' for do-while statement",
                  ARG(expected_while))]]                              //
  [[qljs::message("do-while statement starts here", ARG(do_token))]]  //
  Source_Code_Span do_token;
  Source_Code_Span expected_while;
};

struct Diag_Newline_Not_Allowed_Between_Async_And_Parameter_List {
  [[qljs::diag("E0163", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("newline is not allowed between 'async' and arrow "
                  "function parameter list", ARG(async))]]                //
  // clang-format on
  [[qljs::message("arrow is here", ARG(arrow))]]  //
  Source_Code_Span async;
  Source_Code_Span arrow;
};

struct Diag_Newline_Not_Allowed_Between_Async_And_Function_Keyword {
  [[qljs::diag("E0317", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed between 'async' and 'function'",
                  ARG(async_keyword))]]                           //
  [[qljs::message("'function' is here", ARG(function_keyword))]]  //
  Source_Code_Span async_keyword;
  Source_Code_Span function_keyword;
};

struct Diag_Newline_Not_Allowed_Between_Modifier_And_Method_Name {
  [[qljs::diag("E0399", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed between '{0}' and the method name",
                  ARG(modifier))]]  //
  Source_Code_Span modifier;
};

struct Diag_Newline_Not_Allowed_After_Abstract_Keyword {
  [[qljs::diag("E0300", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed after 'abstract'",
                  ARG(abstract_keyword))]]  //
  Source_Code_Span abstract_keyword;
};

struct Diag_Newline_Not_Allowed_After_Asserts_In_Assertion_Signature {
  [[qljs::diag("E0382", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed after 'asserts'",
                  ARG(asserts_keyword))]]  //
  Source_Code_Span asserts_keyword;
};

struct Diag_Newline_Not_Allowed_After_Export_Declare {
  [[qljs::diag("E0453", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed after 'export declare'",
                  ARG(declare_keyword), ARG(export_keyword))]]  //
  Source_Code_Span declare_keyword;
  Source_Code_Span export_keyword;
};

struct Diag_Newline_Not_Allowed_After_Interface_Keyword {
  [[qljs::diag("E0275", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed after 'interface'",
                  ARG(interface_keyword))]]  //
  Source_Code_Span interface_keyword;
};

struct Diag_Newline_Not_Allowed_After_Namespace_Keyword {
  [[qljs::diag("E0276", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed after '{0}'",
                  ARG(namespace_keyword))]]  //
  Source_Code_Span namespace_keyword;
};

struct Diag_Newline_Not_Allowed_After_Type_Keyword {
  [[qljs::diag("E0277", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed after 'type'",
                  ARG(type_keyword))]]  //
  Source_Code_Span type_keyword;
};

struct Diag_Newline_Not_Allowed_Before_Assignment_Assertion_Operator {
  [[qljs::diag("E0241", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed between field name and '!'",
                  ARG(bang))]]                               //
  [[qljs::message("field declared here", ARG(field_name))]]  //
  Source_Code_Span bang;
  Source_Code_Span field_name;
};

struct Diag_Newline_Not_Allowed_Before_Definite_Assignment_Assertion {
  [[qljs::diag("E0446", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed between variable name and '!'",
                  ARG(definite_assignment_assertion))]]  //
  Source_Code_Span definite_assignment_assertion;
};

struct Diag_Newline_Not_Allowed_Before_Extends_In_Type {
  [[qljs::diag("E0448", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed before 'extends'",
                  ARG(extends_keyword))]]  //
  Source_Code_Span extends_keyword;
};

struct Diag_Newline_Not_Allowed_Before_Generic_Arguments_In_Type {
  [[qljs::diag("E0431", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed before '<'",
                  ARG(less))]]  //
  Source_Code_Span less;
};

struct Diag_Newline_Not_Allowed_After_In_Out_Const_Modifiers {
  [[qljs::diag("E0440", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "newline is not allowed after '{0}' modifier in generic parameter",
      ARG(modifier))]]  //
  Source_Code_Span modifier;
};

struct Diag_Newline_Not_Allowed_Before_Is_In_Assertion_Signature {
  [[qljs::diag("E0454", Diagnostic_Severity::error)]]  //
  [[qljs::message("newline is not allowed before 'is'",
                  ARG(is_keyword))]]  //
  Source_Code_Span is_keyword;
};

struct Diag_Number_Literal_Contains_Consecutive_Underscores {
  [[qljs::diag("E0028", Diagnostic_Severity::error)]]  //
  [[qljs::message("number literal contains consecutive underscores",
                  ARG(underscores))]]  //
  Source_Code_Span underscores;
};

struct Diag_Number_Literal_Contains_Trailing_Underscores {
  [[qljs::diag("E0029", Diagnostic_Severity::error)]]  //
  [[qljs::message("number literal contains trailing underscore(s)",
                  ARG(underscores))]]  //
  Source_Code_Span underscores;
};

struct Diag_Octal_Literal_May_Not_Have_Exponent {
  [[qljs::diag("E0030", Diagnostic_Severity::error)]]                        //
  [[qljs::message("octal literal may not have exponent", ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Octal_Literal_May_Not_Have_Decimal {
  [[qljs::diag("E0031", Diagnostic_Severity::error)]]                       //
  [[qljs::message("octal literal may not have decimal", ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Object_Literal_Default_In_Expression {
  [[qljs::diag("E0253", Diagnostic_Severity::error)]]                         //
  [[qljs::message("use ':' instead of '=' in object literals", ARG(equal))]]  //
  Source_Code_Span equal;
};

struct Diag_Optional_Arrow_Parameter_Requires_Parentheses {
  [[qljs::diag("E0311", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing parentheses around parameter",
                  ARG(parameter_and_question))]]  //
  [[qljs::message("TypeScript optional parameter requires parentheses",
                  ARG(question))]]  //
  Source_Code_Span parameter_and_question;
  Source_Code_Span question;
};

struct Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses {
  [[qljs::diag("E0312", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing parentheses around parameter",
                  ARG(parameter_and_annotation))]]  //
  [[qljs::message(
      "TypeScript optional parameter with type annotation requires parentheses",
      ARG(question))]]  //
  Source_Code_Span parameter_and_annotation;
  Source_Code_Span question;
  Source_Code_Span type_colon;
};

struct Diag_Optional_Parameter_Cannot_Have_Initializer {
  [[qljs::diag("E0310", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "optional parameter cannot have both '?' and initializer; remove '?'",
      ARG(question))]]                                      //
  [[qljs::message("initializer starts here", ARG(equal))]]  //
  Source_Code_Span equal;
  Source_Code_Span question;
};

struct Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter {
  [[qljs::diag("E0379", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "optional parameter cannot be followed by a required parameter",
      ARG(optional_parameter))]]  //
  [[qljs::message(
      "this required parameter appears after the optional parameter",
      ARG(required_parameter))]]  //
  Source_Code_Span optional_parameter;
  Source_Code_Span required_parameter;
};

struct Diag_Override_Property_Not_Allowed_In_Interface {
  [[qljs::diag("E0449", Diagnostic_Severity::error)]]  //
  [[qljs::message("override properties are not allowed in interfaces",
                  ARG(override_keyword))]]  //
  Source_Code_Span override_keyword;
};

struct Diag_Integer_Literal_Will_Lose_Precision {
  [[qljs::diag("E0212", Diagnostic_Severity::warning)]]  //
  [[qljs::message("integer cannot be represented and will be rounded to '{1}'",
                  ARG(characters), ARG(rounded_val))]]  //
  Source_Code_Span characters;
  String8_View rounded_val;
};

struct Diag_Parameter_Decorator_In_Abstract_Method {
  [[qljs::diag("E0437", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter decorators are not allowed in abstract methods",
                  ARG(decorator_at))]]  //
  [[qljs::message("'abstract' here",
                  ARG(abstract_keyword))]]  //
  Source_Code_Span decorator_at;
  Source_Code_Span abstract_keyword;
};

struct Diag_Parameter_Decorator_In_Declare_Class {
  [[qljs::diag("E0436", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter decorators are not allowed in 'declare class'",
                  ARG(decorator_at))]]  //
  [[qljs::message("'declare' here",
                  ARG(declare_keyword))]]  //
  Source_Code_Span decorator_at;
  Source_Code_Span declare_keyword;
};

struct Diag_Parameter_Decorator_In_Non_Class_Method {
  [[qljs::diag("E0435", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter decorators are only allowed in class methods",
                  ARG(decorator_at))]]  //
  Source_Code_Span decorator_at;
};

struct Diag_Parameter_Decorator_Must_Preceed_Modifiers {
  [[qljs::diag("E0434", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter decorator must be before other modifiers",
                  ARG(decorator_at))]]  //
  [[qljs::message("move the parameter decorator before '{0}' here",
                  ARG(modifier))]]  //
  Source_Code_Span modifier;
  Source_Code_Span decorator_at;
};

struct Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals {
  [[qljs::diag("E0156", Diagnostic_Severity::error)]]  //
  [[qljs::message("private properties are not allowed in object literals",
                  ARG(private_identifier))]]  //
  Source_Code_Span private_identifier;
};

struct Diag_Readonly_Static_Field {
  [[qljs::diag("E0232", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'readonly static' is not allowed; write 'static readonly' instead",
      ARG(readonly_static))]]  //
  Source_Code_Span readonly_static;
};

struct Diag_Redeclaration_Of_Global_Variable {
  [[qljs::diag("E0033", Diagnostic_Severity::error)]]                        //
  [[qljs::message("redeclaration of global variable", ARG(redeclaration))]]  //
  Source_Code_Span redeclaration;
};

struct Diag_Redeclaration_Of_Variable {
  [[qljs::diag("E0034", Diagnostic_Severity::error)]]                      //
  [[qljs::message("redeclaration of variable: {0}", ARG(redeclaration))]]  //
  [[qljs::message("variable already declared here",
                  ARG(original_declaration))]]  //
  Source_Code_Span redeclaration;
  Source_Code_Span original_declaration;
};

struct Diag_Redundant_Await {
  [[qljs::diag("E0266", Diagnostic_Severity::warning)]]        //
  [[qljs::message("redundant 'await'", ARG(await_operator))]]  //
  Source_Code_Span await_operator;
};

struct Diag_Regexp_Literal_Flags_Cannot_Contain_Unicode_Escapes {
  [[qljs::diag("E0035", Diagnostic_Severity::error)]]  //
  [[qljs::message("RegExp literal flags cannot contain Unicode escapes",
                  ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Return_Statement_Returns_Nothing {
  [[qljs::diag("E0179", Diagnostic_Severity::warning)]]  //
  [[qljs::message("return statement returns nothing (undefined)",
                  ARG(return_keyword))]]  //
  Source_Code_Span return_keyword;
};

struct Diag_Spread_Parameter_Cannot_Be_This {
  [[qljs::diag("E0304", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot use '...' on 'this' parameter",
                  ARG(spread_operator))]]  //
  Source_Code_Span this_keyword;
  Source_Code_Span spread_operator;
};

struct Diag_Statement_Before_First_Switch_Case {
  [[qljs::diag("E0198", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("unexpected statement before first switch case, expected "
                  "'case' or 'default'", ARG(unexpected_statement))]]  //
  // clang-format on
  Source_Code_Span unexpected_statement;
};

struct Diag_Stray_Comma_In_Let_Statement {
  [[qljs::diag("E0036", Diagnostic_Severity::error)]]            //
  [[qljs::message("stray comma in let statement", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Stray_Comma_In_Parameter {
  [[qljs::diag("E0180", Diagnostic_Severity::error)]]                 //
  [[qljs::message("stray comma in function parameter", ARG(comma))]]  //
  Source_Code_Span comma;
};

struct Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module {
  [[qljs::diag("E0359", Diagnostic_Severity::error)]]  //
  [[qljs::message("string module name is only allowed with 'declare module'",
                  ARG(module_name))]]  //
  Source_Code_Span module_name;
};

struct Diag_String_Namespace_Name_Not_Allowed_In_Namespace {
  [[qljs::diag("E0361", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("TypeScript 'declare module' with string name is not "
                  "allowed in namespaces",
                  ARG(module_name))]]  //
  // clang-format on
  Source_Code_Span module_name;
};

struct Diag_This_Parameter_Must_Be_First {
  [[qljs::diag("E0303", Diagnostic_Severity::error)]]                         //
  [[qljs::message("'this' must be the first parameter", ARG(this_keyword))]]  //
  [[qljs::message("first parameter starts here",
                  ARG(first_parameter_begin))]]  //
  Source_Code_Span this_keyword;
  Source_Code_Span first_parameter_begin;
};

struct Diag_This_Parameter_Not_Allowed_In_Arrow_Functions {
  [[qljs::diag("E0301", Diagnostic_Severity::error)]]  //
  [[qljs::message("'this' parameters are not allowed in arrow functions",
                  ARG(this_keyword))]]  //
  Source_Code_Span this_keyword;
};

struct Diag_This_Parameter_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0305", Diagnostic_Severity::error)]]  //
  [[qljs::message("'this' parameters are not allowed in JavaScript",
                  ARG(this_keyword))]]  //
  Source_Code_Span this_keyword;
};

struct Diag_This_Parameter_Not_Allowed_When_Destructuring {
  [[qljs::diag("E0302", Diagnostic_Severity::error)]]  //
  [[qljs::message("'this' parameter not allowed when destructuring",
                  ARG(this_keyword))]]  //
  Source_Code_Span this_keyword;
};

struct Diag_TypeScript_Abstract_Class_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0244", Diagnostic_Severity::error)]]  //
  [[qljs::message("{0} classes are not allowed in JavaScript",
                  ARG(abstract_keyword))]]  //
  Source_Code_Span abstract_keyword;
};

struct Diag_TypeScript_Abstract_Static_Property {
  [[qljs::diag("E0398", Diagnostic_Severity::error)]]  //
  [[qljs::message("abstract properties cannot be static",
                  ARG(abstract_keyword))]]  //
  [[qljs::message("property declared static here",
                  ARG(static_keyword))]]  //
  Source_Code_Span abstract_keyword;
  Source_Code_Span static_keyword;
};

struct Diag_TypeScript_Angle_Type_Assertion_Not_Allowed_In_Tsx {
  [[qljs::diag("E0283", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript <Type> type assertions are not allowed in JSX mode",
      ARG(bracketed_type))]]  //
  [[qljs::message("write the type assertion with 'as' here instead",
                  ARG(expected_as))]]  //
  Source_Code_Span bracketed_type;
  Source_Code_Span expected_as;
};

struct Diag_TypeScript_Accessor_Cannot_Be_Optional {
  [[qljs::diag("E0396", Diagnostic_Severity::error)]]                        //
  [[qljs::message("accessors cannot be optional", ARG(optional_question))]]  //
  [[qljs::message("field was declared as an accessor here",
                  ARG(accessor_keyword))]]  //
  Source_Code_Span optional_question;
  Source_Code_Span accessor_keyword;
};

struct Diag_TypeScript_As_Const_With_Non_Literal_Typeable {
  [[qljs::diag("E0291", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("'as const' is only allowed on literals (array, object, "
                  "string, boolean) and enum members", ARG(expression))]]                        //
  // clang-format on
  [[qljs::message("'as const' located here", ARG(as_const))]]  //
  Source_Code_Span expression;
  Source_Code_Span as_const;
};

struct Diag_TypeScript_As_Type_Assertion_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0281", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript 'as' type assertions are not allowed in JavaScript",
      ARG(as_keyword))]]  //
  Source_Code_Span as_keyword;
};

struct Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation {
  [[qljs::diag("E0282", Diagnostic_Severity::error)]]  //
  [[qljs::message("use ':' instead of '{0}' to type a function parameter",
                  ARG(bad_keyword))]]  //
  Source_Code_Span bad_keyword;
};

struct Diag_TypeScript_Assertion_Signature_Only_Allowed_As_Return_Types {
  [[qljs::diag("E0336", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "assertion signatures are only allowed as function return types",
      ARG(asserts_keyword))]]  //
  Source_Code_Span asserts_keyword;
};

struct Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Declare_Class {
  [[qljs::diag("E0425", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "assignment-asserted fields are not allowed in 'declare class'",
      ARG(bang))]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces {
  [[qljs::diag("E0238", Diagnostic_Severity::error)]]  //
  [[qljs::message("assignment-asserted fields are not supported in interfaces",
                  ARG(bang))]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0239", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript assignment-asserted fields are not supported in JavaScript",
      ARG(bang))]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Field_Cannot_Have_Initializer {
  [[qljs::diag("E0290", Diagnostic_Severity::error)]]  //
  [[qljs::message("assignment-assertion fields cannot have default values",
                  ARG(equal))]]                                              //
  [[qljs::message("here is the assignment assertion operator", ARG(bang))]]  //
  Source_Code_Span equal;
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type {
  [[qljs::diag("E0236", Diagnostic_Severity::error)]]  //
  [[qljs::message("assignment-asserted field must have a type annotation",
                  ARG(bang))]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Method {
  [[qljs::diag("E0240", Diagnostic_Severity::error)]]              //
  [[qljs::message("'{0}' is not allowed on methods", ARG(bang))]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Catch_Type_Annotation_Must_Be_Any {
  [[qljs::diag("E0256", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "catch variable can only be typed as '*', 'any', or 'unknown'",
      ARG(type_expression))]]  //
  Source_Code_Span type_expression;
};

struct Diag_TypeScript_Class_Implements_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0247", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript 'implements' is not allowed in JavaScript",
                  ARG(implements_keyword))]]  //
  Source_Code_Span implements_keyword;
};

struct Diag_TypeScript_Delete_Cannot_Delete_Variables {
  [[qljs::diag("E0325", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot delete variables in TypeScript",
                  ARG(delete_expression))]]  //
  Source_Code_Span delete_expression;
};

struct Diag_TypeScript_Definite_Assignment_Assertion_In_Ambient_Context {
  [[qljs::diag("E0445", Diagnostic_Severity::error)]]  //
  [
      [qljs::message("'!' (definite assignment assertion) is not allowed on "
                     "'declare' variables",
                     ARG(definite_assignment_assertion))]]  //
  [[qljs::message("'declare' here",
                  ARG(declare_keyword))]]  //
  Source_Code_Span definite_assignment_assertion;
  Source_Code_Span declare_keyword;
};

struct Diag_TypeScript_Definite_Assignment_Assertion_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0444", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected '!' after variable name",
                  ARG(definite_assignment_assertion))]]  //
  Source_Code_Span definite_assignment_assertion;
};

struct Diag_TypeScript_Definite_Assignment_Assertion_On_Const {
  [[qljs::diag("E0441", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "const variables cannot have '!' (definite assignment assertion)",
      ARG(definite_assignment_assertion))]]  //
  Source_Code_Span definite_assignment_assertion;
  Source_Code_Span const_keyword;
};

struct Diag_TypeScript_Definite_Assignment_Assertion_With_Initializer {
  [[qljs::diag("E0442", Diagnostic_Severity::error)]]  //
  [
      [qljs::message("'!' (definite assignment assertion) cannot be used with "
                     "an initial value",
                     ARG(definite_assignment_assertion))]]  //
  [[qljs::message("initial value was given here",
                  ARG(equal))]]  //
  Source_Code_Span definite_assignment_assertion;
  Source_Code_Span equal;
};

struct Diag_TypeScript_Definite_Assignment_Assertion_Without_Type_Annotation {
  [[qljs::diag("E0443", Diagnostic_Severity::error)]]  //
  [
      [qljs::message("type annotation is required when using '!' (definite "
                     "assignment assertion)",
                     ARG(definite_assignment_assertion))]]  //
  Source_Code_Span definite_assignment_assertion;
};

struct Diag_TypeScript_Enum_Auto_Member_Needs_Initializer_After_Computed {
  [[qljs::diag("E0252", Diagnostic_Severity::error)]]                        //
  [[qljs::message("enum member needs initializer", ARG(auto_member_name))]]  //
  [[qljs::message("computed value disables enum autoincrement",
                  ARG(computed_expression))]]  //
  Source_Code_Span auto_member_name;
  Source_Code_Span computed_expression;
};

struct Diag_TypeScript_Enum_Computed_Name_Must_Be_Simple {
  [[qljs::diag("E0249", Diagnostic_Severity::error)]]  //
  [[qljs::message("computed enum member name must be a simple string",
                  ARG(expression))]]  //
  Source_Code_Span expression;
};

struct Diag_TypeScript_Enum_Is_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0127", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript's 'enum' feature is not allowed in JavaScript",
                  ARG(enum_keyword))]]  //
  Source_Code_Span enum_keyword;
};

struct Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number {
  [[qljs::diag("E0250", Diagnostic_Severity::error)]]                   //
  [[qljs::message("enum member name cannot be numeric", ARG(number))]]  //
  Source_Code_Span number;
};

struct Diag_TypeScript_Enum_Value_Must_Be_Constant {
  [[qljs::diag("E0251", Diagnostic_Severity::error)]]  //
  [[qljs::message("{1:headlinese} value must be a compile-time constant",
                  ARG(expression), ARG(declared_enum_kind))]]  //
  Source_Code_Span expression;
  Enum_Kind declared_enum_kind;
};

struct
    Diag_TypeScript_Export_As_Namespace_Is_Not_Allowed_In_Namespace_Or_Module {
  [[qljs::diag("E0424", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'export as namespace' is not allowed in a namespace or module",
      ARG(export_keyword))]]  //
  [[qljs::message("containing namespace or module declared here",
                  ARG(namespace_or_module_keyword))]]  //
  Source_Code_Span export_keyword;
  Source_Code_Span namespace_or_module_keyword;
};

struct
    Diag_TypeScript_Export_As_Namespace_Is_Only_Allowed_In_TypeScript_Definition_File {
  [[qljs::diag("E0423", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'export as namespace' is only allowed in TypeScript .d.ts files",
      ARG(export_keyword))]]  //
  Source_Code_Span export_keyword;
};

struct Diag_TypeScript_Export_Equal_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0370", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("'export =' is not allowed; write 'export default' or "
                  "'module.exports =' (CommonJS) instead", ARG(equal))]]                                 //
  // clang-format on
  [[qljs::message("'export' keyword here", ARG(export_keyword))]]  //
  Source_Code_Span equal;
  Source_Code_Span export_keyword;
};

struct Diag_TypeScript_Implements_Must_Be_After_Extends {
  [[qljs::diag("E0246", Diagnostic_Severity::error)]]  //
  [[qljs::message("'extends' must be before 'implements'",
                  ARG(extends_keyword))]]  //
  [[qljs::message("move the 'extends' clause before 'implements' here",
                  ARG(implements_keyword))]]  //
  Source_Code_Span implements_keyword;
  Source_Code_Span extends_keyword;
};

struct Diag_TypeScript_Import_Alias_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0274", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript import aliases are not allowed in JavaScript",
                  ARG(equal))]]  //
  [[qljs::message("write 'const' instead of '{0}' here",
                  ARG(import_keyword))]]  //
  Source_Code_Span import_keyword;
  Source_Code_Span equal;
};

struct Diag_TypeScript_Index_Signature_Cannot_Be_Method {
  [[qljs::diag("E0227", Diagnostic_Severity::error)]]  //
  [[qljs::message("index signature must be a field, not a method",
                  ARG(left_paren))]]  //
  Source_Code_Span left_paren;
};

struct Diag_TypeScript_Index_Signature_Needs_Type {
  [[qljs::diag("E0225", Diagnostic_Severity::error)]]  //
  [[qljs::message("index signatures require a value type",
                  ARG(expected_type))]]  //
  Source_Code_Span expected_type;
};

struct Diag_TypeScript_Infer_Outside_Conditional_Type {
  [[qljs::diag("E0367", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'infer' is only allowed between 'extends' and '?' in conditional types",
      ARG(infer_keyword))]]  //
  Source_Code_Span infer_keyword;
};

struct Diag_TypeScript_Infer_Requires_Parentheses {
  [[qljs::diag("E0366", Diagnostic_Severity::error)]]  //
  [[qljs::message("parentheses are required around 'infer {1}'",
                  ARG(infer_and_type), ARG(type))]]  //
  Source_Code_Span infer_and_type;
  Source_Code_Span type;
};

struct Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name {
  [[qljs::diag("E0316", Diagnostic_Severity::error)]]  //
  [[qljs::message("function overload signature must be named '{1}'",
                  ARG(overload_name), ARG(function_name))]]  //
  [[qljs::message("overloaded function '{0}' declared here",
                  ARG(function_name))]]  //
  Source_Code_Span overload_name;
  Source_Code_Span function_name;
};

struct
    Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star {
  [[qljs::diag("E0318", Diagnostic_Severity::error)]]  //
  [[qljs::message("function overload signature cannot have generator '*'",
                  ARG(generator_star))]]  //
  Source_Code_Span generator_star;
};

struct Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode {
  [[qljs::diag("E0285", Diagnostic_Severity::error)]]  //
  [[qljs::message("generic arrow function needs ',' here in TSX",
                  ARG(expected_comma))]]  //
  Source_Code_Span generic_parameters_less;
  Source_Code_Span expected_comma;
  Source_Code_Span arrow;
};

struct Diag_TypeScript_Generic_Parameter_List_Is_Empty {
  [[qljs::diag("E0264", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected at least one parameter in generic parameter list",
                  ARG(expected_parameter))]]  //
  Source_Code_Span expected_parameter;
};

struct Diag_TypeScript_Generics_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0233", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript generics are not allowed in JavaScript code",
                  ARG(opening_less))]]  //
  Source_Code_Span opening_less;
};

struct Diag_TypeScript_Global_Block_Must_Be_Declare {
  [[qljs::diag("E0422", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript global declaration block must use 'declare'",
                  ARG(global_keyword))]]  //
  Source_Code_Span global_keyword;
  Source_Code_Span expected_declare_keyword;
};

struct Diag_TypeScript_Global_Block_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0420", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript 'declare global' is not allowed in JavaScript",
                  ARG(global_keyword))]]  //
  Source_Code_Span global_keyword;
};

struct Diag_TypeScript_Global_Block_Not_Allowed_In_Namespace {
  [[qljs::diag("E0421", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript 'declare global' is not allowed in namespaces",
                  ARG(global_keyword))]]  //
  [[qljs::message("inside namespace here",
                  ARG(namespace_keyword))]]  //
  Source_Code_Span global_keyword;
  Source_Code_Span namespace_keyword;
};

struct Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0278", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript type exports are not allowed in JavaScript",
                  ARG(type_keyword))]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Type_Predicate_Only_Allowed_As_Return_Type {
  [[qljs::diag("E0426", Diagnostic_Severity::error)]]  //
  [[qljs::message("type predicates are only allowed as function return types",
                  ARG(is_keyword))]]  //
  Source_Code_Span is_keyword;
};

struct Diag_TypeScript_Inline_Type_Export_Not_Allowed_In_Type_Only_Export {
  [[qljs::diag("E0280", Diagnostic_Severity::error)]]  //
  [[qljs::message("'type' cannot be used twice in export",
                  ARG(inline_type_keyword))]]                      //
  [[qljs::message("remove this 'type'", ARG(type_only_keyword))]]  //
  Source_Code_Span inline_type_keyword;
  Source_Code_Span type_only_keyword;
};

struct Diag_TypeScript_Inline_Type_Import_Not_Allowed_In_Type_Only_Import {
  [[qljs::diag("E0272", Diagnostic_Severity::error)]]  //
  [[qljs::message("'type' cannot be used twice in import",
                  ARG(inline_type_keyword))]]                      //
  [[qljs::message("remove this 'type'", ARG(type_only_keyword))]]  //
  Source_Code_Span inline_type_keyword;
  Source_Code_Span type_only_keyword;
};

struct Diag_TypeScript_Interfaces_Cannot_Contain_Static_Blocks {
  [[qljs::diag("E0243", Diagnostic_Severity::error)]]  //
  [[qljs::message("interfaces cannot contain static blocks",
                  ARG(static_token))]]  //
  Source_Code_Span static_token;
};

struct Diag_TypeScript_Generic_Less_Less_Not_Split {
  [[qljs::diag("E0429", Diagnostic_Severity::error)]]  //
  [[qljs::message("space is required between '<' and '<' inside {1:headlinese}",
                  ARG(expected_space), ARG(context))]]  //
  Source_Code_Span expected_space;
  Statement_Kind context;
};

struct Diag_TypeScript_Declare_Class_Cannot_Contain_Static_Block_Statement {
  [[qljs::diag("E0332", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare class' cannot contain static block",
                  ARG(static_token))]]  //
  Source_Code_Span static_token;
};

struct Diag_TypeScript_Declare_Field_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0415", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript 'declare' fields are now allowed in JavaScript",
                  ARG(declare_keyword))]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_TypeScript_Declare_Field_Cannot_Use_Private_Identifier {
  [[qljs::diag("E0416", Diagnostic_Severity::error)]]  //
  [
      [qljs::message("private identifiers are not allowed for 'declare' "
                     "fields; use 'private' instead",
                     ARG(private_identifier_hash))]]         //
  [[qljs::message("'declare' here", ARG(declare_keyword))]]  //
  Source_Code_Span private_identifier_hash;
  Source_Code_Span declare_keyword;
};

struct Diag_TypeScript_Declare_Field_Cannot_Be_Assignment_Asserted {
  [[qljs::diag("E0418", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "assignment assertion is not allowed on fields be marked 'declare'",
      ARG(bang))]]                                           //
  [[qljs::message("'declare' here", ARG(declare_keyword))]]  //
  Source_Code_Span bang;
  Source_Code_Span declare_keyword;
};

struct Diag_TypeScript_Declare_Method {
  [[qljs::diag("E0417", Diagnostic_Severity::error)]]  //
  [[qljs::message("methods cannot be marked 'declare'",
                  ARG(declare_keyword))]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_TypeScript_Interfaces_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0213", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript's 'interface' feature is not allowed in JavaScript code",
      ARG(interface_keyword))]]  //
  Source_Code_Span interface_keyword;
};

struct Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type {
  [[qljs::diag("E0319", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing name for element in named tuple type",
                  ARG(expected_name_and_colon))]]  //
  // clang-format off
  [[qljs::message("this tuple type is a named tuple type because at least "
                  "one element has a name", ARG(existing_name))]]  //
  // clang-format on
  Source_Code_Span expected_name_and_colon;
  Source_Code_Span existing_name;
};

struct Diag_TypeScript_Missing_Name_In_Named_Tuple_Type {
  [[qljs::diag("E0320", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing name for element in named tuple type",
                  ARG(colon))]]  //
  Source_Code_Span colon;
};

struct Diag_TypeScript_Named_Tuple_Element_Question_After_Name_And_Type {
  [[qljs::diag("E0322", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'?' belongs only after the tuple element name, not also after the type",
      ARG(type_question))]]  //
  Source_Code_Span type_question;
  Source_Code_Span name_question;
};

struct Diag_TypeScript_Named_Tuple_Element_Question_After_Type {
  [[qljs::diag("E0314", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'?' belongs after the tuple element name, not after the type",
      ARG(question))]]                                        //
  [[qljs::message("'?' goes here", ARG(expected_question))]]  //
  Source_Code_Span question;
  Source_Code_Span expected_question;
};

struct Diag_TypeScript_Named_Tuple_Element_Spread_Before_Name_And_Type {
  [[qljs::diag("E0329", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("'...' belongs only before the tuple element name, not "
                  "also before the type", ARG(type_spread))]]  //
  // clang-format on
  Source_Code_Span type_spread;
  Source_Code_Span name_spread;
};

struct Diag_TypeScript_Named_Tuple_Element_Spread_Before_Type {
  [[qljs::diag("E0328", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'...' belongs before the tuple element name, not before the type",
      ARG(spread))]]                                          //
  [[qljs::message("'...' goes here", ARG(expected_spread))]]  //
  Source_Code_Span spread;
  Source_Code_Span expected_spread;
};

struct Diag_TypeScript_Namespace_Cannot_Export_Default {
  [[qljs::diag("E0363", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot 'export default' from inside a namespace",
                  ARG(default_keyword))]]                             //
  [[qljs::message("namespace starts here", ARG(namespace_keyword))]]  //
  Source_Code_Span default_keyword;
  Source_Code_Span namespace_keyword;
};

struct Diag_TypeScript_Namespaces_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0273", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript namespaces are not allowed in JavaScript",
                  ARG(namespace_keyword))]]  //
  Source_Code_Span namespace_keyword;
};

struct Diag_TypeScript_Non_Null_Assertion_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0261", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript non-null assertions are not allowed in JavaScript",
      ARG(bang))]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Optional_Parameters_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0308", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript optional parameters are not allowed in JavaScript",
      ARG(question))]]  //
  Source_Code_Span question;
};

struct Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0228", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript optional properties are not allowed in JavaScript code",
      ARG(question))]]  //
  Source_Code_Span question;
};

struct Diag_TypeScript_Optional_Tuple_Element_Cannot_Follow_Spread_Element {
  [[qljs::diag("E0323", Diagnostic_Severity::error)]]  //
  [[qljs::message("optional tuple elements cannot come after spread elements",
                  ARG(optional_question))]]                                //
  [[qljs::message("prior spread element is here", ARG(previous_spread))]]  //
  Source_Code_Span optional_question;
  Source_Code_Span previous_spread;
};

struct Diag_TypeScript_Overload_Signature_Access_Specifier_Mismatch {
  [[qljs::diag("E0405", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "overload signature must have the correct access specifier ('{1}')",
      ARG(signature_access_specifier), ARG(method_access_specifier))]]  //
  [[qljs::message("overloaded method is marked '{0}'",
                  ARG(method_access_specifier))]]  //
  Source_Code_Span method_access_specifier;
  Source_Code_Span signature_access_specifier;
};

struct Diag_TypeScript_Parameter_Decorator_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0433", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript parameter decorators are not allowed in JavaScript",
      ARG(at))]]  //
  Source_Code_Span at;
};

struct Diag_TypeScript_Parameter_Property_Cannot_Be_Destructured {
  [[qljs::diag("E0372", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter properties cannot be destructured",
                  ARG(destructure_token))]]  //
  [[qljs::message("property declared using '{0}' here",
                  ARG(property_keyword))]]  //
  Source_Code_Span destructure_token;
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Parameter_Property_Cannot_Be_Rest {
  [[qljs::diag("E0377", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter properties cannot be a rest parameter",
                  ARG(spread))]]  //
  [[qljs::message("property declared using '{0}' here",
                  ARG(property_keyword))]]  //
  Source_Code_Span spread;
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Parameter_Property_Not_Allowed_In_Declare_Class {
  [[qljs::diag("E0375", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter properties are not allowed in 'declare class'",
                  ARG(property_keyword))]]                             //
  [[qljs::message("'declare' specified here", ARG(declare_keyword))]]  //
  Source_Code_Span property_keyword;
  Source_Code_Span declare_keyword;
};

struct Diag_TypeScript_Parameter_Property_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0371", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript parameter properties are not allowed in JavaScript",
      ARG(property_keyword))]]  //
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Parameter_Property_Only_Allowed_In_Class_Constructor {
  [[qljs::diag("E0378", Diagnostic_Severity::error)]]  //
  [[qljs::message("parameter properties are only allowed in class constructors",
                  ARG(property_keyword))]]  //
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Private_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0222", Diagnostic_Severity::error)]]  //
  [[qljs::message("'private' is not allowed in JavaScript",
                  ARG(specifier))]]  //
  Source_Code_Span specifier;
};

struct Diag_TypeScript_Protected_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0234", Diagnostic_Severity::error)]]  //
  [[qljs::message("'protected' is not allowed in JavaScript",
                  ARG(specifier))]]  //
  Source_Code_Span specifier;
};

struct Diag_TypeScript_Public_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0289", Diagnostic_Severity::error)]]                         //
  [[qljs::message("'public' is not allowed in JavaScript", ARG(specifier))]]  //
  Source_Code_Span specifier;
};

struct Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0230", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript's 'readonly' feature is not allowed in JavaScript code",
      ARG(readonly_keyword))]]  //
  Source_Code_Span readonly_keyword;
};

struct Diag_TypeScript_Readonly_Method {
  [[qljs::diag("E0231", Diagnostic_Severity::error)]]                     //
  [[qljs::message("methods cannot be readonly", ARG(readonly_keyword))]]  //
  Source_Code_Span readonly_keyword;
};

struct Diag_TypeScript_Readonly_In_Type_Needs_Array_Or_Tuple_Type {
  [[qljs::diag("E0313", Diagnostic_Severity::error)]]  //
  [[qljs::message("'readonly' only works with array types and tuple types",
                  ARG(readonly_keyword))]]  //
  Source_Code_Span readonly_keyword;
};

struct Diag_TypeScript_Required_Tuple_Element_After_Optional_Element {
  [[qljs::diag("E0321", Diagnostic_Severity::error)]]  //
  [[qljs::message("expected '?' to mark tuple element as optional",
                  ARG(expected_question))]]  //
  [[qljs::message(
      "only optional tuple elements can follow this optional tuple element",
      ARG(previous_optional_question))]]  //
  Source_Code_Span expected_question;
  Source_Code_Span previous_optional_question;
};

struct Diag_TypeScript_Requires_Space_Between_Greater_And_Equal {
  [[qljs::diag("E0365", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript requires whitespace between '>' and '=' here",
                  ARG(greater_equal))]]  //
  Source_Code_Span greater_equal;
};

struct Diag_TypeScript_Satisfies_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0364", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript 'satisfies' operator is not allowed in JavaScript",
      ARG(satisfies_keyword))]]  //
  Source_Code_Span satisfies_keyword;
};

struct Diag_TypeScript_Type_Annotation_In_Expression {
  [[qljs::diag("E0254", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected ':' in expression; did you mean 'as'?",
                  ARG(type_colon))]]  //
  Source_Code_Span type_colon;
};

struct Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0224", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript type annotations are not allowed in JavaScript code",
      ARG(type_colon))]]  //
  Source_Code_Span type_colon;
};

struct Diag_TypeScript_Type_Alias_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0267", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript types are not allowed in JavaScript",
                  ARG(type_keyword))]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Type_Only_Import_Cannot_Import_Default_And_Named {
  [[qljs::diag("E0268", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript type imports cannot import both default and named exports",
      ARG(type_keyword))]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Type_Import_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0270", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript type imports are not allowed in JavaScript",
                  ARG(type_keyword))]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Spread_Element_Cannot_Be_Optional {
  [[qljs::diag("E0324", Diagnostic_Severity::error)]]  //
  [[qljs::message("spread tuple elements cannot be optional",
                  ARG(optional_question))]]             //
  [[qljs::message("spread starts here", ARG(spread))]]  //
  Source_Code_Span optional_question;
  Source_Code_Span spread;
};

struct Diag_TypeScript_Style_Const_Field {
  [[qljs::diag("E0165", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("const fields within classes are only allowed in "
                  "TypeScript, not JavaScript", ARG(const_token))]]  //
  // clang-format on
  Source_Code_Span const_token;
};

struct Diag_TypeScript_Variance_Keywords_In_Wrong_Order {
  [[qljs::diag("E0368", Diagnostic_Severity::error)]]  //
  [[qljs::message("'out in' is not allowed; write 'in out' instead",
                  ARG(in_keyword))]]  //
  Source_Code_Span in_keyword;
  Source_Code_Span out_keyword;
};

struct Diag_TypeScript_Variance_Keyword_Repeated {
  [[qljs::diag("E0432", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{0}' variance specifier cannot be listed twice",
                  ARG(second_keyword))]]  //
  [[qljs::message("'{0}' already written here",
                  ARG(first_keyword))]]  //
  Source_Code_Span first_keyword;
  Source_Code_Span second_keyword;
};

struct Diag_Unclosed_Block_Comment {
  [[qljs::diag("E0037", Diagnostic_Severity::error)]]             //
  [[qljs::message("unclosed block comment", ARG(comment_open))]]  //
  Source_Code_Span comment_open;
};

struct Diag_Unclosed_Class_Block {
  [[qljs::diag("E0199", Diagnostic_Severity::error)]]  //
  [[qljs::message("unclosed class; expected '}' by end of file",
                  ARG(block_open))]]  //
  Source_Code_Span block_open;
};

struct Diag_Unclosed_Code_Block {
  [[qljs::diag("E0134", Diagnostic_Severity::error)]]  //
  [[qljs::message("unclosed code block; expected '}' by end of file",
                  ARG(block_open))]]  //
  Source_Code_Span block_open;
};

struct Diag_Unclosed_Interface_Block {
  [[qljs::diag("E0215", Diagnostic_Severity::error)]]  //
  [[qljs::message("unclosed interface; expected '}' by end of file",
                  ARG(block_open))]]  //
  Source_Code_Span block_open;
};

struct Diag_Unclosed_Identifier_Escape_Sequence {
  [[qljs::diag("E0038", Diagnostic_Severity::error)]]  //
  [[qljs::message("unclosed identifier escape sequence",
                  ARG(escape_sequence))]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Unclosed_Object_Literal {
  [[qljs::diag("E0161", Diagnostic_Severity::error)]]  //
  [[qljs::message("unclosed object literal; expected '}'",
                  ARG(expected_object_close))]]                       //
  [[qljs::message("object literal started here", ARG(object_open))]]  //
  Source_Code_Span object_open;
  Source_Code_Span expected_object_close;
};

struct Diag_Unclosed_Regexp_Literal {
  [[qljs::diag("E0039", Diagnostic_Severity::error)]]                //
  [[qljs::message("unclosed regexp literal", ARG(regexp_literal))]]  //
  Source_Code_Span regexp_literal;
};

struct Diag_Unclosed_String_Literal {
  [[qljs::diag("E0040", Diagnostic_Severity::error)]]                //
  [[qljs::message("unclosed string literal", ARG(string_literal))]]  //
  Source_Code_Span string_literal;
};

struct Diag_Unclosed_JSX_String_Literal {
  [[qljs::diag("E0181", Diagnostic_Severity::error)]]                      //
  [[qljs::message("unclosed string literal", ARG(string_literal_begin))]]  //
  Source_Code_Span string_literal_begin;
};

struct Diag_Unclosed_Template {
  [[qljs::diag("E0041", Diagnostic_Severity::error)]]               //
  [[qljs::message("unclosed template", ARG(incomplete_template))]]  //
  Source_Code_Span incomplete_template;
};

struct Diag_Unexpected_Arrow_After_Expression {
  [[qljs::diag("E0160", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected '{0}'", ARG(arrow))]]    //
  [[qljs::message(
      "expected parameter for arrow function, but got an expression instead",
      ARG(expression))]]  //
  Source_Code_Span arrow;
  Source_Code_Span expression;
};

struct Diag_Unexpected_Arrow_After_Literal {
  [[qljs::diag("E0158", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected '{0}'", ARG(arrow))]]    //
  [[qljs::message(
      "expected parameter for arrow function, but got a literal instead",
      ARG(literal_parameter))]]  //
  Source_Code_Span arrow;
  Source_Code_Span literal_parameter;
};

struct Diag_Unexpected_Backslash_In_Identifier {
  [[qljs::diag("E0043", Diagnostic_Severity::error)]]                 //
  [[qljs::message("unexpected '\\' in identifier", ARG(backslash))]]  //
  Source_Code_Span backslash;
};

struct Diag_Unexpected_Case_Outside_Switch_Statement {
  [[qljs::diag("E0115", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected 'case' outside switch statement",
                  ARG(case_token))]]  //
  Source_Code_Span case_token;
};

struct Diag_Unexpected_Characters_In_Number {
  [[qljs::diag("E0044", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected characters in number literal",
                  ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Control_Character {
  [[qljs::diag("E0045", Diagnostic_Severity::error)]]                //
  [[qljs::message("unexpected control character", ARG(character))]]  //
  Source_Code_Span character;
};

struct Diag_Unexpected_Characters_In_Binary_Number {
  [[qljs::diag("E0046", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected characters in binary literal",
                  ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Characters_In_Octal_Number {
  [[qljs::diag("E0047", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected characters in octal literal",
                  ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Characters_In_Hex_Number {
  [[qljs::diag("E0048", Diagnostic_Severity::error)]]                         //
  [[qljs::message("unexpected characters in hex literal", ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Default_Outside_Switch_Statement {
  [[qljs::diag("E0116", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected 'default' outside switch statement",
                  ARG(default_token))]]  //
  Source_Code_Span default_token;
};

struct Diag_Unexpected_Greater_In_JSX_Text {
  [[qljs::diag("E0182", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead",
      ARG(greater))]]  //
  Source_Code_Span greater;
};

struct Diag_Unexpected_Literal_In_Parameter_List {
  [[qljs::diag("E0159", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "unexpected literal in parameter list; expected parameter name",
      ARG(literal))]]  //
  Source_Code_Span literal;
};

struct Diag_Unexpected_Right_Curly_In_JSX_Text {
  [[qljs::diag("E0183", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "'}' is not allowed directly in JSX text; write {{'}'} instead",
      ARG(right_curly))]]  //
  Source_Code_Span right_curly;
};

struct Diag_Unexpected_Question_In_Expression {
  [[qljs::diag("E0307", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected '?'", ARG(question))]]   //
  Source_Code_Span question;
};

struct Diag_Unexpected_Question_When_Destructuring {
  [[qljs::diag("E0309", Diagnostic_Severity::error)]]                    //
  [[qljs::message("unexpected '?' when destructuring", ARG(question))]]  //
  Source_Code_Span question;
};

struct Diag_Unexpected_Semicolon_After_Decorator {
  [[qljs::diag("E0410", Diagnostic_Severity::error)]]  //
  [[qljs::message("semicolon is not allowed after decorators",
                  ARG(semicolon))]]  //
  [[qljs::message("decorator starts here",
                  ARG(decorator_at))]]  //
  Source_Code_Span semicolon;
  Source_Code_Span decorator_at;
};

struct Diag_Unexpected_Semicolon_After_Overload_Signature {
  [[qljs::diag("E0400", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript overload signature can only have one semicolon",
                  ARG(extra_semicolon))]]  //
  [[qljs::message("original semicolon is here",
                  ARG(original_semicolon))]]  //
  Source_Code_Span extra_semicolon;
  Source_Code_Span original_semicolon;
};

struct Diag_Unexpected_Semicolon_In_C_Style_For_Loop {
  [[qljs::diag("E0102", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "C-style for loops have only three semicolon-separated components",
      ARG(semicolon))]]  //
  Source_Code_Span semicolon;
};

struct Diag_Unexpected_Semicolon_In_For_In_Loop {
  [[qljs::diag("E0110", Diagnostic_Severity::error)]]  //
  [[qljs::message("for-in loop expression cannot have semicolons",
                  ARG(semicolon))]]  //
  Source_Code_Span semicolon;
};

struct Diag_Unexpected_Semicolon_In_For_Of_Loop {
  [[qljs::diag("E0109", Diagnostic_Severity::error)]]  //
  [[qljs::message("for-of loop expression cannot have semicolons",
                  ARG(semicolon))]]  //
  Source_Code_Span semicolon;
};

struct Diag_Unopened_Block_Comment {
  [[qljs::diag("E0210", Diagnostic_Severity::error)]]              //
  [[qljs::message("unopened block comment", ARG(comment_close))]]  //
  Source_Code_Span comment_close;
};

struct Diag_Unused_Variable_Shadows {
  [[qljs::diag("E0196", Diagnostic_Severity::warning)]]  //
  [[qljs::message("new variable shadows existing variable",
                  ARG(shadowing_declaration))]]  //
  [[qljs::message("existing variable declared here",
                  ARG(shadowed_declaration))]]  //
  Source_Code_Span shadowing_declaration;
  Source_Code_Span shadowed_declaration;
};

struct Diag_No_Digits_In_Binary_Number {
  [[qljs::diag("E0049", Diagnostic_Severity::error)]]                        //
  [[qljs::message("binary number literal has no digits", ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_No_Digits_In_Hex_Number {
  [[qljs::diag("E0050", Diagnostic_Severity::error)]]                     //
  [[qljs::message("hex number literal has no digits", ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_No_Digits_In_Octal_Number {
  [[qljs::diag("E0051", Diagnostic_Severity::error)]]                       //
  [[qljs::message("octal number literal has no digits", ARG(characters))]]  //
  Source_Code_Span characters;
};

struct Diag_Non_Null_Assertion_Not_Allowed_In_Parameter {
  [[qljs::diag("E0260", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript non-null assertion is not allowed on parameters",
                  ARG(bang))]]  //
  Source_Code_Span bang;
};

struct Diag_Unexpected_Hash_Character {
  [[qljs::diag("E0052", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected '#'", ARG(where))]]      //
  Source_Code_Span where;
};

struct Diag_Unexpected_Bom_Before_Shebang {
  [[qljs::diag("E0095", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("unicode byte order mark (BOM) cannot appear before #! at "
                  "beginning of script", ARG(bom))]]  //
  // clang-format on
  Source_Code_Span bom;
};

struct Diag_Unexpected_Identifier_In_Expression {
  [[qljs::diag("E0147", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "unexpected identifier in expression; missing operator before",
      ARG(unexpected))]]  //
  Source_Code_Span unexpected;
};

// NOTE(strager): Try not to use this error. Find or make a more descriptive
// and helpful error instead.
struct Diag_Unexpected_Token {
  [[qljs::diag("E0054", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected token", ARG(token))]]    //
  Source_Code_Span token;
};

struct Diag_Unexpected_Token_After_Export {
  [[qljs::diag("E0112", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("unexpected token in export; expected 'export default "
                  "...' or 'export {{name}' or 'export * from ...' or "
                  "'export class' or 'export function' or 'export let'", ARG(unexpected_token))]]  //
  // clang-format on
  Source_Code_Span unexpected_token;
};

struct Diag_Unexpected_Token_In_Variable_Declaration {
  [[qljs::diag("E0114", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "unexpected token in variable declaration; expected variable name",
      ARG(unexpected_token))]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Unmatched_Indexing_Bracket {
  [[qljs::diag("E0055", Diagnostic_Severity::error)]]                //
  [[qljs::message("unmatched indexing bracket", ARG(left_square))]]  //
  Source_Code_Span left_square;
};

struct Diag_Unmatched_Parenthesis {
  [[qljs::diag("E0056", Diagnostic_Severity::error)]]     //
  [[qljs::message("unmatched parenthesis", ARG(where))]]  //
  Source_Code_Span where;
};

struct Diag_Unmatched_Right_Curly {
  [[qljs::diag("E0143", Diagnostic_Severity::error)]]   //
  [[qljs::message("unmatched '}'", ARG(right_curly))]]  //
  Source_Code_Span right_curly;
};

struct Diag_Use_Of_Undeclared_Parameter_In_Assertion_Signature {
  [[qljs::diag("E0428", Diagnostic_Severity::error)]]                 //
  [[qljs::message("{0} is not the name of a parameter", ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Use_Of_Undeclared_Parameter_In_Type_Predicate {
  [[qljs::diag("E0315", Diagnostic_Severity::error)]]                 //
  [[qljs::message("{0} is not the name of a parameter", ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Use_Of_Undeclared_Type {
  [[qljs::diag("E0214", Diagnostic_Severity::warning)]]        //
  [[qljs::message("use of undeclared type: {0}", ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Use_Of_Undeclared_Variable {
  [[qljs::diag("E0057", Diagnostic_Severity::warning)]]            //
  [[qljs::message("use of undeclared variable: {0}", ARG(name))]]  //
  Source_Code_Span name;
};

struct Diag_Variable_Used_Before_Declaration {
  [[qljs::diag("E0058", Diagnostic_Severity::error)]]                   //
  [[qljs::message("variable used before declaration: {0}", ARG(use))]]  //
  [[qljs::message("variable declared here", ARG(declaration))]]         //
  Source_Code_Span use;
  Source_Code_Span declaration;
};

struct Diag_Function_Call_Before_Declaration_In_Block_Scope {
  [[qljs::diag("E0077", Diagnostic_Severity::warning)]]  //
  [[qljs::message("function called before declaration in block scope: {0}",
                  ARG(use))]]                                    //
  [[qljs::message("function declared here", ARG(declaration))]]  //
  Source_Code_Span use;
  Source_Code_Span declaration;
};

struct Diag_Import_Cannot_Have_Declare_Keyword {
  [[qljs::diag("E0360", Diagnostic_Severity::error)]]  //
  [[qljs::message("cannot use 'declare' keyword with 'import'",
                  ARG(declare_keyword))]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Interface_Field_Cannot_Be_Accessor {
  [[qljs::diag("E0397", Diagnostic_Severity::error)]]  //
  [[qljs::message("'accessor' is not allowed for TypeScript interface fields",
                  ARG(accessor_keyword))]]  //
  Source_Code_Span accessor_keyword;
};

struct Diag_Interface_Field_Cannot_Be_Declare {
  [[qljs::diag("E0419", Diagnostic_Severity::error)]]  //
  [[qljs::message("'declare' is not allowed for TypeScript interface fields",
                  ARG(declare_keyword))]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Interface_Fields_Cannot_Have_Initializers {
  [[qljs::diag("E0221", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript interface fields cannot be initalized",
                  ARG(equal))]]  //
  Source_Code_Span equal;
};

struct Diag_Interface_Methods_Cannot_Be_Async {
  [[qljs::diag("E0217", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript interface methods cannot be marked 'async'",
                  ARG(async_keyword))]]  //
  Source_Code_Span async_keyword;
};

struct Diag_Interface_Methods_Cannot_Be_Generators {
  [[qljs::diag("E0218", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "TypeScript interface methods cannot be marked as a generator",
      ARG(star))]]  //
  Source_Code_Span star;
};

struct Diag_Interface_Methods_Cannot_Contain_Bodies {
  [[qljs::diag("E0220", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript interface methods cannot contain a body",
                  ARG(body_start))]]  //
  Source_Code_Span body_start;
};

struct Diag_Interface_Properties_Cannot_Be_Explicitly_Public {
  [[qljs::diag("E0237", Diagnostic_Severity::error)]]  //
  [[qljs::message("interface properties cannot be marked public explicitly",
                  ARG(public_keyword))]]  //
  Source_Code_Span public_keyword;
};

struct Diag_Interface_Properties_Cannot_Be_Private {
  [[qljs::diag("E0219", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "interface properties are always public and cannot be private",
      ARG(property_name_or_private_keyword))]]  //
  Source_Code_Span property_name_or_private_keyword;
};

struct Diag_Interface_Properties_Cannot_Be_Protected {
  [[qljs::diag("E0288", Diagnostic_Severity::error)]]  //
  // clang-format off
  [[qljs::message("TypeScript interface properties are always public and "
                  "cannot be marked protected", ARG(protected_keyword))]]  //
  // clang-format on
  Source_Code_Span protected_keyword;
};

struct Diag_Interface_Properties_Cannot_Be_Static {
  [[qljs::diag("E0216", Diagnostic_Severity::error)]]  //
  [[qljs::message("TypeScript interface properties cannot be 'static'",
                  ARG(static_keyword))]]  //
  Source_Code_Span static_keyword;
};

struct Diag_Invalid_Break {
  [[qljs::diag("E0200", Diagnostic_Severity::error)]]  //
  [[qljs::message("break can only be used inside of a loop or switch",
                  ARG(break_statement))]]  //
  Source_Code_Span break_statement;
};

struct Diag_Invalid_Continue {
  [[qljs::diag("E0201", Diagnostic_Severity::error)]]  //
  [[qljs::message("continue can only be used inside of a loop",
                  ARG(continue_statement))]]  //
  Source_Code_Span continue_statement;
};

struct Diag_Pointless_String_Comp_Contains_Lower {
  [[qljs::diag("E0286", Diagnostic_Severity::warning)]]  //
  [[qljs::message("lower case letters compared with toUpperCase",
                  ARG(span_operator))]]  //
  Source_Code_Span span_operator;
};

struct Diag_Pointless_String_Comp_Contains_Upper {
  [[qljs::diag("E0287", Diagnostic_Severity::warning)]]  //
  [[qljs::message("upper case letters compared with toLowerCase",
                  ARG(span_operator))]]  //
  Source_Code_Span span_operator;
};

struct Diag_Pointless_Strict_Comp_Against_Array_Literal {
  [[qljs::diag("E0341", Diagnostic_Severity::warning)]]  //
  [[qljs::message("using '{0}' against an array literal does not compare items",
                  ARG(equals_operator))]]  //
  Source_Code_Span equals_operator;
};

struct Diag_Pointless_Comp_Against_Arrow_Function {
  [[qljs::diag("E0342", Diagnostic_Severity::warning)]]  //
  [[qljs::message("using '{0}' against an arrow function always returns '{1}'",
                  ARG(equals_operator), ARG(comparison_result))]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Comp_Against_Class_Literal {
  [[qljs::diag("E0343", Diagnostic_Severity::warning)]]  //
  [[qljs::message("using '{0}' against a class literal always returns '{1}'",
                  ARG(equals_operator), ARG(comparison_result))]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Strict_Comp_Against_Empty_Array_Literal {
  [[qljs::diag("E0344", Diagnostic_Severity::warning)]]  //
  [[qljs::message("'{0} []' is always '{1}'", ARG(equals_operator),
                  ARG(comparison_result))]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Comp_Against_Object_Literal {
  [[qljs::diag("E0345", Diagnostic_Severity::warning)]]  //
  [[qljs::message("using '{0}' against an object literal always returns '{1}'",
                  ARG(equals_operator), ARG(comparison_result))]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Comp_Against_Regular_Expression_Literal {
  [[qljs::diag("E0346", Diagnostic_Severity::warning)]]  //
  [[qljs::message(
      "using '{0}' against a regular expression literal always returns '{1}'",
      ARG(equals_operator), ARG(comparison_result))]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Unexpected_Function_Parameter_Is_Parenthesized {
  [[qljs::diag("E0349", Diagnostic_Severity::error)]]  //
  [[qljs::message("function parameter cannot be parenthesized",
                  ARG(left_paren_to_right_paren))]]  //
  Source_Code_Span left_paren_to_right_paren;
};

struct Diag_Unexpected_Comma_After_Class_Field {
  [[qljs::diag("E0330", Diagnostic_Severity::error)]]         //
  [[qljs::message("',' should be ';' instead", ARG(comma))]]  //
  Source_Code_Span comma;
};

struct Diag_Unexpected_Colon_After_Generic_Definition {
  [[qljs::diag("E0331", Diagnostic_Severity::error)]]               //
  [[qljs::message("':' should be 'extends' instead", ARG(colon))]]  //
  Source_Code_Span colon;
};

struct Diag_Pointless_Nullish_Coalescing_Operator {
  [[qljs::diag("E0369", Diagnostic_Severity::warning)]]  //
  // clang-format off
  [[qljs::message("nullish coalescing operator does nothing when left "
                  "operand is never null", ARG(question_question))]]  //
  // clang-format on
  Source_Code_Span question_question;
};

struct Diag_Bang_Equal_Equal_Interpreted_As_Non_Null_Assertion {
  [[qljs::diag("E0373", Diagnostic_Severity::warning)]]  //
  [[qljs::message("unexpected whitespace between '!' and '=='",
                  ARG(unexpected_space))]]  //
  [[qljs::message(
      "'!' here treated as the TypeScript non-null assertion operator",
      ARG(bang))]]  //
  Source_Code_Span unexpected_space;
  Source_Code_Span bang;
};

struct Diag_Unexpected_Space_Between_Bang_And_Equal_Equal {
  [[qljs::diag("E0374", Diagnostic_Severity::error)]]  //
  [[qljs::message("unexpected whitespace between '!' and '=='",
                  ARG(unexpected_space))]]  //
  Source_Code_Span unexpected_space;
};

struct Diag_JSX_Prop_Is_Missing_Expression {
  [[qljs::diag("E0376", Diagnostic_Severity::error)]]  //
  [[qljs::message("JSX prop is missing an expression",
                  ARG(left_brace_to_right_brace))]]  //
  Source_Code_Span left_brace_to_right_brace;
};

struct Diag_Keyword_Contains_Escape_Characters {
  [[qljs::diag("E0381", Diagnostic_Severity::error)]]  //
  [[qljs::message("Keywords in TypeScript does not allow escape characters",
                  ARG(escape_character_in_keyword))]]  //
  Source_Code_Span escape_character_in_keyword;
};

struct Diag_Access_Specifier_Must_Precede_Other_Modifiers {
  [[qljs::diag("E0380", Diagnostic_Severity::error)]]  //
  [[qljs::message("'{0}' access specifier must precede '{1}'",
                  ARG(second_modifier), ARG(first_modifier))]]  //
  Source_Code_Span second_modifier;
  Source_Code_Span first_modifier;
};

struct Diag_Spread_Must_Precede_Expression {
  [[qljs::diag("E0708", Diagnostic_Severity::error)]]                      //
  [[qljs::message("unexpected '...'; expected expression", ARG(spread))]]  //
  Source_Code_Span spread;
};

struct Diag_Spread_Must_Precede_Variable_Name {
  [[qljs::diag("E0709", Diagnostic_Severity::error)]]                   //
  [[qljs::message("expected variable name after '...'", ARG(spread))]]  //
  Source_Code_Span spread;
};

struct Diag_Variable_Assigned_To_Self_Is_Noop {
  [[qljs::diag("E0383", Diagnostic_Severity::warning)]]  //
  [[qljs::message("variable assignment to self is no-op",
                  ARG(assignment_statement))]]  //
  Source_Code_Span assignment_statement;
};

struct Diag_Xor_Used_As_Exponentiation {
  [[qljs::diag("E0710", Diagnostic_Severity::warning)]]  //
  [[qljs::message("'^' is the XOR operator; to exponentiate, use '**' instead",
                  ARG(xor_operator))]]  //
  Source_Code_Span xor_operator;
};

struct Diag_Expected_Expression_In_Template_Literal {
  [[qljs::diag("E0711", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing expression in placeholder within template literal",
                  ARG(placeholder))]]  //
  Source_Code_Span placeholder;
};

struct Diag_Missing_Comma_Between_Array_Elements {
  [[qljs::diag("E0712", Diagnostic_Severity::error)]]  //
  [[qljs::message("missing ',' between array elements",
                  ARG(expected_comma))]]  //
  Source_Code_Span expected_comma;
};

struct Diag_Class_Generator_On_Getter_Or_Setter {
  [[qljs::diag("E0713", Diagnostic_Severity::error)]]  //
  [[qljs::message("getters and setters cannot be generators",
                  ARG(star_token))]]                           //
  [[qljs::message("'{0}' here", ARG(getter_setter_keyword))]]  //
  Source_Code_Span star_token;
  Source_Code_Span getter_setter_keyword;
};

struct Diag_Class_Async_On_Getter_Or_Setter {
  [[qljs::diag("E0714", Diagnostic_Severity::error)]]  //
  [[qljs::message("'async' keyword is not allowed on getters or setters",
                  ARG(async_keyword))]]                        //
  [[qljs::message("'{0}' here", ARG(getter_setter_keyword))]]  //
  Source_Code_Span async_keyword;
  Source_Code_Span getter_setter_keyword;
};

struct Diag_Multiple_Export_Defaults {
  [[qljs::diag("E0715", Diagnostic_Severity::error)]]  //
  [[qljs::message(
      "cannot use multiple `export default` statements in one module",
      ARG(second_export_default))]]  //
  [[qljs::message("export default previously appeared here",
                  ARG(first_export_default))]]  //
  Source_Code_Span second_export_default;
  Source_Code_Span first_export_default;
};

struct Diag_Unintuitive_Bitshift_Precedence {
  [[qljs::diag("E0716", Diagnostic_Severity::warning)]]  //
  // clang-format off
  [[qljs::message("unintuitive operator precedence when using & and '{0}'; "
                  "'{0}' evaluates before &",
                  ARG(bitshift_operator))]]         //
  // clang-format on
  [[qljs::message("'&' here", ARG(and_operator))]]  //
  Source_Code_Span bitshift_operator;
  Source_Code_Span and_operator;
};

struct Diag_TypeScript_Namespace_Alias_Cannot_Use_Import_Type {
  [[qljs::diag("E0717", Diagnostic_Severity::error)]]  //
  [[qljs::message("namespace alias cannot use 'import type'",
                  ARG(type_keyword))]]  //
  Source_Code_Span type_keyword;
};
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
