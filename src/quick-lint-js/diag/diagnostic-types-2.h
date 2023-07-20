// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_DIAG_DIAGNOSTIC_TYPES_2_H
#define QUICK_LINT_JS_DIAG_DIAGNOSTIC_TYPES_2_H

#include <quick-lint-js/fe/language.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/port/warning.h>

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wunknown-attributes")
QLJS_WARNING_IGNORE_GCC("-Wattributes")

namespace quick_lint_js {
struct Diag_Abstract_Field_Cannot_Have_Initializer {
  [[qljs::diag("E0295", error)]]                                          //
  [[qljs::message("abstract fields cannot have default values", equal)]]  //
  [[qljs::message("field marked abstract here", abstract_keyword)]]       //
  Source_Code_Span equal;
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Methods_Cannot_Be_Async {
  [[qljs::diag("E0298", error)]]  //
  [[qljs::message("abstract methods cannot be marked 'async'",
                  async_keyword)]]  //
  Source_Code_Span async_keyword;
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Methods_Cannot_Be_Generators {
  [[qljs::diag("E0299", error)]]  //
  [[qljs::message("abstract methods cannot be marked as a generator",
                  star)]]  //
  Source_Code_Span star;
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Property_Not_Allowed_In_Interface {
  [[qljs::diag("E0297", error)]]  //
  [[qljs::message("abstract properties are not allowed in interfaces",
                  abstract_keyword)]]  //
  Source_Code_Span abstract_keyword;
};

struct Diag_Abstract_Property_Not_Allowed_In_Non_Abstract_Class {
  [[qljs::diag("E0296", error)]]  //
  [[qljs::message("abstract properties are only allowed in abstract classes",
                  abstract_keyword)]]                               //
  [[qljs::message("class is not marked abstract", class_keyword)]]  //
  Source_Code_Span abstract_keyword;
  Source_Code_Span class_keyword;
};

struct Diag_Abstract_Methods_Cannot_Contain_Bodies {
  [[qljs::diag("E0294", error)]]                                           //
  [[qljs::message("abstract methods cannot contain a body", body_start)]]  //
  Source_Code_Span body_start;
};

struct Diag_Adjacent_JSX_Without_Parent {
  [[qljs::diag("E0189", error)]]  //
  [[qljs::message("missing '<>' and '</>' to enclose multiple children",
                  begin)]]                     //
  [[qljs::message("children end here", end)]]  //
  Source_Code_Span begin;
  Source_Code_Span begin_of_second_element;
  Source_Code_Span end;
};

struct Diag_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses {
  [[qljs::diag("E0255", error)]]  //
  [[qljs::message("missing parentheses around parameter",
                  parameter_and_annotation)]]  //
  [[qljs::message("TypeScript type annotation requires parentheses",
                  type_colon)]]  //
  Source_Code_Span parameter_and_annotation;
  Source_Code_Span type_colon;
};

struct Diag_TypeScript_Question_In_Type_Expression_Should_Be_Void {
  [[qljs::diag("E0348", error)]]  //
  // clang-format off
  [[qljs::message("invalid usage of ? as a prefix or suffix in the a type "
                  "expression, use '| void' instead",
                  question)]]  //
  // clang-format on
  Source_Code_Span question;
};

struct Diag_Assignment_Before_Variable_Declaration {
  [[qljs::diag("E0001", error)]]                                             //
  [[qljs::message("variable assigned before its declaration", assignment)]]  //
  [[qljs::message("variable declared here", declaration)]]                   //
  Source_Code_Span assignment;
  Source_Code_Span declaration;
};

struct Diag_Assignment_Makes_Condition_Constant {
  [[qljs::diag("E0188", warning)]]  //
  [[qljs::message("'=' changes variables; to compare, use '===' instead",
                  assignment_operator)]]  //
  Source_Code_Span assignment_operator;
};

struct Diag_Assignment_To_Const_Global_Variable {
  [[qljs::diag("E0002", error)]]                                        //
  [[qljs::message("assignment to const global variable", assignment)]]  //
  Source_Code_Span assignment;
};

struct Diag_Assignment_To_Const_Variable {
  [[qljs::diag("E0003", error)]]                                  //
  [[qljs::message("assignment to const variable", assignment)]]   //
  [[qljs::message("const variable declared here", declaration)]]  //
  Source_Code_Span declaration;
  Source_Code_Span assignment;
  Variable_Kind var_kind;
};

struct Diag_Assignment_To_Imported_Variable {
  [[qljs::diag("E0185", error)]]                                     //
  [[qljs::message("assignment to imported variable", assignment)]]   //
  [[qljs::message("imported variable declared here", declaration)]]  //
  Source_Code_Span declaration;
  Source_Code_Span assignment;
  Variable_Kind var_kind;
};

struct Diag_Assignment_To_Const_Variable_Before_Its_Declaration {
  [[qljs::diag("E0004", error)]]  //
  [[qljs::message("assignment to const variable before its declaration",
                  assignment)]]                                   //
  [[qljs::message("const variable declared here", declaration)]]  //
  Source_Code_Span declaration;
  Source_Code_Span assignment;
  Variable_Kind var_kind;
};

struct Diag_Assignment_To_Undeclared_Variable {
  [[qljs::diag("E0059", warning)]]                                    //
  [[qljs::message("assignment to undeclared variable", assignment)]]  //
  Source_Code_Span assignment;
};

struct Diag_Await_Operator_Outside_Async {
  [[qljs::diag("E0162", error)]]  //
  [[qljs::message("'await' is only allowed in async functions",
                  await_operator)]]  //
  Source_Code_Span await_operator;
};

struct Diag_Await_Followed_By_Arrow_Function {
  [[qljs::diag("E0178", error)]]  //
  [[qljs::message(
      "'await' cannot be followed by an arrow function; use 'async' instead",
      await_operator)]]  //
  Source_Code_Span await_operator;
};

struct Diag_Async_Static_Method {
  [[qljs::diag("E0269", error)]]  //
  [[qljs::message("'async static' is not allowed; write 'static async' instead",
                  async_static)]]  //
  Source_Code_Span async_static;
};

struct Diag_Async_Export_Function {
  [[qljs::diag("E0326", error)]]  //
  [[qljs::message("'async export' is not allowed; write 'export async' instead",
                  async_export)]]  //
  Source_Code_Span async_export;
};

struct Diag_Declare_Class_Fields_Cannot_Have_Initializers {
  [[qljs::diag("E0335", error)]]                                           //
  [[qljs::message("'declare class' fields cannot be initalized", equal)]]  //
  Source_Code_Span equal;
};

struct Diag_Declare_Class_Methods_Cannot_Be_Async {
  [[qljs::diag("E0338", error)]]  //
  [[qljs::message("'declare class' methods cannot be marked 'async'",
                  async_keyword)]]  //
  Source_Code_Span async_keyword;
};

struct Diag_Declare_Class_Methods_Cannot_Be_Generators {
  [[qljs::diag("E0337", error)]]  //
  [[qljs::message("'declare class' methods cannot be marked as a generator",
                  star)]]  //
  Source_Code_Span star;
};

struct Diag_Declare_Class_Methods_Cannot_Contain_Bodies {
  [[qljs::diag("E0333", error)]]  //
  [[qljs::message("'declare class' methods cannot contain a body",
                  body_start)]]  //
  Source_Code_Span body_start;
};

struct Diag_Declare_Abstract_Class_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0340", error)]]  //
  [[qljs::message(
      "TypeScript 'declare abstract class' is not allowed in JavaScript",
      declare_keyword)]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Class_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0339", error)]]  //
  [[qljs::message("TypeScript 'declare class' is not allowed in JavaScript",
                  declare_keyword)]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Function_Cannot_Be_Async {
  [[qljs::diag("E0354", error)]]  //
  [[qljs::message("'declare function' cannot be marked 'async'",
                  async_keyword)]]  //
  Source_Code_Span async_keyword;
};

struct Diag_Declare_Function_Cannot_Be_Generator {
  [[qljs::diag("E0355", error)]]  //
  [[qljs::message("'declare function' cannot be marked as a generator",
                  star)]]  //
  Source_Code_Span star;
};

struct Diag_Declare_Function_Cannot_Have_Body {
  [[qljs::diag("E0353", error)]]                                          //
  [[qljs::message("'declare function' cannot have a body", body_start)]]  //
  [[qljs::message("'declare function' here", declare_keyword)]]           //
  Source_Code_Span body_start;
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Function_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0352", error)]]  //
  [[qljs::message("TypeScript 'declare function' is not allowed in JavaScript",
                  declare_keyword)]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Keyword_Is_Not_Allowed_Inside_Declare_Namespace {
  [[qljs::diag("E0358", error)]]  //
  [[qljs::message(
      "'declare' should not be written inside a 'declare namespace'",
      declare_keyword)]]  //
  [[qljs::message("containing 'declare namespace' starts here",
                  declare_namespace_declare_keyword)]]  //
  Source_Code_Span declare_keyword;
  Source_Code_Span declare_namespace_declare_keyword;
};

struct Diag_Declare_Namespace_Cannot_Contain_Statement {
  [[qljs::diag("E0357", error)]]  //
  [[qljs::message(
      "'declare namespace' cannot contain statements, only declarations",
      first_statement_token)]]                          //
  [[qljs::message("'declare' here", declare_keyword)]]  //
  Source_Code_Span first_statement_token;
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Namespace_Cannot_Import_Module {
  [[qljs::diag("E0362", error)]]  //
  [[qljs::message("cannot import a module from inside a 'declare namespace'",
                  importing_keyword)]]                                   //
  [[qljs::message("'declare namespace' starts here", declare_keyword)]]  //
  Source_Code_Span importing_keyword;
  Source_Code_Span declare_keyword;
};

struct Diag_Declare_Var_Cannot_Have_Initializer {
  [[qljs::diag("E0351", error)]]  //
  [[qljs::message("'declare {1}' cannot have initializer", equal,
                  declaring_token)]]  //
  [[qljs::message("'declare {1}' started here", declare_keyword,
                  declaring_token)]]  //
  Source_Code_Span equal;
  Source_Code_Span declare_keyword;
  Source_Code_Span declaring_token;
};

struct Diag_Declare_Var_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0350", error)]]  //
  [[qljs::message("TypeScript 'declare {1}' is not allowed in JavaScript",
                  declare_keyword, declaring_token)]]  //
  Source_Code_Span declare_keyword;
  Source_Code_Span declaring_token;
};

struct Diag_Function_Async_Function {
  [[qljs::diag("E0327", error)]]  //
  [[qljs::message(
      "'function async' is not allowed; write 'async function' instead",
      function_async)]]  //
  Source_Code_Span function_async;
};

struct Diag_Big_Int_Literal_Contains_Decimal_Point {
  [[qljs::diag("E0005", error)]]                                     //
  [[qljs::message("BigInt literal contains decimal point", where)]]  //
  Source_Code_Span where;
};

struct Diag_Big_Int_Literal_Contains_Exponent {
  [[qljs::diag("E0006", error)]]                                //
  [[qljs::message("BigInt literal contains exponent", where)]]  //
  Source_Code_Span where;
};

struct Diag_C_Style_For_Loop_Is_Missing_Third_Component {
  [[qljs::diag("E0093", error)]]  //
  [[qljs::message("C-style for loop is missing its third component",
                  expected_last_component)]]  //
  Source_Code_Span expected_last_component;
  Source_Code_Span existing_semicolon;
};

struct Diag_Cannot_Assign_To_Loop_Variable_In_For_Of_Or_In_Loop {
  [[qljs::diag("E0173", error)]]  //
  [[qljs::message("cannot assign to loop variable in for of/in loop",
                  equal_token)]]  //
  Source_Code_Span equal_token;
};

struct Diag_Cannot_Access_Private_Identifier_Outside_Class {
  [[qljs::diag("E0208", error)]]  //
  [[qljs::message("cannot access private identifier outside class",
                  private_identifier)]]  //
  Source_Code_Span private_identifier;
};

struct Diag_Cannot_Assign_To_Variable_Named_Async_In_For_Of_Loop {
  [[qljs::diag("E0082", error)]]  //
  [[qljs::message("assigning to 'async' in a for-of loop requires parentheses",
                  async_identifier)]]  //
  Source_Code_Span async_identifier;
};

struct Diag_Cannot_Declare_Await_In_Async_Function {
  [[qljs::diag("E0069", error)]]                                           //
  [[qljs::message("cannot declare 'await' inside async function", name)]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Declare_Class_Named_Let {
  [[qljs::diag("E0007", error)]]                            //
  [[qljs::message("classes cannot be named 'let'", name)]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Declare_Variable_Named_Let_With_Let {
  [[qljs::diag("E0008", error)]]  //
  [[qljs::message("let statement cannot declare variables named 'let'",
                  name)]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Declare_Variable_With_Keyword_Name {
  [[qljs::diag("E0124", error)]]                                             //
  [[qljs::message("cannot declare variable named keyword '{0}'", keyword)]]  //
  Source_Code_Span keyword;
};

struct Diag_Cannot_Declare_Yield_In_Generator_Function {
  [[qljs::diag("E0071", error)]]  //
  [[qljs::message("cannot declare 'yield' inside generator function",
                  name)]]  //
  Source_Code_Span name;
};

struct Diag_Cannot_Export_Default_Variable {
  [[qljs::diag("E0076", error)]]  //
  [[qljs::message("cannot declare and export variable with 'export default'",
                  declaring_token)]]  //
  Source_Code_Span declaring_token;
};

struct Diag_Cannot_Export_Let {
  [[qljs::diag("E0009", error)]]                                        //
  [[qljs::message("cannot export variable named 'let'", export_name)]]  //
  Source_Code_Span export_name;
};

struct Diag_Cannot_Export_Variable_Named_Keyword {
  [[qljs::diag("E0144", error)]]  //
  [[qljs::message("cannot export variable named keyword '{0}'",
                  export_name)]]  //
  Source_Code_Span export_name;
};

struct Diag_Cannot_Import_Let {
  [[qljs::diag("E0010", error)]]                         //
  [[qljs::message("cannot import 'let'", import_name)]]  //
  Source_Code_Span import_name;
};

struct Diag_Cannot_Import_Variable_Named_Keyword {
  [[qljs::diag("E0145", error)]]  //
  [[qljs::message("cannot import variable named keyword '{0}'",
                  import_name)]]  //
  Source_Code_Span import_name;
};

struct Diag_Cannot_Import_From_Unquoted_Module {
  [[qljs::diag("E0235", error)]]                                             //
  [[qljs::message("missing quotes around module name '{0}'", import_name)]]  //
  Source_Code_Span import_name;
};

struct Diag_Cannot_Refer_To_Private_Variable_Without_Object {
  [[qljs::diag("E0155", error)]]  //
  [[qljs::message(
      "cannot reference private variables without object; use 'this.'",
      private_identifier)]]  //
  Source_Code_Span private_identifier;
};

struct Diag_Cannot_Update_Variable_During_Declaration {
  [[qljs::diag("E0136", error)]]  //
  [[qljs::message("cannot update variable with '{0}' while declaring it",
                  updating_operator)]]  //
  [[qljs::message("remove '{0}' to update an existing variable",
                  declaring_token)]]  //
  Source_Code_Span declaring_token;
  Source_Code_Span updating_operator;
};

struct Diag_Catch_Without_Try {
  [[qljs::diag("E0117", error)]]                                      //
  [[qljs::message("unexpected 'catch' without 'try'", catch_token)]]  //
  Source_Code_Span catch_token;
};

struct Diag_Class_Statement_Not_Allowed_In_Body {
  [[qljs::diag("E0149", error)]]  //
  [[qljs::message("missing body for {1:headlinese}", expected_body,
                  kind_of_statement)]]  //
  [[qljs::message(
      "a class statement is not allowed as the body of {1:singular}",
      class_keyword, kind_of_statement)]]  //
  Statement_Kind kind_of_statement;
  Source_Code_Span expected_body;
  Source_Code_Span class_keyword;
};

struct Diag_Character_Disallowed_In_Identifiers {
  [[qljs::diag("E0011", error)]]                                           //
  [[qljs::message("character is not allowed in identifiers", character)]]  //
  Source_Code_Span character;
};

struct Diag_Comma_Not_Allowed_After_Spread_Parameter {
  [[qljs::diag("E0070", error)]]                                             //
  [[qljs::message("commas are not allowed after spread parameter", comma)]]  //
  Source_Code_Span comma;
  Source_Code_Span spread;
};

struct Diag_Comma_Not_Allowed_Before_First_Generic_Parameter {
  [[qljs::diag("E0262", error)]]  //
  [[qljs::message("leading commas are not allowed in generic parameter lists",
                  unexpected_comma)]]  //
  Source_Code_Span unexpected_comma;
};

struct Diag_Comma_Not_Allowed_Between_Class_Methods {
  [[qljs::diag("E0209", error)]]  //
  [[qljs::message("commas are not allowed between class methods",
                  unexpected_comma)]]  //
  Source_Code_Span unexpected_comma;
};

struct Diag_Config_Json_Syntax_Error {
  [[qljs::diag("E0164", error)]]                 //
  [[qljs::message("JSON syntax error", where)]]  //
  Source_Code_Span where;
};

struct Diag_Config_Global_Groups_Group_Type_Mismatch {
  [[qljs::diag("E0170", error)]]                                         //
  [[qljs::message("\"global-groups\" entries must be strings", group)]]  //
  Source_Code_Span group;
};

struct Diag_Config_Global_Groups_Type_Mismatch {
  [[qljs::diag("E0169", error)]]  //
  [[qljs::message("\"global-groups\" must be a boolean or an array",
                  value)]]  //
  Source_Code_Span value;
};

struct Diag_Config_Globals_Descriptor_Type_Mismatch {
  [[qljs::diag("E0171", error)]]  //
  [[qljs::message("\"globals\" descriptor must be a boolean or an object",
                  descriptor)]]  //
  Source_Code_Span descriptor;
};

struct Diag_Config_Globals_Descriptor_Shadowable_Type_Mismatch {
  [[qljs::diag("E0166", error)]]  //
  [[qljs::message(
      "\"globals\" descriptor \"shadowable\" property must be a boolean",
      value)]]  //
  Source_Code_Span value;
};

struct Diag_Config_Globals_Descriptor_Writable_Type_Mismatch {
  [[qljs::diag("E0167", error)]]  //
  [[qljs::message(
      "\"globals\" descriptor \"writable\" property must be a boolean",
      value)]]  //
  Source_Code_Span value;
};

struct Diag_Config_Globals_Type_Mismatch {
  [[qljs::diag("E0168", error)]]                             //
  [[qljs::message("\"globals\" must be an object", value)]]  //
  Source_Code_Span value;
};

struct Diag_Depth_Limit_Exceeded {
  [[qljs::diag("E0203", error)]]                    //
  [[qljs::message("depth limit exceeded", token)]]  //
  Source_Code_Span token;
};

struct Diag_Dot_Not_Allowed_After_Generic_Arguments_In_Type {
  [[qljs::diag("E0259", error)]]  //
  [[qljs::message(
      "'.' is not allowed after generic arguments; write [\"{1}\"] instead",
      dot, property_name)]]  //
  Source_Code_Span dot;
  Source_Code_Span property_name;
};

struct Diag_Dot_Dot_Is_Not_An_Operator {
  [[qljs::diag("E0053", error)]]                                        //
  [[qljs::message("missing property name between '.' and '.'", dots)]]  //
  Source_Code_Span dots;
};

struct Diag_Duplicated_Cases_In_Switch_Statement {
  [[qljs::diag("E0347", warning)]]  //
  [[qljs::message("duplicated case clause in switch statement",
                  duplicated_switch_case)]]                           //
  [[qljs::message("this case will run instead", first_switch_case)]]  //
  Source_Code_Span first_switch_case;
  Source_Code_Span duplicated_switch_case;
};

struct Diag_Else_Has_No_If {
  [[qljs::diag("E0065", error)]]                                     //
  [[qljs::message("'else' has no corresponding 'if'", else_token)]]  //
  Source_Code_Span else_token;
};

struct Diag_Equals_Does_Not_Distribute_Over_Or {
  [[qljs::diag("E0190", warning)]]  //
  [[qljs::message(
      "missing comparison; '{1}' does not extend to the right side of '{0}'",
      or_operator, equals_operator)]]                     //
  [[qljs::message("'{0}' found here", equals_operator)]]  //
  Source_Code_Span or_operator;
  Source_Code_Span equals_operator;
};

struct Diag_Escaped_Character_Disallowed_In_Identifiers {
  [[qljs::diag("E0012", error)]]  //
  [[qljs::message("escaped character is not allowed in identifiers",
                  escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Escaped_Code_Point_In_Identifier_Out_Of_Range {
  [[qljs::diag("E0013", error)]]                                 //
  [[qljs::message("code point out of range", escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Escaped_Code_Point_In_Unicode_Out_Of_Range {
  [[qljs::diag("E0207", error)]]  //
  [[qljs::message(
      "code point in Unicode escape sequence must not be greater than U+10FFFF",
      escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Escaped_Hyphen_Not_Allowed_In_JSX_Tag {
  [[qljs::diag("E0019", error)]]  //
  [[qljs::message("escaping '-' is not allowed in tag names; write '-' instead",
                  escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Extra_Comma_Not_Allowed_Between_Arguments {
  [[qljs::diag("E0068", error)]]  //
  [[qljs::message("extra ',' is not allowed between function call arguments",
                  comma)]]  //
  Source_Code_Span comma;
};

struct Diag_Extra_Comma_Not_Allowed_Between_Enum_Members {
  [[qljs::diag("E0248", error)]]                                             //
  [[qljs::message("extra ',' is not allowed between enum members", comma)]]  //
  Source_Code_Span comma;
};

struct Diag_Misleading_Comma_Operator_In_Index_Operation {
  [[qljs::diag("E0450", warning)]]                                     //
  [[qljs::message("misleading use of ',' operator in index", comma)]]  //
  [[qljs::message("index starts here", left_square)]]                  //
  Source_Code_Span comma;
  Source_Code_Span left_square;
};

struct Diag_Misleading_Comma_Operator_In_Conditional_Statement {
  [[qljs::diag("E0451", warning)]]  //
  [[qljs::message("misleading use of ',' operator in conditional statement",
                  comma)]]  //
  Source_Code_Span comma;
};

struct Diag_Empty_Paren_After_Control_Statement {
  [[qljs::diag("E0452", error)]]                                           //
  [[qljs::message("expected expression after '('", expected_expression)]]  //
  [[qljs::message("'{1}' statement starts here", token,
                  token)]]  //
  Source_Code_Span token;
  Source_Code_Span expected_expression;
};

struct Diag_Expected_As_Before_Imported_Namespace_Alias {
  [[qljs::diag("E0126", error)]]  //
  [[qljs::message("expected 'as' between '{1}' and '{2}'",
                  star_through_alias_token, star_token, alias)]]  //
  Source_Code_Span star_through_alias_token;
  Source_Code_Span alias;
  Source_Code_Span star_token;
};

struct Diag_Expected_Comma_To_Separate_Object_Literal_Entries {
  [[qljs::diag("E0131", error)]]  //
  [[qljs::message("expected ',' between object literal entries",
                  unexpected_token)]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Expected_Expression_Before_Newline {
  [[qljs::diag("E0014", error)]]                                  //
  [[qljs::message("expected expression before newline", where)]]  //
  Source_Code_Span where;
};

struct Diag_Expected_Expression_For_Switch_Case {
  [[qljs::diag("E0140", error)]]                                     //
  [[qljs::message("expected expression after 'case'", case_token)]]  //
  Source_Code_Span case_token;
};

struct Diag_Expected_Expression_Before_Semicolon {
  [[qljs::diag("E0015", error)]]                                    //
  [[qljs::message("expected expression before semicolon", where)]]  //
  Source_Code_Span where;
};

struct Diag_Expected_From_And_Module_Specifier {
  [[qljs::diag("E0129", error)]]                                      //
  [[qljs::message("expected 'from \"name_of_module.mjs\"'", where)]]  //
  Source_Code_Span where;
};

struct Diag_Expected_From_Before_Module_Specifier {
  [[qljs::diag("E0128", error)]]  //
  [[qljs::message("expected 'from' before module specifier",
                  module_specifier)]]  //
  Source_Code_Span module_specifier;
};

struct Diag_Expected_Hex_Digits_In_Unicode_Escape {
  [[qljs::diag("E0016", error)]]  //
  [[qljs::message("expected hexadecimal digits in Unicode escape sequence",
                  escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Expected_Left_Curly {
  [[qljs::diag("E0107", error)]]                           //
  [[qljs::message("expected '{{'", expected_left_curly)]]  //
  Source_Code_Span expected_left_curly;
};

struct Diag_Expected_Right_Paren_For_Function_Call {
  [[qljs::diag("E0141", error)]]  //
  [[qljs::message("expected ')' to close function call",
                  expected_right_paren)]]                      //
  [[qljs::message("function call started here", left_paren)]]  //
  Source_Code_Span expected_right_paren;
  Source_Code_Span left_paren;
};

struct Diag_Expected_Parentheses_Around_Do_While_Condition {
  [[qljs::diag("E0084", error)]]  //
  [[qljs::message("do-while loop needs parentheses around condition",
                  condition)]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_Do_While_Condition {
  [[qljs::diag("E0085", error)]]  //
  [[qljs::message("do-while loop is missing '{1}' around condition", where,
                  token)]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_If_Condition {
  [[qljs::diag("E0017", error)]]  //
  [[qljs::message("if statement needs parentheses around condition",
                  condition)]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_If_Condition {
  [[qljs::diag("E0018", error)]]  //
  [[qljs::message("if statement is missing '{1}' around condition", where,
                  token)]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_Switch_Condition {
  [[qljs::diag("E0091", error)]]  //
  [[qljs::message("switch statement needs parentheses around condition",
                  condition)]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_Switch_Condition {
  [[qljs::diag("E0092", error)]]  //
  [[qljs::message("switch statement is missing '{1}' around condition", where,
                  token)]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_While_Condition {
  [[qljs::diag("E0087", error)]]  //
  [[qljs::message("while loop needs parentheses around condition",
                  condition)]]  //
  Source_Code_Span condition;
};

struct Diag_Expected_Parenthesis_Around_While_Condition {
  [[qljs::diag("E0088", error)]]  //
  [[qljs::message("while loop is missing '{1}' around condition", where,
                  token)]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Parentheses_Around_With_Expression {
  [[qljs::diag("E0089", error)]]  //
  [[qljs::message("with statement needs parentheses around expression",
                  expression)]]  //
  Source_Code_Span expression;
};

struct Diag_Expected_Parenthesis_Around_With_Expression {
  [[qljs::diag("E0090", error)]]  //
  [[qljs::message("with statement is missing '{1}' around expression", where,
                  token)]]  //
  Source_Code_Span where;
  Char8 token;
};

struct Diag_Expected_Variable_Name_For_Catch {
  [[qljs::diag("E0135", error)]]                                             //
  [[qljs::message("expected variable name for 'catch'", unexpected_token)]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Expected_Variable_Name_For_Import_As {
  [[qljs::diag("E0175", error)]]  //
  [[qljs::message("expected variable name for 'import'-'as'",
                  unexpected_token)]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Exporting_Requires_Default {
  [[qljs::diag("E0067", error)]]                                 //
  [[qljs::message("exporting requires 'default'", expression)]]  //
  Source_Code_Span expression;
};

struct Diag_Exporting_Requires_Curlies {
  [[qljs::diag("E0066", error)]]                               //
  [[qljs::message("exporting requires '{{' and '}'", names)]]  //
  Source_Code_Span names;
};

struct Diag_Exporting_String_Name_Only_Allowed_For_Export_From {
  [[qljs::diag("E0153", error)]]  //
  [[qljs::message("forwarding exports are only allowed in export-from",
                  export_name)]]  //
  Source_Code_Span export_name;
};

struct Diag_Finally_Without_Try {
  [[qljs::diag("E0118", error)]]                                          //
  [[qljs::message("unexpected 'finally' without 'try'", finally_token)]]  //
  Source_Code_Span finally_token;
};

struct Diag_Function_Statement_Not_Allowed_In_Body {
  [[qljs::diag("E0148", error)]]  //
  [[qljs::message("missing body for {1:headlinese}", expected_body,
                  kind_of_statement)]]  //
  [[qljs::message(
      "a function statement is not allowed as the body of {1:singular}",
      function_keywords, kind_of_statement)]]  //
  Statement_Kind kind_of_statement;
  Source_Code_Span expected_body;
  Source_Code_Span function_keywords;
};

struct Diag_Generator_Function_Star_Belongs_After_Keyword_Function {
  [[qljs::diag("E0204", error)]]  //
  [[qljs::message("generator function '*' belongs after keyword function",
                  star)]]  //
  Source_Code_Span star;
};

struct Diag_Generator_Function_Star_Belongs_Before_Name {
  [[qljs::diag("E0133", error)]]  //
  [[qljs::message("generator function '*' belongs before function name",
                  star)]]  //
  Source_Code_Span function_name;
  Source_Code_Span star;
};

struct Diag_Multiple_Commas_In_Generic_Parameter_List {
  [[qljs::diag("E0263", error)]]  //
  [[qljs::message(
      "only one comma is allowed between or after generic parameters",
      unexpected_comma)]]  //
  Source_Code_Span unexpected_comma;
};

struct Diag_In_Disallowed_In_C_Style_For_Loop {
  [[qljs::diag("E0108", error)]]  //
  [[qljs::message("'in' disallowed in C-style for loop initializer",
                  in_token)]]  //
  Source_Code_Span in_token;
};

struct Diag_Indexing_Requires_Expression {
  [[qljs::diag("E0075", error)]]                                 //
  [[qljs::message("indexing requires an expression", squares)]]  //
  Source_Code_Span squares;
};

struct Diag_Invalid_Expression_Left_Of_Assignment {
  [[qljs::diag("E0020", error)]]                                     //
  [[qljs::message("invalid expression left of assignment", where)]]  //
  Source_Code_Span where;
};

struct Diag_Invalid_Hex_Escape_Sequence {
  [[qljs::diag("E0060", error)]]                                          //
  [[qljs::message("invalid hex escape sequence: {0}", escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Invalid_Lone_Literal_In_Object_Literal {
  [[qljs::diag("E0021", error)]]                                      //
  [[qljs::message("invalid lone literal in object literal", where)]]  //
  Source_Code_Span where;
};

struct Diag_Invalid_Parameter {
  [[qljs::diag("E0151", error)]]                              //
  [[qljs::message("invalid function parameter", parameter)]]  //
  Source_Code_Span parameter;
};

struct Diag_Invalid_Quotes_Around_String_Literal {
  [[qljs::diag("E0197", error)]]  //
  [[qljs::message("'{0}' is not allowed for strings; use {1} instead",
                  opening_quote, suggested_quote)]]  //
  Source_Code_Span opening_quote;
  Char8 suggested_quote;
};

struct Diag_Invalid_Rhs_For_Dot_Operator {
  [[qljs::diag("E0074", error)]]  //
  // clang-format off
  [[qljs::message("'.' operator needs a key name; use + to concatenate "
                  "strings; use [] to access with a dynamic key",
                  dot)]]  //
  // clang-format on
  Source_Code_Span dot;
};

struct Diag_Invalid_Utf_8_Sequence {
  [[qljs::diag("E0022", error)]]                         //
  [[qljs::message("invalid UTF-8 sequence", sequence)]]  //
  Source_Code_Span sequence;
};

struct Diag_JSX_Attribute_Has_Wrong_Capitalization {
  [[qljs::diag("E0192", error)]]  //
  [[qljs::message("attribute has wrong capitalization; write '{1}' instead",
                  attribute_name, expected_attribute_name)]]  //
  Source_Code_Span attribute_name;
  String8_View expected_attribute_name;
};

struct Diag_JSX_Attribute_Renamed_By_React {
  [[qljs::diag("E0193", error)]]  //
  [[qljs::message("misspelled React attribute; write '{1}' instead",
                  attribute_name, react_attribute_name)]]  //
  Source_Code_Span attribute_name;
  String8_View react_attribute_name;
};

struct Diag_JSX_Event_Attribute_Should_Be_Camel_Case {
  [[qljs::diag("E0191", error)]]  //
  [[qljs::message("event attributes must be camelCase: '{1}'", attribute_name,
                  expected_attribute_name)]]  //
  Source_Code_Span attribute_name;
  String8_View expected_attribute_name;
};

struct Diag_JSX_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0177", error)]]  //
  [[qljs::message("React/JSX is not allowed in vanilla JavaScript code",
                  jsx_start)]]  //
  Source_Code_Span jsx_start;
};

struct Diag_JSX_Not_Allowed_In_TypeScript {
  [[qljs::diag("E0306", error)]]  //
  [[qljs::message("React/JSX is not allowed in TypeScript code",
                  jsx_start)]]  //
  Source_Code_Span jsx_start;
};

struct Diag_Keywords_Cannot_Contain_Escape_Sequences {
  [[qljs::diag("E0023", error)]]  //
  [[qljs::message("keywords cannot contain escape sequences",
                  escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Label_Named_Await_Not_Allowed_In_Async_Function {
  [[qljs::diag("E0206", error)]]  //
  [[qljs::message("label named 'await' not allowed in async function",
                  await)]]  //
  Source_Code_Span await;
  Source_Code_Span colon;
};

struct Diag_Legacy_Octal_Literal_May_Not_Be_Big_Int {
  [[qljs::diag("E0032", error)]]                                           //
  [[qljs::message("legacy octal literal may not be BigInt", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Legacy_Octal_Literal_May_Not_Contain_Underscores {
  [[qljs::diag("E0152", error)]]  //
  [[qljs::message("legacy octal literals may not contain underscores",
                  underscores)]]  //
  Source_Code_Span underscores;
};

struct Diag_Let_With_No_Bindings {
  [[qljs::diag("E0024", error)]]                    //
  [[qljs::message("{0} with no bindings", where)]]  //
  Source_Code_Span where;
};

struct Diag_Lexical_Declaration_Not_Allowed_In_Body {
  [[qljs::diag("E0150", error)]]  //
  [[qljs::message("missing body for {1:headlinese}", expected_body,
                  kind_of_statement)]]  //
  [[qljs::message(
      "a lexical declaration is not allowed as the body of {1:singular}",
      declaring_keyword, kind_of_statement)]]  //
  Statement_Kind kind_of_statement;
  Source_Code_Span expected_body;
  Source_Code_Span declaring_keyword;
};

struct Diag_Functions_Or_Methods_Should_Not_Have_Arrow_Operator {
  [[qljs::diag("E0174", error)]]  //
  [[qljs::message("functions/methods should not have '=>'",
                  arrow_operator)]]  //
  Source_Code_Span arrow_operator;
};

struct Diag_Methods_Should_Not_Use_Function_Keyword {
  [[qljs::diag("E0072", error)]]  //
  [[qljs::message("methods should not use the 'function' keyword",
                  function_token)]]  //
  Source_Code_Span function_token;
};

struct Diag_Mismatched_JSX_Tags {
  [[qljs::diag("E0187", error)]]  //
  [[qljs::message("mismatched JSX tags; expected '</{1}>'", closing_tag_name,
                  opening_tag_name_pretty)]]  //
  [[qljs::message("opening '<{1}>' tag here", opening_tag_name,
                  opening_tag_name_pretty)]]  //
  Source_Code_Span opening_tag_name;
  Source_Code_Span closing_tag_name;
  String8_View opening_tag_name_pretty;
};

struct Diag_Missing_Array_Close {
  [[qljs::diag("E0157", error)]]  //
  [[qljs::message("missing end of array; expected ']'",
                  expected_right_square)]]              //
  [[qljs::message("array started here", left_square)]]  //
  Source_Code_Span left_square;
  Source_Code_Span expected_right_square;
};

struct Diag_Missing_Arrow_Operator_In_Arrow_Function {
  [[qljs::diag("E0176", error)]]                                         //
  [[qljs::message("missing arrow operator for arrow function", where)]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Arrow_Function_Parameter_List {
  [[qljs::diag("E0105", error)]]                                     //
  [[qljs::message("missing parameters for arrow function", arrow)]]  //
  Source_Code_Span arrow;
};

struct Diag_Missing_Body_For_Catch_Clause {
  [[qljs::diag("E0119", error)]]                                   //
  [[qljs::message("missing body for catch clause", catch_token)]]  //
  Source_Code_Span catch_token;
};

struct Diag_Missing_Body_For_Class {
  [[qljs::diag("E0111", error)]]  //
  [[qljs::message("missing body for class",
                  class_keyword_and_name_and_heritage)]]  //
  Source_Code_Span class_keyword_and_name_and_heritage;
};

struct Diag_Missing_Body_For_Do_While_Statement {
  [[qljs::diag("E0101", error)]]                                 //
  [[qljs::message("missing body for do-while loop", do_token)]]  //
  Source_Code_Span do_token;
};

struct Diag_Missing_Body_For_Finally_Clause {
  [[qljs::diag("E0121", error)]]                                       //
  [[qljs::message("missing body for finally clause", finally_token)]]  //
  Source_Code_Span finally_token;
};

struct Diag_Missing_Body_For_For_Statement {
  [[qljs::diag("E0094", error)]]                                    //
  [[qljs::message("missing body for 'for' loop", for_and_header)]]  //
  Source_Code_Span for_and_header;
};

struct Diag_Missing_Body_For_If_Statement {
  [[qljs::diag("E0064", error)]]                                       //
  [[qljs::message("missing body for 'if' statement", expected_body)]]  //
  Source_Code_Span expected_body;
};

struct Diag_Missing_Body_For_Switch_Statement {
  [[qljs::diag("E0106", error)]]  //
  [[qljs::message("missing body for 'switch' statement",
                  switch_and_condition)]]  //
  Source_Code_Span switch_and_condition;
};

struct Diag_Missing_Body_For_Try_Statement {
  [[qljs::diag("E0120", error)]]                                  //
  [[qljs::message("missing body for try statement", try_token)]]  //
  Source_Code_Span try_token;
};

struct Diag_Missing_Body_For_TypeScript_Interface {
  [[qljs::diag("E0245", error)]]  //
  [[qljs::message("missing body for TypeScript interface",
                  interface_keyword_and_name_and_heritage)]]  //
  Source_Code_Span interface_keyword_and_name_and_heritage;
};

struct Diag_Missing_Body_For_TypeScript_Namespace {
  [[qljs::diag("E0356", error)]]                                             //
  [[qljs::message("missing body for TypeScript namespace", expected_body)]]  //
  Source_Code_Span expected_body;
};

struct Diag_Missing_Body_For_While_Statement {
  [[qljs::diag("E0104", error)]]                                         //
  [[qljs::message("missing body for while loop", while_and_condition)]]  //
  Source_Code_Span while_and_condition;
};

struct Diag_Missing_Catch_Or_Finally_For_Try_Statement {
  [[qljs::diag("E0122", error)]]  //
  [[qljs::message("missing catch or finally clause for try statement",
                  expected_catch_or_finally)]]               //
  [[qljs::message("try statement starts here", try_token)]]  //
  Source_Code_Span expected_catch_or_finally;
  Source_Code_Span try_token;
};

struct Diag_Missing_Catch_Variable_Between_Parentheses {
  [[qljs::diag("E0130", error)]]  //
  [[qljs::message("missing catch variable name between parentheses",
                  left_paren_to_right_paren)]]  //
  Source_Code_Span left_paren_to_right_paren;
  Source_Code_Span left_paren;
  Source_Code_Span right_paren;
};

struct Diag_Missing_Comma_Between_Object_Literal_Entries {
  [[qljs::diag("E0025", error)]]                                            //
  [[qljs::message("missing comma between object literal entries", where)]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Comma_Between_Generic_Parameters {
  [[qljs::diag("E0265", error)]]  //
  [[qljs::message("missing comma between generic parameters",
                  expected_comma)]]  //
  Source_Code_Span expected_comma;
};

struct Diag_Missing_Comma_Between_Variable_Declarations {
  [[qljs::diag("E0132", error)]]  //
  [[qljs::message("missing ',' between variable declarations",
                  expected_comma)]]  //
  Source_Code_Span expected_comma;
};

struct Diag_Missing_Colon_In_Conditional_Expression {
  [[qljs::diag("E0146", error)]]                                              //
  [[qljs::message("missing ':' in conditional expression", expected_colon)]]  //
  [[qljs::message("'?' creates a conditional expression", question)]]         //
  Source_Code_Span expected_colon;
  Source_Code_Span question;
};

struct Diag_Missing_Condition_For_If_Statement {
  [[qljs::diag("E0138", error)]]                                       //
  [[qljs::message("missing condition for if statement", if_keyword)]]  //
  Source_Code_Span if_keyword;
};

struct Diag_Missing_Condition_For_While_Statement {
  [[qljs::diag("E0139", error)]]                                             //
  [[qljs::message("missing condition for while statement", while_keyword)]]  //
  Source_Code_Span while_keyword;
};

struct Diag_Missing_Condition_For_Switch_Statement {
  [[qljs::diag("E0137", error)]]  //
  [[qljs::message("missing condition for switch statement",
                  switch_keyword)]]  //
  Source_Code_Span switch_keyword;
};

struct Diag_Missing_Dots_For_Attribute_Spread {
  [[qljs::diag("E0186", error)]]                                             //
  [[qljs::message("missing '...' in JSX attribute spread", expected_dots)]]  //
  Source_Code_Span expected_dots;
};

struct Diag_Missing_Equal_After_Variable {
  [[qljs::diag("E0202", error)]]                                   //
  [[qljs::message("missing '=' after variable", expected_equal)]]  //
  Source_Code_Span expected_equal;
};

struct Diag_Missing_Expression_Between_Parentheses {
  [[qljs::diag("E0078", error)]]  //
  [[qljs::message("missing expression between parentheses",
                  left_paren_to_right_paren)]]  //
  Source_Code_Span left_paren_to_right_paren;
  Source_Code_Span left_paren;
  Source_Code_Span right_paren;
};

struct Diag_Missing_For_Loop_Header {
  [[qljs::diag("E0125", error)]]                                          //
  [[qljs::message("missing header and body for 'for' loop", for_token)]]  //
  Source_Code_Span for_token;
};

struct Diag_Missing_For_Loop_Rhs_Or_Components_After_Expression {
  [[qljs::diag("E0097", error)]]  //
  [[qljs::message("for loop needs an iterable, or condition and update clauses",
                  header)]]  //
  [[qljs::message("use 'while' instead to loop until a condition is false",
                  for_token)]]  //
  Source_Code_Span header;
  Source_Code_Span for_token;
};

struct Diag_Missing_For_Loop_Rhs_Or_Components_After_Declaration {
  [[qljs::diag("E0098", error)]]  //
  [[qljs::message("for loop needs an iterable, or condition and update clauses",
                  header)]]  //
  Source_Code_Span header;
  Source_Code_Span for_token;
};

struct Diag_Missing_Function_Parameter_List {
  [[qljs::diag("E0073", error)]]  //
  [[qljs::message("missing function parameter list",
                  expected_parameter_list)]]  //
  Source_Code_Span expected_parameter_list;
};

struct Diag_Missing_Function_Body {
  [[qljs::diag("E0172", error)]]                                 //
  [[qljs::message("missing body for function", expected_body)]]  //
  Source_Code_Span expected_body;
};

struct Diag_Missing_Header_Of_For_Loop {
  [[qljs::diag("E0096", error)]]                       //
  [[qljs::message("missing for loop header", where)]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Initializer_In_Const_Declaration {
  [[qljs::diag("E0205", error)]]  //
  [[qljs::message("missing initializer in const declaration",
                  variable_name)]]  //
  Source_Code_Span variable_name;
};

struct Diag_Missing_Key_For_Object_Entry {
  [[qljs::diag("E0154", error)]]  //
  [[qljs::message("unexpected expression; missing key for object entry",
                  expression)]]  //
  Source_Code_Span expression;
};

struct Diag_Missing_Class_Method_Name {
  [[qljs::diag("E0229", error)]]                                     //
  [[qljs::message("missing name for class method", expected_name)]]  //
  Source_Code_Span expected_name;
};

struct Diag_Missing_Name_In_Function_Statement {
  [[qljs::diag("E0061", error)]]                                  //
  [[qljs::message("missing name in function statement", where)]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Name_In_Class_Statement {
  [[qljs::diag("E0080", error)]]                             //
  [[qljs::message("missing name of class", class_keyword)]]  //
  Source_Code_Span class_keyword;
};

struct Diag_Missing_Name_Of_Exported_Class {
  [[qljs::diag("E0081", error)]]                                      //
  [[qljs::message("missing name of exported class", class_keyword)]]  //
  Source_Code_Span class_keyword;
};

struct Diag_Missing_Name_Of_Exported_Function {
  [[qljs::diag("E0079", error)]]                                            //
  [[qljs::message("missing name of exported function", function_keyword)]]  //
  Source_Code_Span function_keyword;
};

struct Diag_Missing_Name_Or_Parentheses_For_Function {
  [[qljs::diag("E0062", error)]]                                        //
  [[qljs::message("missing name or parentheses for function", where)]]  //
  Source_Code_Span where;
  Source_Code_Span function;
};

struct Diag_Missing_Operand_For_Operator {
  [[qljs::diag("E0026", error)]]                            //
  [[qljs::message("missing operand for operator", where)]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Separator_Between_Object_Type_Entries {
  [[qljs::diag("E0257", error)]]  //
  [[qljs::message("missing ',', ';', or newline between object type entries",
                  expected_separator)]]  //
  Source_Code_Span expected_separator;
};

struct Diag_Redundant_Delete_Statement_On_Variable {
  [[qljs::diag("E0086", warning)]]  //
  [[qljs::message("redundant delete statement on variable",
                  delete_expression)]]  //
  Source_Code_Span delete_expression;
};

struct Diag_Missing_If_After_Else {
  [[qljs::diag("E0184", error)]]                               //
  [[qljs::message("missing 'if' after 'else'", expected_if)]]  //
  Source_Code_Span expected_if;
};

struct Diag_Missing_Operator_Between_Expression_And_Arrow_Function {
  [[qljs::diag("E0063", error)]]  //
  [[qljs::message("missing operator between expression and arrow function",
                  where)]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Parentheses_Around_Exponent_With_Unary_Lhs {
  [[qljs::diag("E0195", error)]]  //
  [[qljs::message("missing parentheses around operand of '{0}'",
                  exponent_expression)]]  //
  [[qljs::message(
      "'{0}' operator cannot be used before '**' without parentheses",
      unary_operator)]]  //
  Source_Code_Span exponent_expression;
  Source_Code_Span unary_operator;
};

struct Diag_Missing_Parentheses_Around_Self_Invoked_Function {
  [[qljs::diag("E0211", error)]]  //
  [[qljs::message("missing parentheses around self-invoked function",
                  invocation)]]                          //
  [[qljs::message("function starts here", func_start)]]  //
  Source_Code_Span invocation;
  Source_Code_Span func_start;
};

struct Diag_Missing_Parentheses_Around_Unary_Lhs_Of_Exponent {
  [[qljs::diag("E0194", error)]]  //
  [[qljs::message("missing parentheses around left-hand side of '**'",
                  unary_expression)]]  //
  [[qljs::message(
      "'**' operator cannot be used after unary '{1}' without parentheses",
      exponent_operator, unary_expression)]]  //
  Source_Code_Span unary_expression;
  Source_Code_Span exponent_operator;
};

struct Diag_Missing_Property_Name_For_Dot_Operator {
  [[qljs::diag("E0142", error)]]                                      //
  [[qljs::message("missing property name after '.' operator", dot)]]  //
  Source_Code_Span dot;
};

struct Diag_Missing_Semicolon_After_Abstract_Method {
  [[qljs::diag("E0293", error)]]  //
  [[qljs::message("missing semicolon after abstract method",
                  expected_semicolon)]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Declare_Class_Method {
  [[qljs::diag("E0334", error)]]  //
  [[qljs::message("missing semicolon after 'declare class' method",
                  expected_semicolon)]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Statement {
  [[qljs::diag("E0027", error)]]                                 //
  [[qljs::message("missing semicolon after statement", where)]]  //
  Source_Code_Span where;
};

struct Diag_Missing_Semicolon_After_Field {
  [[qljs::diag("E0223", error)]]                                          //
  [[qljs::message("missing semicolon after field", expected_semicolon)]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Index_Signature {
  [[qljs::diag("E0226", error)]]  //
  [[qljs::message("missing semicolon after index signature",
                  expected_semicolon)]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_After_Interface_Method {
  [[qljs::diag("E0292", error)]]  //
  [[qljs::message("missing semicolon after interface method",
                  expected_semicolon)]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_Between_For_Loop_Condition_And_Update {
  [[qljs::diag("E0100", error)]]  //
  [[qljs::message(
      "missing semicolon between condition and update parts of for loop",
      expected_semicolon)]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Semicolon_Between_For_Loop_Init_And_Condition {
  [[qljs::diag("E0099", error)]]  //
  [[qljs::message(
      "missing semicolon between init and condition parts of for loop",
      expected_semicolon)]]  //
  Source_Code_Span expected_semicolon;
};

struct Diag_Missing_Token_After_Export {
  [[qljs::diag("E0113", error)]]  //
  // clang-format off
  [[qljs::message("incomplete export; expected 'export default ...' or "
                  "'export {{name}' or 'export * from ...' or 'export "
                  "class' or 'export function' or 'export let'",
                  export_token)]]  //
  // clang-format on
  Source_Code_Span export_token;
};

struct Diag_Missing_Type_Between_Intersection_Or_Union {
  [[qljs::diag("E0258", error)]]  //
  [[qljs::message("missing type between '{1}' and '{0}'", right_operator,
                  left_operator)]]  //
  Source_Code_Span left_operator;
  Source_Code_Span right_operator;
};

// TODO(strager): Make more specific errors, like 'missing type after :',
// 'missing type after keyof', etc.
struct Diag_Missing_TypeScript_Type {
  [[qljs::diag("E0284", error)]]                               //
  [[qljs::message("missing TypeScript type", expected_type)]]  //
  Source_Code_Span expected_type;
};

struct Diag_Missing_Value_For_Object_Literal_Entry {
  [[qljs::diag("E0083", error)]]                               //
  [[qljs::message("missing value for object property", key)]]  //
  Source_Code_Span key;
};

struct Diag_Missing_Variable_Name_In_Declaration {
  [[qljs::diag("E0123", error)]]                           //
  [[qljs::message("missing variable name", equal_token)]]  //
  Source_Code_Span equal_token;
};

struct Diag_Missing_While_And_Condition_For_Do_While_Statement {
  [[qljs::diag("E0103", error)]]  //
  [[qljs::message("missing 'while (condition)' for do-while statement",
                  expected_while)]]                              //
  [[qljs::message("do-while statement starts here", do_token)]]  //
  Source_Code_Span do_token;
  Source_Code_Span expected_while;
};

struct Diag_Newline_Not_Allowed_Between_Async_And_Parameter_List {
  [[qljs::diag("E0163", error)]]  //
  // clang-format off
  [[qljs::message("newline is not allowed between 'async' and arrow "
                  "function parameter list",
                  async)]]                //
  // clang-format on
  [[qljs::message("arrow is here", arrow)]]  //
  Source_Code_Span async;
  Source_Code_Span arrow;
};

struct Diag_Newline_Not_Allowed_Between_Async_And_Function_Keyword {
  [[qljs::diag("E0317", error)]]  //
  [[qljs::message("newline is not allowed between 'async' and 'function'",
                  async_keyword)]]                           //
  [[qljs::message("'function' is here", function_keyword)]]  //
  Source_Code_Span async_keyword;
  Source_Code_Span function_keyword;
};

struct Diag_Newline_Not_Allowed_After_Abstract_Keyword {
  [[qljs::diag("E0300", error)]]  //
  [[qljs::message("newline is not allowed after 'abstract'",
                  abstract_keyword)]]  //
  Source_Code_Span abstract_keyword;
};

struct Diag_Newline_Not_Allowed_After_Export_Declare {
  [[qljs::diag("E0382", error)]]  //
  [[qljs::message("newline is not allowed after 'export declare'",
                  declare_keyword, export_keyword)]]  //
  Source_Code_Span declare_keyword;
  Source_Code_Span export_keyword;
};

struct Diag_Newline_Not_Allowed_After_Interface_Keyword {
  [[qljs::diag("E0275", error)]]  //
  [[qljs::message("newline is not allowed after 'interface'",
                  interface_keyword)]]  //
  Source_Code_Span interface_keyword;
};

struct Diag_Newline_Not_Allowed_After_Namespace_Keyword {
  [[qljs::diag("E0276", error)]]                                              //
  [[qljs::message("newline is not allowed after '{0}'", namespace_keyword)]]  //
  Source_Code_Span namespace_keyword;
};

struct Diag_Newline_Not_Allowed_After_Type_Keyword {
  [[qljs::diag("E0277", error)]]                                          //
  [[qljs::message("newline is not allowed after 'type'", type_keyword)]]  //
  Source_Code_Span type_keyword;
};

struct Diag_Newline_Not_Allowed_Before_Assignment_Assertion_Operator {
  [[qljs::diag("E0241", error)]]  //
  [[qljs::message("newline is not allowed between field name and '!'",
                  bang)]]                               //
  [[qljs::message("field declared here", field_name)]]  //
  Source_Code_Span bang;
  Source_Code_Span field_name;
};

struct Diag_Number_Literal_Contains_Consecutive_Underscores {
  [[qljs::diag("E0028", error)]]  //
  [[qljs::message("number literal contains consecutive underscores",
                  underscores)]]  //
  Source_Code_Span underscores;
};

struct Diag_Number_Literal_Contains_Trailing_Underscores {
  [[qljs::diag("E0029", error)]]  //
  [[qljs::message("number literal contains trailing underscore(s)",
                  underscores)]]  //
  Source_Code_Span underscores;
};

struct Diag_Octal_Literal_May_Not_Have_Exponent {
  [[qljs::diag("E0030", error)]]                                        //
  [[qljs::message("octal literal may not have exponent", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Octal_Literal_May_Not_Have_Decimal {
  [[qljs::diag("E0031", error)]]                                       //
  [[qljs::message("octal literal may not have decimal", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Object_Literal_Default_In_Expression {
  [[qljs::diag("E0253", error)]]                                         //
  [[qljs::message("use ':' instead of '=' in object literals", equal)]]  //
  Source_Code_Span equal;
};

struct Diag_Optional_Arrow_Parameter_Requires_Parentheses {
  [[qljs::diag("E0311", error)]]  //
  [[qljs::message("missing parentheses around parameter",
                  parameter_and_question)]]  //
  [[qljs::message("TypeScript optional parameter requires parentheses",
                  question)]]  //
  Source_Code_Span parameter_and_question;
  Source_Code_Span question;
};

struct Diag_Optional_Arrow_Parameter_With_Type_Annotation_Requires_Parentheses {
  [[qljs::diag("E0312", error)]]  //
  [[qljs::message("missing parentheses around parameter",
                  parameter_and_annotation)]]  //
  [[qljs::message(
      "TypeScript optional parameter with type annotation requires parentheses",
      question)]]  //
  Source_Code_Span parameter_and_annotation;
  Source_Code_Span question;
  Source_Code_Span type_colon;
};

struct Diag_Optional_Parameter_Cannot_Have_Initializer {
  [[qljs::diag("E0310", error)]]  //
  [[qljs::message(
      "optional parameter cannot have both '?' and initializer; remove '?'",
      question)]]                                      //
  [[qljs::message("initializer starts here", equal)]]  //
  Source_Code_Span equal;
  Source_Code_Span question;
};

struct Diag_Optional_Parameter_Cannot_Be_Followed_By_Required_Parameter {
  [[qljs::diag("E0379", error)]]  //
  [[qljs::message(
      "optional parameter cannot be followed by a required parameter",
      optional_parameter)]]  //
  [[qljs::message(
      "this required parameter appears after the optional parameter",
      required_parameter)]]  //
  Source_Code_Span optional_parameter;
  Source_Code_Span required_parameter;
};

struct Diag_Integer_Literal_Will_Lose_Precision {
  [[qljs::diag("E0212", warning)]]  //
  [[qljs::message("integer cannot be represented and will be rounded to '{1}'",
                  characters, rounded_val)]]  //
  Source_Code_Span characters;
  String8_View rounded_val;
};

struct Diag_Private_Properties_Are_Not_Allowed_In_Object_Literals {
  [[qljs::diag("E0156", error)]]  //
  [[qljs::message("private properties are not allowed in object literals",
                  private_identifier)]]  //
  Source_Code_Span private_identifier;
};

struct Diag_Readonly_Static_Field {
  [[qljs::diag("E0232", error)]]  //
  [[qljs::message(
      "'readonly static' is not allowed; write 'static readonly' instead",
      readonly_static)]]  //
  Source_Code_Span readonly_static;
};

struct Diag_Redeclaration_Of_Global_Variable {
  [[qljs::diag("E0033", error)]]                                        //
  [[qljs::message("redeclaration of global variable", redeclaration)]]  //
  Source_Code_Span redeclaration;
};

struct Diag_Redeclaration_Of_Variable {
  [[qljs::diag("E0034", error)]]                                             //
  [[qljs::message("redeclaration of variable: {0}", redeclaration)]]         //
  [[qljs::message("variable already declared here", original_declaration)]]  //
  Source_Code_Span redeclaration;
  Source_Code_Span original_declaration;
};

struct Diag_Redundant_Await {
  [[qljs::diag("E0266", warning)]]                        //
  [[qljs::message("redundant 'await'", await_operator)]]  //
  Source_Code_Span await_operator;
};

struct Diag_Regexp_Literal_Flags_Cannot_Contain_Unicode_Escapes {
  [[qljs::diag("E0035", error)]]  //
  [[qljs::message("RegExp literal flags cannot contain Unicode escapes",
                  escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Return_Statement_Returns_Nothing {
  [[qljs::diag("E0179", warning)]]  //
  [[qljs::message("return statement returns nothing (undefined)",
                  return_keyword)]]  //
  Source_Code_Span return_keyword;
};

struct Diag_Spread_Parameter_Cannot_Be_This {
  [[qljs::diag("E0304", error)]]                                              //
  [[qljs::message("cannot use '...' on 'this' parameter", spread_operator)]]  //
  Source_Code_Span this_keyword;
  Source_Code_Span spread_operator;
};

struct Diag_Statement_Before_First_Switch_Case {
  [[qljs::diag("E0198", error)]]  //
  // clang-format off
  [[qljs::message("unexpected statement before first switch case, expected "
                  "'case' or 'default'",
                  unexpected_statement)]]  //
  // clang-format on
  Source_Code_Span unexpected_statement;
};

struct Diag_Stray_Comma_In_Let_Statement {
  [[qljs::diag("E0036", error)]]                            //
  [[qljs::message("stray comma in let statement", where)]]  //
  Source_Code_Span where;
};

struct Diag_Stray_Comma_In_Parameter {
  [[qljs::diag("E0180", error)]]                                 //
  [[qljs::message("stray comma in function parameter", comma)]]  //
  Source_Code_Span comma;
};

struct Diag_String_Namespace_Name_Is_Only_Allowed_With_Declare_Module {
  [[qljs::diag("E0359", error)]]  //
  [[qljs::message("string module name is only allowed with 'declare module'",
                  module_name)]]  //
  Source_Code_Span module_name;
};

struct Diag_String_Namespace_Name_Is_Only_Allowed_At_Top_Level {
  [[qljs::diag("E0361", error)]]  //
  [[qljs::message("module with string name is only allowed at the top level",
                  module_name)]]  //
  Source_Code_Span module_name;
};

struct Diag_This_Parameter_Must_Be_First {
  [[qljs::diag("E0303", error)]]                                           //
  [[qljs::message("'this' must be the first parameter", this_keyword)]]    //
  [[qljs::message("first parameter starts here", first_parameter_begin)]]  //
  Source_Code_Span this_keyword;
  Source_Code_Span first_parameter_begin;
};

struct Diag_This_Parameter_Not_Allowed_In_Arrow_Functions {
  [[qljs::diag("E0301", error)]]  //
  [[qljs::message("'this' parameters are not allowed in arrow functions",
                  this_keyword)]]  //
  Source_Code_Span this_keyword;
};

struct Diag_This_Parameter_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0305", error)]]  //
  [[qljs::message("'this' parameters are not allowed in JavaScript",
                  this_keyword)]]  //
  Source_Code_Span this_keyword;
};

struct Diag_This_Parameter_Not_Allowed_When_Destructuring {
  [[qljs::diag("E0302", error)]]  //
  [[qljs::message("'this' parameter not allowed when destructuring",
                  this_keyword)]]  //
  Source_Code_Span this_keyword;
};

struct Diag_TypeScript_Abstract_Class_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0244", error)]]  //
  [[qljs::message("{0} classes are not allowed in JavaScript",
                  abstract_keyword)]]  //
  Source_Code_Span abstract_keyword;
};

struct Diag_TypeScript_Angle_Type_Assertion_Not_Allowed_In_Tsx {
  [[qljs::diag("E0283", error)]]  //
  [[qljs::message(
      "TypeScript <Type> type assertions are not allowed in JSX mode",
      bracketed_type)]]  //
  [[qljs::message("write the type assertion with 'as' here instead",
                  expected_as)]]  //
  Source_Code_Span bracketed_type;
  Source_Code_Span expected_as;
};

struct Diag_TypeScript_As_Const_With_Non_Literal_Typeable {
  [[qljs::diag("E0291", error)]]  //
  // clang-format off
  [[qljs::message("'as const' is only allowed on literals (array, object, "
                  "string, boolean) and enum members",
                  expression)]]                        //
  // clang-format on
  [[qljs::message("'as const' located here", as_const)]]  //
  Source_Code_Span expression;
  Source_Code_Span as_const;
};

struct Diag_TypeScript_As_Type_Assertion_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0281", error)]]  //
  [[qljs::message(
      "TypeScript 'as' type assertions are not allowed in JavaScript",
      as_keyword)]]  //
  Source_Code_Span as_keyword;
};

struct Diag_TypeScript_As_Or_Satisfies_Used_For_Parameter_Type_Annotation {
  [[qljs::diag("E0282", error)]]  //
  [[qljs::message("use ':' instead of '{0}' to type a function parameter",
                  bad_keyword)]]  //
  Source_Code_Span bad_keyword;
};

struct Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Declare_Class {
  [[qljs::diag("E0336", error)]]  //
  [[qljs::message(
      "assignment-asserted fields are not allowed in 'declare class'",
      bang)]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_Interfaces {
  [[qljs::diag("E0238", error)]]  //
  [[qljs::message("assignment-asserted fields are not supported in interfaces",
                  bang)]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Fields_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0239", error)]]  //
  [[qljs::message(
      "TypeScript assignment-asserted fields are not supported in JavaScript",
      bang)]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Field_Cannot_Have_Initializer {
  [[qljs::diag("E0290", error)]]  //
  [[qljs::message("assignment-assertion fields cannot have default values",
                  equal)]]                                              //
  [[qljs::message("here is the assignment assertion operator", bang)]]  //
  Source_Code_Span equal;
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Field_Must_Have_A_Type {
  [[qljs::diag("E0236", error)]]  //
  [[qljs::message("assignment-asserted field must have a type annotation",
                  bang)]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Assignment_Asserted_Method {
  [[qljs::diag("E0240", error)]]                              //
  [[qljs::message("'{0}' is not allowed on methods", bang)]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Catch_Type_Annotation_Must_Be_Any {
  [[qljs::diag("E0256", error)]]  //
  [[qljs::message(
      "catch variable can only be typed as '*', 'any', or 'unknown'",
      type_expression)]]  //
  Source_Code_Span type_expression;
};

struct Diag_TypeScript_Class_Implements_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0247", error)]]  //
  [[qljs::message("TypeScript 'implements' is not allowed in JavaScript",
                  implements_keyword)]]  //
  Source_Code_Span implements_keyword;
};

struct Diag_TypeScript_Delete_Cannot_Delete_Variables {
  [[qljs::diag("E0325", error)]]  //
  [[qljs::message("cannot delete variables in TypeScript",
                  delete_expression)]]  //
  Source_Code_Span delete_expression;
};

struct Diag_TypeScript_Enum_Auto_Member_Needs_Initializer_After_Computed {
  [[qljs::diag("E0252", error)]]                                        //
  [[qljs::message("enum member needs initializer", auto_member_name)]]  //
  [[qljs::message("computed value disables enum autoincrement",
                  computed_expression)]]  //
  Source_Code_Span auto_member_name;
  Source_Code_Span computed_expression;
};

struct Diag_TypeScript_Enum_Computed_Name_Must_Be_Simple {
  [[qljs::diag("E0249", error)]]  //
  [[qljs::message("computed enum member name must be a simple string",
                  expression)]]  //
  Source_Code_Span expression;
};

struct Diag_TypeScript_Enum_Is_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0127", error)]]  //
  [[qljs::message("TypeScript's 'enum' feature is not allowed in JavaScript",
                  enum_keyword)]]  //
  Source_Code_Span enum_keyword;
};

struct Diag_TypeScript_Enum_Member_Name_Cannot_Be_Number {
  [[qljs::diag("E0250", error)]]                                   //
  [[qljs::message("enum member name cannot be numeric", number)]]  //
  Source_Code_Span number;
};

struct Diag_TypeScript_Enum_Value_Must_Be_Constant {
  [[qljs::diag("E0251", error)]]  //
  [[qljs::message("{1:headlinese} value must be a compile-time constant",
                  expression, declared_enum_kind)]]  //
  Source_Code_Span expression;
  Enum_Kind declared_enum_kind;
};

struct Diag_TypeScript_Export_Equal_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0370", error)]]  //
  // clang-format off
  [[qljs::message("'export =' is not allowed; write 'export default' or "
                  "'module.exports =' (CommonJS) instead",
                  equal)]]                                 //
  // clang-format on
  [[qljs::message("'export' keyword here", export_keyword)]]  //
  Source_Code_Span equal;
  Source_Code_Span export_keyword;
};

struct Diag_TypeScript_Implements_Must_Be_After_Extends {
  [[qljs::diag("E0246", error)]]  //
  [[qljs::message("'extends' must be before 'implements'",
                  extends_keyword)]]  //
  [[qljs::message("move the 'extends' clause before 'implements' here",
                  implements_keyword)]]  //
  Source_Code_Span implements_keyword;
  Source_Code_Span extends_keyword;
};

struct Diag_TypeScript_Import_Alias_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0274", error)]]  //
  [[qljs::message("TypeScript import aliases are not allowed in JavaScript",
                  equal)]]                                                  //
  [[qljs::message("write 'const' instead of '{0}' here", import_keyword)]]  //
  Source_Code_Span import_keyword;
  Source_Code_Span equal;
};

struct Diag_TypeScript_Index_Signature_Cannot_Be_Method {
  [[qljs::diag("E0227", error)]]  //
  [[qljs::message("index signature must be a field, not a method",
                  left_paren)]]  //
  Source_Code_Span left_paren;
};

struct Diag_TypeScript_Index_Signature_Needs_Type {
  [[qljs::diag("E0225", error)]]                                             //
  [[qljs::message("index signatures require a value type", expected_type)]]  //
  Source_Code_Span expected_type;
};

struct Diag_TypeScript_Infer_Outside_Conditional_Type {
  [[qljs::diag("E0367", error)]]  //
  [[qljs::message(
      "'infer' is only allowed between 'extends' and '?' in conditional types",
      infer_keyword)]]  //
  Source_Code_Span infer_keyword;
};

struct Diag_TypeScript_Infer_Requires_Parentheses {
  [[qljs::diag("E0366", error)]]  //
  [[qljs::message("parentheses are required around 'infer {1}'", infer_and_type,
                  type)]]  //
  Source_Code_Span infer_and_type;
  Source_Code_Span type;
};

struct Diag_TypeScript_Function_Overload_Signature_Must_Have_Same_Name {
  [[qljs::diag("E0316", error)]]  //
  [[qljs::message("function overload signature must be named '{1}'",
                  overload_name, function_name)]]  //
  [[qljs::message("overloaded function '{0}' declared here",
                  function_name)]]  //
  Source_Code_Span overload_name;
  Source_Code_Span function_name;
};

struct
    Diag_TypeScript_Function_Overload_Signature_Must_Not_Have_Generator_Star {
  [[qljs::diag("E0318", error)]]  //
  [[qljs::message("function overload signature cannot have generator '*'",
                  generator_star)]]  //
  Source_Code_Span generator_star;
};

struct Diag_TypeScript_Generic_Arrow_Needs_Comma_In_JSX_Mode {
  [[qljs::diag("E0285", error)]]  //
  [[qljs::message("generic arrow function needs ',' here in TSX",
                  expected_comma)]]  //
  Source_Code_Span generic_parameters_less;
  Source_Code_Span expected_comma;
  Source_Code_Span arrow;
};

struct Diag_TypeScript_Generic_Parameter_List_Is_Empty {
  [[qljs::diag("E0264", error)]]  //
  [[qljs::message("expected at least one parameter in generic parameter list",
                  expected_parameter)]]  //
  Source_Code_Span expected_parameter;
};

struct Diag_TypeScript_Generics_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0233", error)]]  //
  [[qljs::message("TypeScript generics are not allowed in JavaScript code",
                  opening_less)]]  //
  Source_Code_Span opening_less;
};

struct Diag_TypeScript_Type_Export_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0278", error)]]  //
  [[qljs::message("TypeScript type exports are not allowed in JavaScript",
                  type_keyword)]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Inline_Type_Export_Not_Allowed_In_Type_Only_Export {
  [[qljs::diag("E0280", error)]]  //
  [[qljs::message("'type' cannot be used twice in export",
                  inline_type_keyword)]]                      //
  [[qljs::message("remove this 'type'", type_only_keyword)]]  //
  Source_Code_Span inline_type_keyword;
  Source_Code_Span type_only_keyword;
};

struct Diag_TypeScript_Inline_Type_Import_Not_Allowed_In_Type_Only_Import {
  [[qljs::diag("E0272", error)]]  //
  [[qljs::message("'type' cannot be used twice in import",
                  inline_type_keyword)]]                      //
  [[qljs::message("remove this 'type'", type_only_keyword)]]  //
  Source_Code_Span inline_type_keyword;
  Source_Code_Span type_only_keyword;
};

struct Diag_TypeScript_Interfaces_Cannot_Contain_Static_Blocks {
  [[qljs::diag("E0243", error)]]                                              //
  [[qljs::message("interfaces cannot contain static blocks", static_token)]]  //
  Source_Code_Span static_token;
};

struct Diag_TypeScript_Declare_Class_Cannot_Contain_Static_Block_Statement {
  [[qljs::diag("E0332", error)]]  //
  [[qljs::message("'declare class' cannot contain static block",
                  static_token)]]  //
  Source_Code_Span static_token;
};

struct Diag_TypeScript_Interfaces_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0213", error)]]  //
  [[qljs::message(
      "TypeScript's 'interface' feature is not allowed in JavaScript code",
      interface_keyword)]]  //
  Source_Code_Span interface_keyword;
};

struct Diag_TypeScript_Missing_Name_And_Colon_In_Named_Tuple_Type {
  [[qljs::diag("E0319", error)]]  //
  [[qljs::message("missing name for element in named tuple type",
                  expected_name_and_colon)]]  //
  // clang-format off
  [[qljs::message("this tuple type is a named tuple type because at least "
                  "one element has a name",
                  existing_name)]]  //
  // clang-format on
  Source_Code_Span expected_name_and_colon;
  Source_Code_Span existing_name;
};

struct Diag_TypeScript_Missing_Name_In_Named_Tuple_Type {
  [[qljs::diag("E0320", error)]]                                            //
  [[qljs::message("missing name for element in named tuple type", colon)]]  //
  Source_Code_Span colon;
};

struct Diag_TypeScript_Named_Tuple_Element_Question_After_Name_And_Type {
  [[qljs::diag("E0322", error)]]  //
  [[qljs::message(
      "'?' belongs only after the tuple element name, not also after the type",
      type_question)]]  //
  Source_Code_Span type_question;
  Source_Code_Span name_question;
};

struct Diag_TypeScript_Named_Tuple_Element_Question_After_Type {
  [[qljs::diag("E0314", error)]]  //
  [[qljs::message(
      "'?' belongs after the tuple element name, not after the type",
      question)]]                                        //
  [[qljs::message("'?' goes here", expected_question)]]  //
  Source_Code_Span question;
  Source_Code_Span expected_question;
};

struct Diag_TypeScript_Named_Tuple_Element_Spread_Before_Name_And_Type {
  [[qljs::diag("E0329", error)]]  //
  // clang-format off
  [[qljs::message("'...' belongs only before the tuple element name, not "
                  "also before the type",
                  type_spread)]]  //
  // clang-format on
  Source_Code_Span type_spread;
  Source_Code_Span name_spread;
};

struct Diag_TypeScript_Named_Tuple_Element_Spread_Before_Type {
  [[qljs::diag("E0328", error)]]  //
  [[qljs::message(
      "'...' belongs before the tuple element name, not before the type",
      spread)]]                                          //
  [[qljs::message("'...' goes here", expected_spread)]]  //
  Source_Code_Span spread;
  Source_Code_Span expected_spread;
};

struct Diag_TypeScript_Namespace_Cannot_Export_Default {
  [[qljs::diag("E0363", error)]]  //
  [[qljs::message("cannot 'export default' from inside a namespace",
                  default_keyword)]]                             //
  [[qljs::message("namespace starts here", namespace_keyword)]]  //
  Source_Code_Span default_keyword;
  Source_Code_Span namespace_keyword;
};

struct Diag_TypeScript_Namespaces_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0273", error)]]  //
  [[qljs::message("TypeScript namespaces are not allowed in JavaScript",
                  namespace_keyword)]]  //
  Source_Code_Span namespace_keyword;
};

struct Diag_TypeScript_Non_Null_Assertion_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0261", error)]]  //
  [[qljs::message(
      "TypeScript non-null assertions are not allowed in JavaScript",
      bang)]]  //
  Source_Code_Span bang;
};

struct Diag_TypeScript_Optional_Parameters_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0308", error)]]  //
  [[qljs::message(
      "TypeScript optional parameters are not allowed in JavaScript",
      question)]]  //
  Source_Code_Span question;
};

struct Diag_TypeScript_Optional_Properties_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0228", error)]]  //
  [[qljs::message(
      "TypeScript optional properties are not allowed in JavaScript code",
      question)]]  //
  Source_Code_Span question;
};

struct Diag_TypeScript_Optional_Tuple_Element_Cannot_Follow_Spread_Element {
  [[qljs::diag("E0323", error)]]  //
  [[qljs::message("optional tuple elements cannot come after spread elements",
                  optional_question)]]                                //
  [[qljs::message("prior spread element is here", previous_spread)]]  //
  Source_Code_Span optional_question;
  Source_Code_Span previous_spread;
};

struct Diag_TypeScript_Parameter_Property_Cannot_Be_Destructured {
  [[qljs::diag("E0372", error)]]  //
  [[qljs::message("parameter properties cannot be destructured",
                  destructure_token)]]                                       //
  [[qljs::message("property declared using '{0}' here", property_keyword)]]  //
  Source_Code_Span destructure_token;
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Parameter_Property_Cannot_Be_Rest {
  [[qljs::diag("E0377", error)]]  //
  [[qljs::message("parameter properties cannot be a rest parameter",
                  spread)]]                                                  //
  [[qljs::message("property declared using '{0}' here", property_keyword)]]  //
  Source_Code_Span spread;
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Parameter_Property_Not_Allowed_In_Declare_Class {
  [[qljs::diag("E0375", error)]]  //
  [[qljs::message("parameter properties are not allowed in 'declare class'",
                  property_keyword)]]                             //
  [[qljs::message("'declare' specified here", declare_keyword)]]  //
  Source_Code_Span property_keyword;
  Source_Code_Span declare_keyword;
};

struct Diag_TypeScript_Parameter_Property_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0371", error)]]  //
  [[qljs::message(
      "TypeScript parameter properties are not allowed in JavaScript",
      property_keyword)]]  //
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Parameter_Property_Only_Allowed_In_Class_Constructor {
  [[qljs::diag("E0378", error)]]  //
  [[qljs::message("parameter properties are only allowed in class constructors",
                  property_keyword)]]  //
  Source_Code_Span property_keyword;
};

struct Diag_TypeScript_Private_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0222", error)]]                                          //
  [[qljs::message("'private' is not allowed in JavaScript", specifier)]]  //
  Source_Code_Span specifier;
};

struct Diag_TypeScript_Protected_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0234", error)]]                                            //
  [[qljs::message("'protected' is not allowed in JavaScript", specifier)]]  //
  Source_Code_Span specifier;
};

struct Diag_TypeScript_Public_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0289", error)]]                                         //
  [[qljs::message("'public' is not allowed in JavaScript", specifier)]]  //
  Source_Code_Span specifier;
};

struct Diag_TypeScript_Readonly_Fields_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0230", error)]]  //
  [[qljs::message(
      "TypeScript's 'readonly' feature is not allowed in JavaScript code",
      readonly_keyword)]]  //
  Source_Code_Span readonly_keyword;
};

struct Diag_TypeScript_Readonly_Method {
  [[qljs::diag("E0231", error)]]                                     //
  [[qljs::message("methods cannot be readonly", readonly_keyword)]]  //
  Source_Code_Span readonly_keyword;
};

struct Diag_TypeScript_Readonly_In_Type_Needs_Array_Or_Tuple_Type {
  [[qljs::diag("E0313", error)]]  //
  [[qljs::message("'readonly' only works with array types and tuple types",
                  readonly_keyword)]]  //
  Source_Code_Span readonly_keyword;
};

struct Diag_TypeScript_Required_Tuple_Element_After_Optional_Element {
  [[qljs::diag("E0321", error)]]  //
  [[qljs::message("expected '?' to mark tuple element as optional",
                  expected_question)]]  //
  [[qljs::message(
      "only optional tuple elements can follow this optional tuple element",
      previous_optional_question)]]  //
  Source_Code_Span expected_question;
  Source_Code_Span previous_optional_question;
};

struct Diag_TypeScript_Requires_Space_Between_Greater_And_Equal {
  [[qljs::diag("E0365", error)]]  //
  [[qljs::message("TypeScript requires whitespace between '>' and '=' here",
                  greater_equal)]]  //
  Source_Code_Span greater_equal;
};

struct Diag_TypeScript_Satisfies_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0364", error)]]  //
  [[qljs::message(
      "TypeScript 'satisfies' operator is not allowed in JavaScript",
      satisfies_keyword)]]  //
  Source_Code_Span satisfies_keyword;
};

struct Diag_TypeScript_Type_Annotation_In_Expression {
  [[qljs::diag("E0254", error)]]  //
  [[qljs::message("unexpected ':' in expression; did you mean 'as'?",
                  type_colon)]]  //
  Source_Code_Span type_colon;
};

struct Diag_TypeScript_Type_Annotations_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0224", error)]]  //
  [[qljs::message(
      "TypeScript type annotations are not allowed in JavaScript code",
      type_colon)]]  //
  Source_Code_Span type_colon;
};

struct Diag_TypeScript_Type_Alias_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0267", error)]]  //
  [[qljs::message("TypeScript types are not allowed in JavaScript",
                  type_keyword)]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Type_Only_Import_Cannot_Import_Default_And_Named {
  [[qljs::diag("E0268", error)]]  //
  [[qljs::message(
      "TypeScript type imports cannot import both default and named exports",
      type_keyword)]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Type_Import_Not_Allowed_In_JavaScript {
  [[qljs::diag("E0270", error)]]  //
  [[qljs::message("TypeScript type imports are not allowed in JavaScript",
                  type_keyword)]]  //
  Source_Code_Span type_keyword;
};

struct Diag_TypeScript_Spread_Element_Cannot_Be_Optional {
  [[qljs::diag("E0324", error)]]  //
  [[qljs::message("spread tuple elements cannot be optional",
                  optional_question)]]             //
  [[qljs::message("spread starts here", spread)]]  //
  Source_Code_Span optional_question;
  Source_Code_Span spread;
};

struct Diag_TypeScript_Style_Const_Field {
  [[qljs::diag("E0165", error)]]  //
  // clang-format off
  [[qljs::message("const fields within classes are only allowed in "
                  "TypeScript, not JavaScript",
                  const_token)]]  //
  // clang-format on
  Source_Code_Span const_token;
};

struct Diag_TypeScript_Variance_Keywords_In_Wrong_Order {
  [[qljs::diag("E0368", error)]]  //
  [[qljs::message("'out in' is not allowed; write 'in out' instead",
                  in_keyword)]]  //
  Source_Code_Span in_keyword;
  Source_Code_Span out_keyword;
};

struct Diag_Unclosed_Block_Comment {
  [[qljs::diag("E0037", error)]]                             //
  [[qljs::message("unclosed block comment", comment_open)]]  //
  Source_Code_Span comment_open;
};

struct Diag_Unclosed_Class_Block {
  [[qljs::diag("E0199", error)]]  //
  [[qljs::message("unclosed class; expected '}' by end of file",
                  block_open)]]  //
  Source_Code_Span block_open;
};

struct Diag_Unclosed_Code_Block {
  [[qljs::diag("E0134", error)]]  //
  [[qljs::message("unclosed code block; expected '}' by end of file",
                  block_open)]]  //
  Source_Code_Span block_open;
};

struct Diag_Unclosed_Interface_Block {
  [[qljs::diag("E0215", error)]]  //
  [[qljs::message("unclosed interface; expected '}' by end of file",
                  block_open)]]  //
  Source_Code_Span block_open;
};

struct Diag_Unclosed_Identifier_Escape_Sequence {
  [[qljs::diag("E0038", error)]]                                             //
  [[qljs::message("unclosed identifier escape sequence", escape_sequence)]]  //
  Source_Code_Span escape_sequence;
};

struct Diag_Unclosed_Object_Literal {
  [[qljs::diag("E0161", error)]]  //
  [[qljs::message("unclosed object literal; expected '}'",
                  expected_object_close)]]                       //
  [[qljs::message("object literal started here", object_open)]]  //
  Source_Code_Span object_open;
  Source_Code_Span expected_object_close;
};

struct Diag_Unclosed_Regexp_Literal {
  [[qljs::diag("E0039", error)]]                                //
  [[qljs::message("unclosed regexp literal", regexp_literal)]]  //
  Source_Code_Span regexp_literal;
};

struct Diag_Unclosed_String_Literal {
  [[qljs::diag("E0040", error)]]                                //
  [[qljs::message("unclosed string literal", string_literal)]]  //
  Source_Code_Span string_literal;
};

struct Diag_Unclosed_JSX_String_Literal {
  [[qljs::diag("E0181", error)]]                                      //
  [[qljs::message("unclosed string literal", string_literal_begin)]]  //
  Source_Code_Span string_literal_begin;
};

struct Diag_Unclosed_Template {
  [[qljs::diag("E0041", error)]]                               //
  [[qljs::message("unclosed template", incomplete_template)]]  //
  Source_Code_Span incomplete_template;
};

struct Diag_Unexpected_At_Character {
  [[qljs::diag("E0042", error)]]                  //
  [[qljs::message("unexpected '@'", character)]]  //
  Source_Code_Span character;
};

struct Diag_Unexpected_Arrow_After_Expression {
  [[qljs::diag("E0160", error)]]                //
  [[qljs::message("unexpected '{0}'", arrow)]]  //
  [[qljs::message(
      "expected parameter for arrow function, but got an expression instead",
      expression)]]  //
  Source_Code_Span arrow;
  Source_Code_Span expression;
};

struct Diag_Unexpected_Arrow_After_Literal {
  [[qljs::diag("E0158", error)]]                //
  [[qljs::message("unexpected '{0}'", arrow)]]  //
  [[qljs::message(
      "expected parameter for arrow function, but got a literal instead",
      literal_parameter)]]  //
  Source_Code_Span arrow;
  Source_Code_Span literal_parameter;
};

struct Diag_Unexpected_Backslash_In_Identifier {
  [[qljs::diag("E0043", error)]]                                 //
  [[qljs::message("unexpected '\\' in identifier", backslash)]]  //
  Source_Code_Span backslash;
};

struct Diag_Unexpected_Case_Outside_Switch_Statement {
  [[qljs::diag("E0115", error)]]  //
  [[qljs::message("unexpected 'case' outside switch statement",
                  case_token)]]  //
  Source_Code_Span case_token;
};

struct Diag_Unexpected_Characters_In_Number {
  [[qljs::diag("E0044", error)]]                                            //
  [[qljs::message("unexpected characters in number literal", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Control_Character {
  [[qljs::diag("E0045", error)]]                                //
  [[qljs::message("unexpected control character", character)]]  //
  Source_Code_Span character;
};

struct Diag_Unexpected_Characters_In_Binary_Number {
  [[qljs::diag("E0046", error)]]                                            //
  [[qljs::message("unexpected characters in binary literal", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Characters_In_Octal_Number {
  [[qljs::diag("E0047", error)]]                                           //
  [[qljs::message("unexpected characters in octal literal", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Characters_In_Hex_Number {
  [[qljs::diag("E0048", error)]]                                         //
  [[qljs::message("unexpected characters in hex literal", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Unexpected_Default_Outside_Switch_Statement {
  [[qljs::diag("E0116", error)]]  //
  [[qljs::message("unexpected 'default' outside switch statement",
                  default_token)]]  //
  Source_Code_Span default_token;
};

struct Diag_Unexpected_Greater_In_JSX_Text {
  [[qljs::diag("E0182", error)]]  //
  [[qljs::message(
      "'>' is not allowed directly in JSX text; write {{'>'} or &gt; instead",
      greater)]]  //
  Source_Code_Span greater;
};

struct Diag_Unexpected_Literal_In_Parameter_List {
  [[qljs::diag("E0159", error)]]  //
  [[qljs::message(
      "unexpected literal in parameter list; expected parameter name",
      literal)]]  //
  Source_Code_Span literal;
};

struct Diag_Unexpected_Right_Curly_In_JSX_Text {
  [[qljs::diag("E0183", error)]]  //
  [[qljs::message(
      "'}' is not allowed directly in JSX text; write {{'}'} instead",
      right_curly)]]  //
  Source_Code_Span right_curly;
};

struct Diag_Unexpected_Question_In_Expression {
  [[qljs::diag("E0307", error)]]                 //
  [[qljs::message("unexpected '?'", question)]]  //
  Source_Code_Span question;
};

struct Diag_Unexpected_Question_When_Destructuring {
  [[qljs::diag("E0309", error)]]                                    //
  [[qljs::message("unexpected '?' when destructuring", question)]]  //
  Source_Code_Span question;
};

struct Diag_Unexpected_Semicolon_In_C_Style_For_Loop {
  [[qljs::diag("E0102", error)]]  //
  [[qljs::message(
      "C-style for loops have only three semicolon-separated components",
      semicolon)]]  //
  Source_Code_Span semicolon;
};

struct Diag_Unexpected_Semicolon_In_For_In_Loop {
  [[qljs::diag("E0110", error)]]  //
  [[qljs::message("for-in loop expression cannot have semicolons",
                  semicolon)]]  //
  Source_Code_Span semicolon;
};

struct Diag_Unexpected_Semicolon_In_For_Of_Loop {
  [[qljs::diag("E0109", error)]]  //
  [[qljs::message("for-of loop expression cannot have semicolons",
                  semicolon)]]  //
  Source_Code_Span semicolon;
};

struct Diag_Unopened_Block_Comment {
  [[qljs::diag("E0210", error)]]                              //
  [[qljs::message("unopened block comment", comment_close)]]  //
  Source_Code_Span comment_close;
};

struct Diag_Unused_Variable_Shadows {
  [[qljs::diag("E0196", warning)]]  //
  [[qljs::message("new variable shadows existing variable",
                  shadowing_declaration)]]                                    //
  [[qljs::message("existing variable declared here", shadowed_declaration)]]  //
  Source_Code_Span shadowing_declaration;
  Source_Code_Span shadowed_declaration;
};

struct Diag_No_Digits_In_Binary_Number {
  [[qljs::diag("E0049", error)]]                                        //
  [[qljs::message("binary number literal has no digits", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_No_Digits_In_Hex_Number {
  [[qljs::diag("E0050", error)]]                                     //
  [[qljs::message("hex number literal has no digits", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_No_Digits_In_Octal_Number {
  [[qljs::diag("E0051", error)]]                                       //
  [[qljs::message("octal number literal has no digits", characters)]]  //
  Source_Code_Span characters;
};

struct Diag_Non_Null_Assertion_Not_Allowed_In_Parameter {
  [[qljs::diag("E0260", error)]]  //
  [[qljs::message("TypeScript non-null assertion is not allowed on parameters",
                  bang)]]  //
  Source_Code_Span bang;
};

struct Diag_Unexpected_Hash_Character {
  [[qljs::diag("E0052", error)]]              //
  [[qljs::message("unexpected '#'", where)]]  //
  Source_Code_Span where;
};

struct Diag_Unexpected_Bom_Before_Shebang {
  [[qljs::diag("E0095", error)]]  //
  // clang-format off
  [[qljs::message("unicode byte order mark (BOM) cannot appear before #! at "
                  "beginning of script",
                  bom)]]  //
  // clang-format on
  Source_Code_Span bom;
};

struct Diag_Unexpected_Identifier_In_Expression {
  [[qljs::diag("E0147", error)]]  //
  [[qljs::message(
      "unexpected identifier in expression; missing operator before",
      unexpected)]]  //
  Source_Code_Span unexpected;
};

// NOTE(strager): Try not to use this error. Find or make a more descriptive
// and helpful error instead.
struct Diag_Unexpected_Token {
  [[qljs::diag("E0054", error)]]                //
  [[qljs::message("unexpected token", token)]]  //
  Source_Code_Span token;
};

struct Diag_Unexpected_Token_After_Export {
  [[qljs::diag("E0112", error)]]  //
  // clang-format off
  [[qljs::message("unexpected token in export; expected 'export default "
                  "...' or 'export {{name}' or 'export * from ...' or "
                  "'export class' or 'export function' or 'export let'",
                  unexpected_token)]]  //
  // clang-format on
  Source_Code_Span unexpected_token;
};

struct Diag_Unexpected_Token_In_Variable_Declaration {
  [[qljs::diag("E0114", error)]]  //
  [[qljs::message(
      "unexpected token in variable declaration; expected variable name",
      unexpected_token)]]  //
  Source_Code_Span unexpected_token;
};

struct Diag_Unmatched_Indexing_Bracket {
  [[qljs::diag("E0055", error)]]                                //
  [[qljs::message("unmatched indexing bracket", left_square)]]  //
  Source_Code_Span left_square;
};

struct Diag_Unmatched_Parenthesis {
  [[qljs::diag("E0056", error)]]                     //
  [[qljs::message("unmatched parenthesis", where)]]  //
  Source_Code_Span where;
};

struct Diag_Unmatched_Right_Curly {
  [[qljs::diag("E0143", error)]]                   //
  [[qljs::message("unmatched '}'", right_curly)]]  //
  Source_Code_Span right_curly;
};

struct Diag_Use_Of_Undeclared_Parameter_In_Type_Predicate {
  [[qljs::diag("E0315", error)]]                                 //
  [[qljs::message("{0} is not the name of a parameter", name)]]  //
  Source_Code_Span name;
};

struct Diag_Use_Of_Undeclared_Type {
  [[qljs::diag("E0214", warning)]]                        //
  [[qljs::message("use of undeclared type: {0}", name)]]  //
  Source_Code_Span name;
};

struct Diag_Use_Of_Undeclared_Variable {
  [[qljs::diag("E0057", warning)]]                            //
  [[qljs::message("use of undeclared variable: {0}", name)]]  //
  Source_Code_Span name;
};

struct Diag_Variable_Used_Before_Declaration {
  [[qljs::diag("E0058", error)]]                                   //
  [[qljs::message("variable used before declaration: {0}", use)]]  //
  [[qljs::message("variable declared here", declaration)]]         //
  Source_Code_Span use;
  Source_Code_Span declaration;
};

struct Diag_Function_Call_Before_Declaration_In_Block_Scope {
  [[qljs::diag("E0077", warning)]]  //
  [[qljs::message("function called before declaration in block scope: {0}",
                  use)]]                                    //
  [[qljs::message("function declared here", declaration)]]  //
  Source_Code_Span use;
  Source_Code_Span declaration;
};

struct Diag_Import_Cannot_Have_Declare_Keyword {
  [[qljs::diag("E0360", error)]]  //
  [[qljs::message("cannot use 'declare' keyword with 'import'",
                  declare_keyword)]]  //
  Source_Code_Span declare_keyword;
};

struct Diag_Interface_Fields_Cannot_Have_Initializers {
  [[qljs::diag("E0221", error)]]  //
  [[qljs::message("TypeScript interface fields cannot be initalized",
                  equal)]]  //
  Source_Code_Span equal;
};

struct Diag_Interface_Methods_Cannot_Be_Async {
  [[qljs::diag("E0217", error)]]  //
  [[qljs::message("TypeScript interface methods cannot be marked 'async'",
                  async_keyword)]]  //
  Source_Code_Span async_keyword;
};

struct Diag_Interface_Methods_Cannot_Be_Generators {
  [[qljs::diag("E0218", error)]]  //
  [[qljs::message(
      "TypeScript interface methods cannot be marked as a generator",
      star)]]  //
  Source_Code_Span star;
};

struct Diag_Interface_Methods_Cannot_Contain_Bodies {
  [[qljs::diag("E0220", error)]]  //
  [[qljs::message("TypeScript interface methods cannot contain a body",
                  body_start)]]  //
  Source_Code_Span body_start;
};

struct Diag_Interface_Properties_Cannot_Be_Explicitly_Public {
  [[qljs::diag("E0237", error)]]  //
  [[qljs::message("interface properties cannot be marked public explicitly",
                  public_keyword)]]  //
  Source_Code_Span public_keyword;
};

struct Diag_Interface_Properties_Cannot_Be_Private {
  [[qljs::diag("E0219", error)]]  //
  [[qljs::message(
      "interface properties are always public and cannot be private",
      property_name_or_private_keyword)]]  //
  Source_Code_Span property_name_or_private_keyword;
};

struct Diag_Interface_Properties_Cannot_Be_Protected {
  [[qljs::diag("E0288", error)]]  //
  // clang-format off
  [[qljs::message("TypeScript interface properties are always public and "
                  "cannot be marked protected",
                  protected_keyword)]]  //
  // clang-format on
  Source_Code_Span protected_keyword;
};

struct Diag_Interface_Properties_Cannot_Be_Static {
  [[qljs::diag("E0216", error)]]  //
  [[qljs::message("TypeScript interface properties cannot be 'static'",
                  static_keyword)]]  //
  Source_Code_Span static_keyword;
};

struct Diag_Invalid_Break {
  [[qljs::diag("E0200", error)]]  //
  [[qljs::message("break can only be used inside of a loop or switch",
                  break_statement)]]  //
  Source_Code_Span break_statement;
};

struct Diag_Invalid_Continue {
  [[qljs::diag("E0201", error)]]  //
  [[qljs::message("continue can only be used inside of a loop",
                  continue_statement)]]  //
  Source_Code_Span continue_statement;
};

struct Diag_Pointless_String_Comp_Contains_Lower {
  [[qljs::diag("E0286", warning)]]  //
  [[qljs::message("lower case letters compared with toUpperCase",
                  span_operator)]]  //
  Source_Code_Span span_operator;
};

struct Diag_Pointless_String_Comp_Contains_Upper {
  [[qljs::diag("E0287", warning)]]  //
  [[qljs::message("upper case letters compared with toLowerCase",
                  span_operator)]]  //
  Source_Code_Span span_operator;
};

struct Diag_Pointless_Strict_Comp_Against_Array_Literal {
  [[qljs::diag("E0341", warning)]]  //
  [[qljs::message("using '{0}' against an array literal does not compare items",
                  equals_operator)]]  //
  Source_Code_Span equals_operator;
};

struct Diag_Pointless_Comp_Against_Arrow_Function {
  [[qljs::diag("E0342", warning)]]  //
  [[qljs::message("using '{0}' against an arrow function always returns '{1}'",
                  equals_operator, comparison_result)]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Comp_Against_Class_Literal {
  [[qljs::diag("E0343", warning)]]  //
  [[qljs::message("using '{0}' against a class literal always returns '{1}'",
                  equals_operator, comparison_result)]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Strict_Comp_Against_Empty_Array_Literal {
  [[qljs::diag("E0344", warning)]]  //
  [[qljs::message("'{0} []' is always '{1}'", equals_operator,
                  comparison_result)]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Comp_Against_Object_Literal {
  [[qljs::diag("E0345", warning)]]  //
  [[qljs::message("using '{0}' against an object literal always returns '{1}'",
                  equals_operator, comparison_result)]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Pointless_Comp_Against_Regular_Expression_Literal {
  [[qljs::diag("E0346", warning)]]  //
  [[qljs::message(
      "using '{0}' against a regular expression literal always returns '{1}'",
      equals_operator, comparison_result)]]  //
  Source_Code_Span equals_operator;
  String8_View comparison_result;
};

struct Diag_Unexpected_Function_Parameter_Is_Parenthesized {
  [[qljs::diag("E0349", error)]]  //
  [[qljs::message("function parameter cannot be parenthesized",
                  left_paren_to_right_paren)]]  //
  Source_Code_Span left_paren_to_right_paren;
};

struct Diag_Unexpected_Comma_After_Field_Initialization {
  [[qljs::diag("E0330", error)]]                         //
  [[qljs::message("',' should be ';' instead", comma)]]  //
  Source_Code_Span comma;
};

struct Diag_Unexpected_Colon_After_Generic_Definition {
  [[qljs::diag("E0331", error)]]                               //
  [[qljs::message("':' should be 'extends' instead", colon)]]  //
  Source_Code_Span colon;
};

struct Diag_Pointless_Nullish_Coalescing_Operator {
  [[qljs::diag("E0369", warning)]]  //
  // clang-format off
  [[qljs::message("nullish coalescing operator does nothing when left "
                  "operand is never null",
                  question_question)]]  //
  // clang-format on
  Source_Code_Span question_question;
};

struct Diag_Bang_Equal_Equal_Interpreted_As_Non_Null_Assertion {
  [[qljs::diag("E0373", warning)]]  //
  [[qljs::message("unexpected whitespace between '!' and '=='",
                  unexpected_space)]]  //
  [[qljs::message(
      "'!' here treated as the TypeScript non-null assertion operator",
      bang)]]  //
  Source_Code_Span unexpected_space;
  Source_Code_Span bang;
};

struct Diag_Unexpected_Space_Between_Bang_And_Equal_Equal {
  [[qljs::diag("E0374", error)]]  //
  [[qljs::message("unexpected whitespace between '!' and '=='",
                  unexpected_space)]]  //
  Source_Code_Span unexpected_space;
};

struct Diag_JSX_Prop_Is_Missing_Expression {
  [[qljs::diag("E0376", error)]]  //
  [[qljs::message("JSX prop is missing an expression",
                  left_brace_to_right_brace)]]  //
  Source_Code_Span left_brace_to_right_brace;
};

struct Diag_Keyword_Contains_Escape_Characters {
  [[qljs::diag("E0381", error)]]  //
  [[qljs::message("Keywords in TypeScript does not allow escape characters",
                  escape_character_in_keyword)]]  //
  Source_Code_Span escape_character_in_keyword;
};

struct Diag_Access_Specifier_Must_Precede_Other_Modifiers {
  [[qljs::diag("E0380", error)]]  //
  [[qljs::message("'{0}' access specifier must precede '{1}'", second_modifier,
                  first_modifier)]]  //
  Source_Code_Span second_modifier;
  Source_Code_Span first_modifier;
};

struct Diag_Spread_Must_Precede_Expression {
  [[qljs::diag("E0708", error)]]                                      //
  [[qljs::message("unexpected '...'; expected expression", spread)]]  //
  Source_Code_Span spread;
};

struct Diag_Spread_Must_Precede_Variable_Name {
  [[qljs::diag("E0709", error)]]                                   //
  [[qljs::message("expected variable name after '...'", spread)]]  //
  Source_Code_Span spread;
};

struct Diag_Variable_Assigned_To_Self_Is_Noop {
  [[qljs::diag("E0383", warning)]]  //
  [[qljs::message("variable assignment to self is no-op",
                  assignment_statement)]]  //
  Source_Code_Span assignment_statement;
};
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
