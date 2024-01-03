// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#pragma once

#include <array>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/container/winkable.h>
#include <quick-lint-js/fe/buffering-visitor.h>
#include <quick-lint-js/fe/lex.h>
#include <quick-lint-js/fe/parse-visitor.h>
#include <quick-lint-js/fe/source-code-span.h>
#include <quick-lint-js/fe/token.h>
#include <quick-lint-js/port/char8.h>
#include <quick-lint-js/port/span.h>
#include <quick-lint-js/port/unreachable.h>
#include <quick-lint-js/port/warning.h>
#include <quick-lint-js/util/cast.h>
#include <type_traits>
#include <utility>
#include <vector>

#define QLJS_UNEXPECTED_EXPRESSION_KIND()                                      \
  do {                                                                         \
    QLJS_ASSERT(false && "function not implemented for this expression kind"); \
    QLJS_UNREACHABLE();                                                        \
  } while (false)

QLJS_WARNING_PUSH
QLJS_WARNING_IGNORE_CLANG("-Wshadow-field")

namespace quick_lint_js {
class Expression;

// NOTE(strager): Enum members in Expression_Kind are Upper_Snake_Case (matching
// the type names) instead of the usual lower_snake_case.
enum class Expression_Kind {
  Class,
  Delete,
  Invalid,
  Missing,
  New,
  Template,
  Typeof,
  Array,
  Arrow_Function,
  Angle_Type_Assertion,  // TypeScript only.
  As_Type_Assertion,     // TypeScript only.
  Assignment,
  Await,
  Binary_Operator,
  Call,
  Compound_Assignment,
  Conditional,
  Conditional_Assignment,
  Dot,
  Function,
  Import,
  Index,
  JSX_Element,
  JSX_Element_With_Members,
  JSX_Element_With_Namespace,
  JSX_Fragment,
  Literal,
  Named_Function,
  New_Target,
  Non_Null_Assertion,  // TypeScript only.
  Object,
  Optional,  // TypeScript only.
  Paren,
  Paren_Empty,
  Private_Variable,
  RW_Unary_Prefix,
  RW_Unary_Suffix,
  Satisfies,  // TypeScript only.
  Spread,
  Super,
  Tagged_Template_Literal,
  This_Variable,
  Trailing_Comma,
  Type_Annotated,  // TypeScript only.
  Unary_Operator,
  Variable,
  Yield_Many,
  Yield_None,
  Yield_One,
};

// property is present:
// * { property: value }
// * { propertyAndValue }
// * { property: value = init }
// * { propertyAndValue = init }
// * { [omittedProperty]: value }
// * { [omittedProperty]: value = init }
// property is omitted:
// * { ...value }
// * { ...value = init }
//
// init is present:
// * { property: value = init }
// * { propertyAndValue = init }
// * { [omittedProperty]: value = init }
// * { ...value = init }
// init is omitted:
// * { [omittedProperty]: value }
// * { property: value }
// * { propertyAndValue }
// * { ...value }
struct Object_Property_Value_Pair {
  // property is optional.
  explicit Object_Property_Value_Pair(Expression *property, Expression *value)
      : property(property), value(value), init(nullptr) {}

  // property is required.
  // init is required.
  explicit Object_Property_Value_Pair(Expression *property, Expression *value,
                                      Expression *init,
                                      const Char8 *init_equal_begin)
      : property(property),
        value(value),
        init(init),
        init_equal_begin(init_equal_begin) {
    QLJS_ASSERT(property);
    QLJS_ASSERT(init);
    QLJS_ASSERT(init_equal_begin);
  }

  // Precondition: init is not null.
  Source_Code_Span init_equals_span() const {
    QLJS_ASSERT(this->init);
    QLJS_ASSERT(this->init_equal_begin);
    return Source_Code_Span(this->init_equal_begin, this->init_equal_begin + 1);
  }

  // true examples:
  // * {x}
  // * {x = z}
  // false examples:
  // * {x: x}
  // * {[x]: x}
  // * {x: x = z}
  bool is_merged_property_and_value_shorthand();

  Expression *property;           // Optional.
  Expression *value;              // Required.
  Expression *init;               // Optional.
  const Char8 *init_equal_begin;  // Used only if init is not null.
};

class Expression_Arena {
 public:
  // TODO(strager): Inline.
  template <class T>
  using Array_Ptr = Span<const T>;

  using Buffering_Visitor_Ptr = Buffering_Visitor *;

  template <class T>
  using Vector = Vector<T>;

  template <class T>
  static inline constexpr bool is_allocatable =
      is_winkable_v<std::remove_reference_t<T>>;

  template <class Expression, class... Args>
  Expression *make_expression(Args &&... args);

  template <class T>
  Array_Ptr<T> make_array(Vector<T> &&);

  template <class T>
  Array_Ptr<T> make_array(T *begin, T *end);

  template <class T, std::size_t size>
  Array_Ptr<T> make_array(std::array<T, size> &&);

  Monotonic_Allocator *allocator() { return &this->allocator_; }

 private:
  template <class T, class... Args>
  T *allocate(Args &&... args) {
    static_assert(is_allocatable<T>);
    return this->allocator_.new_object<T>(std::forward<Args>(args)...);
  }

  // TODO(strager): Accept a Span.
  // TODO(strager): Return a Span.
  template <class T>
  T *allocate_array_move(T *begin, T *end) {
    static_assert(is_allocatable<T>);
    // TODO(strager): Implement Linked_Bump_Allocator::new_objects_move.
    Span<T> result = this->allocator_.allocate_uninitialized_span<T>(
        narrow_cast<std::size_t>(end - begin));
    std::uninitialized_move(begin, end, result.data());
    return result.data();
  }

  Monotonic_Allocator allocator_{"expression_arena"};
};

class Expression {
 public:
  class Expression_With_Prefix_Operator_Base;
  class JSX_Base;

  class Class;
  class Delete;
  class Invalid;
  class Missing;
  class New;
  class Template;
  class Typeof;
  class Array;
  class Arrow_Function;
  class Angle_Type_Assertion;
  class As_Type_Assertion;
  class Assignment;
  class Await;
  class Binary_Operator;
  class Call;
  class Conditional;
  class Dot;
  class Function;
  class Import;
  class Index;
  class JSX_Element;
  class JSX_Element_With_Members;
  class JSX_Element_With_Namespace;
  class JSX_Fragment;
  class Literal;
  class Named_Function;
  class New_Target;
  class Non_Null_Assertion;
  class Object;
  class Optional;
  class Paren;
  class Paren_Empty;
  class Private_Variable;
  class RW_Unary_Prefix;
  class RW_Unary_Suffix;
  class Satisfies;
  class Spread;
  class Super;
  class Tagged_Template_Literal;
  class This_Variable;
  class Trailing_Comma;
  class Type_Annotated;
  class Unary_Operator;
  class Variable;
  class Yield_Many;
  class Yield_None;
  class Yield_One;

  Expression_Kind kind() const { return this->kind_; }

  Identifier variable_identifier() const;
  Token_Type variable_identifier_token_type() const;

  Span_Size child_count() const;

  Expression *child_0() const { return this->child(0); }
  Expression *child_1() const { return this->child(1); }
  Expression *child_2() const { return this->child(2); }

  Expression *child(Span_Size) const;

  Expression_Arena::Array_Ptr<Expression *> children() const;

  // Remove wrapping paren expressions, if any.
  Expression *without_paren() const;

  Span_Size object_entry_count() const;

  Object_Property_Value_Pair object_entry(Span_Size) const;

  Source_Code_Span span() const;

  const Char8 *span_begin() const;
  const Char8 *span_end() const;

  Function_Attributes attributes() const;

 protected:
  explicit Expression(Expression_Kind kind) : kind_(kind) {}

 private:
  Expression_Kind kind_;
};

inline bool
Object_Property_Value_Pair::is_merged_property_and_value_shorthand() {
  return this->property && this->property->kind() == Expression_Kind::Literal &&
         this->value->kind() == Expression_Kind::Variable &&
         this->property->span().begin() == this->value->span().begin();
}

template <class Derived>
Derived expression_cast(Expression *p) {
  // TODO(strager): Assert that Derived matches the Expression's run-time
  // type.
  return derived_cast<Derived>(p);
}

template <class Derived>
Derived expression_cast(const Expression *p) {
  // TODO(strager): Assert that Derived matches the Expression's run-time
  // type.
  return derived_cast<Derived>(p);
}

template <class Derived>
Derived expression_cast(Expression &p) {
  // TODO(strager): Assert that Derived matches the Expression's run-time
  // type.
  return derived_cast<Derived>(p);
}

template <class Derived>
Derived expression_cast(const Expression &p) {
  // TODO(strager): Assert that Derived matches the Expression's run-time
  // type.
  return derived_cast<Derived>(p);
}

template <class Expression, class... Args>
Expression *Expression_Arena::make_expression(Args &&... args) {
  Expression *result(this->allocate<Expression>(std::forward<Args>(args)...));
  static_assert(is_allocatable<Expression>);
  return result;
}

template <class T>
inline Expression_Arena::Array_Ptr<T> Expression_Arena::make_array(
    Vector<T> &&elements) {
  QLJS_ASSERT(elements.get_allocator() == &this->allocator_);
  // NOTE(strager): Adopt the pointer instead of copying.
  Array_Ptr<T> result(elements.data(), elements.size());
  elements.release();
  return result;
}

template <class T>
inline Expression_Arena::Array_Ptr<T> Expression_Arena::make_array(T *begin,
                                                                   T *end) {
  T *result_begin = this->allocate_array_move(begin, end);
  Span_Size size = narrow_cast<Span_Size>(end - begin);
  return Array_Ptr<T>(result_begin, size);
}

template <class T, std::size_t size>
inline Expression_Arena::Array_Ptr<T> Expression_Arena::make_array(
    std::array<T, size> &&elements) {
  return this->make_array(elements.data(), elements.data() + elements.size());
}

class Expression::Expression_With_Prefix_Operator_Base : public Expression {
 public:
  explicit Expression_With_Prefix_Operator_Base(Expression_Kind kind,
                                                Expression *child,
                                                Source_Code_Span operator_span)
      : Expression(kind),
        unary_operator_begin_(operator_span.begin()),
        child_(child) {}

  const Char8 *unary_operator_begin_;
  Expression *child_;
};

class Expression::Delete final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Delete;

  explicit Delete(Expression *child, Source_Code_Span operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         operator_span) {
    QLJS_ASSERT(operator_span.string_view() == u8"delete"_sv);
  }

  Source_Code_Span unary_operator_span() {
    const Char8 *operator_begin = unary_operator_begin_;
    return Source_Code_Span(operator_begin,
                            operator_begin + u8"delete"_sv.size());
  }
};
static_assert(Expression_Arena::is_allocatable<Expression::Delete>);

class Expression::Class : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Class;

  explicit Class(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Class>);

class Expression::Invalid final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Invalid;

  explicit Invalid(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Invalid>);

class Expression::Missing final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Missing;

  explicit Missing(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Missing>);

class Expression::New final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::New;

  explicit New(Expression_Arena::Array_Ptr<Expression *> children,
               Source_Code_Span span)
      : Expression(kind), span_(span), children_(children) {}

  Source_Code_Span span_;
  Expression_Arena::Array_Ptr<Expression *> children_;
};
static_assert(Expression_Arena::is_allocatable<Expression::New>);

class Expression::Template final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Template;

  explicit Template(Expression_Arena::Array_Ptr<Expression *> children,
                    Source_Code_Span span)
      : Expression(kind), span_(span), children_(children) {}

  Source_Code_Span span_;
  Expression_Arena::Array_Ptr<Expression *> children_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Template>);

class Expression::Typeof final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Typeof;

  explicit Typeof(Expression *child, Source_Code_Span operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         operator_span) {}

  Source_Code_Span unary_operator_span() {
    return Source_Code_Span(this->unary_operator_begin_,
                            this->unary_operator_begin_ + u8"typeof"_sv.size());
  }
};
static_assert(Expression_Arena::is_allocatable<Expression::Typeof>);

class Expression::Array final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Array;

  explicit Array(Expression_Arena::Array_Ptr<Expression *> children,
                 Source_Code_Span span)
      : Expression(kind), span_(span), children_(children) {
    QLJS_ASSERT(span.string_view().substr(0, 1) == u8"["_sv);
  }

  Source_Code_Span left_square_span() const {
    return Source_Code_Span(this->span_.begin(), this->span_.begin() + 1);
  }

  Source_Code_Span span_;
  Expression_Arena::Array_Ptr<Expression *> children_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Array>);

class Expression::Arrow_Function final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Arrow_Function;

  explicit Arrow_Function(Function_Attributes attributes,
                          const Char8 *parameter_list_begin,
                          const Char8 *span_end)
      : Expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        span_end_(span_end) {
    QLJS_ASSERT(this->parameter_list_begin_);
  }

  explicit Arrow_Function(Function_Attributes attributes,
                          Expression_Arena::Array_Ptr<Expression *> parameters,
                          const Char8 *parameter_list_begin,
                          const Char8 *span_end)
      : Expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        span_end_(span_end),
        children_(parameters) {
    if (!this->parameter_list_begin_) {
      QLJS_ASSERT(!this->children_.empty());
    }
  }

  Function_Attributes function_attributes_;
  const Char8 *parameter_list_begin_;
  const Char8 *span_end_;
  Expression_Arena::Array_Ptr<Expression *> children_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Arrow_Function>);

class Expression::Angle_Type_Assertion final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Angle_Type_Assertion;

  explicit Angle_Type_Assertion(Source_Code_Span bracketed_type_span,
                                Expression *child)
      : Expression(kind),
        bracketed_type_span_(bracketed_type_span),
        child_(child) {}

  Source_Code_Span bracketed_type_span_;
  Expression *child_;
};
static_assert(
    Expression_Arena::is_allocatable<Expression::Angle_Type_Assertion>);

class Expression::As_Type_Assertion final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::As_Type_Assertion;

  explicit As_Type_Assertion(Expression *child, Source_Code_Span as_span,
                             const Char8 *span_end)
      : Expression(kind),
        child_(child),
        as_keyword_(as_span.begin()),
        span_end_(span_end) {
    QLJS_ASSERT(as_span.string_view() == u8"as"_sv);
  }

  Source_Code_Span as_span() const {
    return Source_Code_Span(this->as_keyword_, this->as_keyword_ + 2);
  }

  Expression *child_;
  const Char8 *as_keyword_;
  const Char8 *span_end_;
};
static_assert(Expression_Arena::is_allocatable<Expression::As_Type_Assertion>);

class Expression::Assignment final : public Expression {
 public:
  explicit Assignment(Expression_Kind kind, Expression *lhs, Expression *rhs,
                      Source_Code_Span operator_span)
      : Expression(kind), children_{lhs, rhs}, operator_span_(operator_span) {
    QLJS_ASSERT(kind == Expression_Kind::Assignment ||
                kind == Expression_Kind::Compound_Assignment ||
                kind == Expression_Kind::Conditional_Assignment);
  }

  std::array<Expression *, 2> children_;
  Source_Code_Span operator_span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Assignment>);

class Expression::Await final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Await;

  explicit Await(Expression *child, Source_Code_Span operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         operator_span) {}

  Source_Code_Span unary_operator_span() const {
    return Source_Code_Span(this->unary_operator_begin_,
                            this->unary_operator_begin_ + 5);
  }
};
static_assert(Expression_Arena::is_allocatable<Expression::Await>);

class Expression::Binary_Operator final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Binary_Operator;

  explicit Binary_Operator(
      Expression_Arena::Array_Ptr<Expression *> children,
      Expression_Arena::Array_Ptr<Source_Code_Span> operator_spans)
      : Expression(kind),
        children_(children),
        operator_spans_(operator_spans.data()) {
    QLJS_ASSERT(children.size() >= 2);
    QLJS_ASSERT(operator_spans.size() == children.size() - 1);
  }

  Expression_Arena::Array_Ptr<Expression *> children_;
  // An array of size this->children_.size()-1.
  const Source_Code_Span *operator_spans_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Binary_Operator>);

class Expression::Call final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Call;

  explicit Call(Expression_Arena::Array_Ptr<Expression *> children,
                Source_Code_Span left_paren_span, const Char8 *span_end,
                std::optional<Source_Code_Span> optional_chaining_op_)
      : Expression(kind),
        call_left_paren_begin_(left_paren_span.begin()),
        span_end_(span_end),
        children_(children),
        optional_chaining_operator_(optional_chaining_op_) {
    QLJS_ASSERT(left_paren_span.size() == 1);
  }

  Source_Code_Span left_paren_span() const {
    return Source_Code_Span(this->call_left_paren_begin_,
                            this->call_left_paren_begin_ + 1);
  }

  const Char8 *call_left_paren_begin_;
  const Char8 *span_end_;
  Expression_Arena::Array_Ptr<Expression *> children_;
  std::optional<Source_Code_Span> optional_chaining_operator_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Call>);

class Expression::Conditional final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Conditional;

  explicit Conditional(Expression *condition, Expression *true_branch,
                       Expression *false_branch)
      : Expression(kind), children_{condition, true_branch, false_branch} {}

  std::array<Expression *, 3> children_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Conditional>);

class Expression::Dot final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Dot;

  explicit Dot(Expression *lhs, Identifier rhs, Source_Code_Span op_span)
      : Expression(kind),
        variable_identifier_(rhs),
        child_(lhs),
        operator_span_(op_span) {}

  Identifier variable_identifier_;
  Expression *child_;
  Source_Code_Span operator_span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Dot>);

class Expression::Function final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Function;

  explicit Function(Function_Attributes attributes, Source_Code_Span span)
      : Expression(kind), function_attributes_(attributes), span_(span) {}

  Function_Attributes function_attributes_;
  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Function>);

class Expression::Import final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Import;

  explicit Import(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Import>);

class Expression::Index final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Index;

  explicit Index(Expression *container, Expression *subscript,
                 Source_Code_Span left_square_span, const Char8 *subscript_end,
                 std::optional<Source_Code_Span> optional_chaining_op_)
      : Expression(kind),
        index_subscript_end_(subscript_end),
        left_square_span(left_square_span),
        children_{container, subscript},
        optional_chaining_operator_(optional_chaining_op_) {}

  const Char8 *index_subscript_end_;
  Source_Code_Span left_square_span;
  std::array<Expression *, 2> children_;
  std::optional<Source_Code_Span> optional_chaining_operator_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Index>);

class Expression::JSX_Base : public Expression {
 public:
  explicit JSX_Base(Expression_Kind kind, Source_Code_Span span,
                    Expression_Arena::Array_Ptr<Expression *> children)
      : Expression(kind), span(span), children(children) {}

  Source_Code_Span span;
  Expression_Arena::Array_Ptr<Expression *> children;
};
static_assert(Expression_Arena::is_allocatable<Expression::JSX_Base>);

class Expression::JSX_Element final : public JSX_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::JSX_Element;

  explicit JSX_Element(Source_Code_Span span, const Identifier &tag,
                       Expression_Arena::Array_Ptr<Expression *> children)
      : JSX_Base(kind, span, children), tag(tag) {}

  bool is_intrinsic() const { return is_intrinsic(this->tag); }

  static bool is_intrinsic(const Identifier &tag) {
    // TODO(strager): Have the lexer do this work for us.
    String8_View name = tag.normalized_name();
    QLJS_ASSERT(!name.empty());
    Char8 first_char = name[0];
    return islower(first_char) || contains(name, u8'-');
  }

  Identifier tag;
};
static_assert(Expression_Arena::is_allocatable<Expression::JSX_Element>);

class Expression::JSX_Element_With_Members final : public JSX_Base {
 public:
  static constexpr Expression_Kind kind =
      Expression_Kind::JSX_Element_With_Members;

  explicit JSX_Element_With_Members(
      Source_Code_Span span, Expression_Arena::Array_Ptr<Identifier> members,
      Expression_Arena::Array_Ptr<Expression *> children)
      : JSX_Base(kind, span, children), members(members) {}

  bool is_intrinsic() const { return false; }

  Expression_Arena::Array_Ptr<Identifier> members;
};
static_assert(
    Expression_Arena::is_allocatable<Expression::JSX_Element_With_Members>);

class Expression::JSX_Element_With_Namespace final : public JSX_Base {
 public:
  static constexpr Expression_Kind kind =
      Expression_Kind::JSX_Element_With_Namespace;

  explicit JSX_Element_With_Namespace(
      Source_Code_Span span, const Identifier &ns, const Identifier &tag,
      Expression_Arena::Array_Ptr<Expression *> children)
      : JSX_Base(kind, span, children), ns(ns), tag(tag) {}

  bool is_intrinsic() const { return true; }

  Identifier ns;   // Namespace (before ':').
  Identifier tag;  // After ':'.
};
static_assert(
    Expression_Arena::is_allocatable<Expression::JSX_Element_With_Namespace>);

class Expression::JSX_Fragment final : public JSX_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::JSX_Fragment;

  explicit JSX_Fragment(Source_Code_Span span,
                        Expression_Arena::Array_Ptr<Expression *> children)
      : JSX_Base(kind, span, children) {}
};
static_assert(Expression_Arena::is_allocatable<Expression::JSX_Fragment>);

class Expression::Literal final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Literal;

  explicit Literal(Source_Code_Span span) : Expression(kind), span_(span) {}

  bool is_null() const { return this->span_.string_view()[0] == u8'n'; }

  bool is_regexp() const { return this->span_.string_view()[0] == u8'/'; }

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Literal>);

class Expression::Named_Function final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Named_Function;

  explicit Named_Function(Function_Attributes attributes, Identifier name,
                          Source_Code_Span span)
      : Expression(kind),
        function_attributes_(attributes),
        variable_identifier_(name),
        span_(span) {}

  Function_Attributes function_attributes_;
  Identifier variable_identifier_;
  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Named_Function>);

class Expression::New_Target final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::New_Target;

  explicit New_Target(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::New_Target>);

class Expression::Non_Null_Assertion final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Non_Null_Assertion;

  explicit Non_Null_Assertion(Expression *child, Source_Code_Span bang_span)
      : Expression(kind), bang_end_(bang_span.end()), child_(child) {
    QLJS_ASSERT(same_pointers(this->bang_span(), bang_span));
  }

  Source_Code_Span bang_span() const {
    return Source_Code_Span(this->bang_end_ - 1, this->bang_end_);
  }

  const Char8 *bang_end_;
  Expression *child_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Non_Null_Assertion>);

class Expression::Object final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Object;

  explicit Object(
      Expression_Arena::Array_Ptr<Object_Property_Value_Pair> entries,
      Source_Code_Span span)
      : Expression(kind), span_(span), entries_(entries) {
    QLJS_ASSERT(span.string_view().substr(0, 1) == u8"{"_sv);
  }

  Source_Code_Span left_curly_span() const {
    return Source_Code_Span(this->span_.begin(), this->span_.begin() + 1);
  }

  Source_Code_Span span_;
  Expression_Arena::Array_Ptr<Object_Property_Value_Pair> entries_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Object>);

class Expression::Optional final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Optional;

  explicit Optional(Expression *child, Source_Code_Span question_span)
      : Expression(kind), child_(child), question_end_(question_span.end()) {
    QLJS_ASSERT(question_span.end() - question_span.begin() == 1);
  }

  Source_Code_Span question_span() const {
    return Source_Code_Span(this->question_end_ - 1, this->question_end_);
  }

  Expression *child_;
  const Char8 *question_end_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Optional>);

class Expression::Paren final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Paren;

  explicit Paren(Source_Code_Span span, Expression *child)
      : Expression(kind), span_(span), child_(child) {}

  Source_Code_Span span_;
  Expression *child_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Paren>);

class Expression::Paren_Empty final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Paren_Empty;

  explicit Paren_Empty(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span left_paren_span() const {
    return Source_Code_Span(this->span_.begin(), this->span_.begin() + 1);
  }

  Source_Code_Span right_paren_span() const {
    return Source_Code_Span(this->span_.end() - 1, this->span_.end());
  }

  void report_missing_expression_error(Diag_Reporter *reporter) {
    reporter->report(Diag_Missing_Expression_Between_Parentheses{
        .left_paren_to_right_paren = this->span_,
        .left_paren = this->left_paren_span(),
        .right_paren = this->right_paren_span(),
    });
  }

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Paren_Empty>);

class Expression::Private_Variable final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Private_Variable;

  explicit Private_Variable(Identifier variable_identifier)
      : Expression(kind), variable_identifier_(variable_identifier) {}

  Identifier variable_identifier_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Private_Variable>);

class Expression::RW_Unary_Prefix final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::RW_Unary_Prefix;

  explicit RW_Unary_Prefix(Expression *child, Source_Code_Span operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         operator_span) {}
};
static_assert(Expression_Arena::is_allocatable<Expression::RW_Unary_Prefix>);

class Expression::RW_Unary_Suffix final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::RW_Unary_Suffix;

  explicit RW_Unary_Suffix(Expression *child, Source_Code_Span operator_span)
      : Expression(kind),
        unary_operator_end_(operator_span.end()),
        child_(child) {}

  const Char8 *unary_operator_end_;
  Expression *child_;
};
static_assert(Expression_Arena::is_allocatable<Expression::RW_Unary_Suffix>);

class Expression::Satisfies final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Satisfies;

  explicit Satisfies(Expression *child, Source_Code_Span satisfies_span,
                     const Char8 *span_end)
      : Expression(kind),
        child_(child),
        satisfies_keyword_(satisfies_span.begin()),
        span_end_(span_end) {
    QLJS_ASSERT(satisfies_span.string_view() == u8"satisfies"_sv);
  }

  Source_Code_Span satisfies_span() const {
    return Source_Code_Span(this->satisfies_keyword_,
                            this->satisfies_keyword_ + 9);
  }

  Expression *child_;
  const Char8 *satisfies_keyword_;
  const Char8 *span_end_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Satisfies>);

class Expression::Spread final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Spread;

  explicit Spread(Expression *child, Source_Code_Span operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         operator_span) {
    QLJS_ASSERT(operator_span.end() - operator_span.begin() ==
                this->spread_operator_length);
  }

  Source_Code_Span spread_operator_span() const {
    return Source_Code_Span(
        this->unary_operator_begin_,
        this->unary_operator_begin_ + this->spread_operator_length);
  }

  static constexpr int spread_operator_length = 3;  // "..."
};
static_assert(Expression_Arena::is_allocatable<Expression::Spread>);

class Expression::Super final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Super;

  explicit Super(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Super>);

class Expression::Tagged_Template_Literal final : public Expression {
 public:
  explicit Tagged_Template_Literal(
      Expression_Arena::Array_Ptr<Expression *> tag_and_template_children,
      Source_Code_Span template_span)
      : Expression(Expression_Kind::Tagged_Template_Literal),
        tag_and_template_children_(tag_and_template_children),
        template_span_end_(template_span.end()) {
    QLJS_ASSERT(!tag_and_template_children.empty());
  }

  Expression_Arena::Array_Ptr<Expression *> tag_and_template_children_;
  const Char8 *template_span_end_;
};
static_assert(
    Expression_Arena::is_allocatable<Expression::Tagged_Template_Literal>);

class Expression::This_Variable final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::This_Variable;

  explicit This_Variable(Source_Code_Span span)
      : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::This_Variable>);

class Expression::Trailing_Comma final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Trailing_Comma;

  explicit Trailing_Comma(Expression_Arena::Array_Ptr<Expression *> children,
                          Source_Code_Span comma_span)
      : Expression(kind), children_(children), comma_end_(comma_span.end()) {
    QLJS_ASSERT(comma_span.end() == comma_span.begin() + 1);
  }

  Source_Code_Span comma_span() const {
    return Source_Code_Span(this->comma_end_ - 1, this->comma_end_);
  }

  Expression_Arena::Array_Ptr<Expression *> children_;
  const Char8 *comma_end_;
};

class Expression::Type_Annotated final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Type_Annotated;

  explicit Type_Annotated(Expression *child, Source_Code_Span colon_span,
                          Buffering_Visitor &&type_visits,
                          const Char8 *span_end)
      : Expression(kind),
        child_(child),
        colon_(colon_span.begin()),
        type_visits_(std::move(type_visits)),
        span_end_(span_end) {
    QLJS_ASSERT(*colon_span.begin() == u8':');
    QLJS_ASSERT(colon_span.size() == 1);
  }

  Source_Code_Span colon_span() const {
    return Source_Code_Span(this->colon_, this->colon_ + 1);
  }

  void visit_type_annotation(Parse_Visitor_Base &v) {
    std::move(this->type_visits_).move_into(v);
  }

  Expression *child_;
  const Char8 *colon_;
  Buffering_Visitor type_visits_{nullptr};
  const Char8 *span_end_;
};
template <>
struct Is_Winkable<Expression::Type_Annotated>
    : Is_Winkable<Buffering_Visitor> {};
static_assert(Expression_Arena::is_allocatable<Expression::Type_Annotated>);

class Expression::Unary_Operator final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Unary_Operator;

  explicit Unary_Operator(Expression *child, Source_Code_Span operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         operator_span) {}

  bool is_void_operator() const {
    // HACK(strager): Should we create Expression::_void?
    return this->unary_operator_begin_[0] == u8'v';
  }
};
static_assert(Expression_Arena::is_allocatable<Expression::Unary_Operator>);

class Expression::Variable final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Variable;

  explicit Variable(Identifier variable_identifier, Token_Type type)
      : Expression(kind),
        type_(type),
        variable_identifier_(variable_identifier) {}

  Token_Type type_;
  Identifier variable_identifier_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Variable>);

class Expression::Yield_Many final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Yield_Many;

  explicit Yield_Many(Expression *child, Source_Code_Span yield_operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         yield_operator_span) {}
};
static_assert(Expression_Arena::is_allocatable<Expression::Yield_Many>);

class Expression::Yield_None final : public Expression {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Yield_None;

  explicit Yield_None(Source_Code_Span span) : Expression(kind), span_(span) {}

  Source_Code_Span span_;
};
static_assert(Expression_Arena::is_allocatable<Expression::Yield_None>);

class Expression::Yield_One final
    : public Expression::Expression_With_Prefix_Operator_Base {
 public:
  static constexpr Expression_Kind kind = Expression_Kind::Yield_One;

  explicit Yield_One(Expression *child, Source_Code_Span operator_span)
      : Expression::Expression_With_Prefix_Operator_Base(kind, child,
                                                         operator_span) {}
};
static_assert(Expression_Arena::is_allocatable<Expression::Yield_One>);

inline Identifier Expression::variable_identifier() const {
  switch (this->kind_) {
  case Expression_Kind::Dot:
    return expression_cast<const Expression::Dot *>(this)->variable_identifier_;
  case Expression_Kind::JSX_Element:
    return expression_cast<const Expression::JSX_Element *>(this)->tag;
  case Expression_Kind::Named_Function:
    return expression_cast<const Expression::Named_Function *>(this)
        ->variable_identifier_;
  case Expression_Kind::Private_Variable:
    return expression_cast<const Expression::Private_Variable *>(this)
        ->variable_identifier_;
  case Expression_Kind::Variable:
    return expression_cast<const Expression::Variable *>(this)
        ->variable_identifier_;

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline Token_Type Expression::variable_identifier_token_type() const {
  switch (this->kind_) {
  case Expression_Kind::Variable:
    return expression_cast<const Expression::Variable *>(this)->type_;

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline Span_Size Expression::child_count() const {
  return this->children().size();
}

inline Expression *Expression::child(Span_Size index) const {
  return this->children()[index];
}

inline Expression_Arena::Array_Ptr<Expression *> Expression::children() const {
  switch (this->kind_) {
  case Expression_Kind::Assignment:
  case Expression_Kind::Compound_Assignment:
  case Expression_Kind::Conditional_Assignment: {
    auto *assignment = expression_cast<const Expression::Assignment *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(assignment->children_);
  }

  case Expression_Kind::Delete:
  case Expression_Kind::Typeof:
  case Expression_Kind::Await:
  case Expression_Kind::RW_Unary_Prefix:
  case Expression_Kind::Spread:
  case Expression_Kind::Unary_Operator:
  case Expression_Kind::Yield_Many:
  case Expression_Kind::Yield_One: {
    auto *ast = expression_cast<
        const Expression::Expression_With_Prefix_Operator_Base *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&ast->child_, 1);
  }

  case Expression_Kind::JSX_Element:
  case Expression_Kind::JSX_Element_With_Members:
  case Expression_Kind::JSX_Element_With_Namespace:
  case Expression_Kind::JSX_Fragment: {
    auto *jsx = expression_cast<const Expression::JSX_Base *>(this);
    return jsx->children;
  }

  case Expression_Kind::New:
    return expression_cast<const Expression::New *>(this)->children_;
  case Expression_Kind::Template:
    return expression_cast<const Expression::Template *>(this)->children_;
  case Expression_Kind::Angle_Type_Assertion: {
    auto *assertion =
        expression_cast<const Expression::Angle_Type_Assertion *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&assertion->child_, 1);
  }
  case Expression_Kind::Array:
    return expression_cast<const Expression::Array *>(this)->children_;
  case Expression_Kind::Arrow_Function:
    return expression_cast<const Expression::Arrow_Function *>(this)->children_;
  case Expression_Kind::As_Type_Assertion: {
    auto *assertion =
        expression_cast<const Expression::As_Type_Assertion *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&assertion->child_, 1);
  }
  case Expression_Kind::Binary_Operator:
    return expression_cast<const Expression::Binary_Operator *>(this)
        ->children_;
  case Expression_Kind::Call:
    return expression_cast<const Expression::Call *>(this)->children_;
  case Expression_Kind::Conditional: {
    auto *conditional = expression_cast<const Expression::Conditional *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(conditional->children_);
  }
  case Expression_Kind::Dot: {
    auto *dot = expression_cast<const Expression::Dot *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&dot->child_, 1);
  }
  case Expression_Kind::Index: {
    auto *index = expression_cast<const Expression::Index *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(index->children_);
  }
  case Expression_Kind::Non_Null_Assertion: {
    auto *assertion =
        expression_cast<const Expression::Non_Null_Assertion *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&assertion->child_, 1);
  }
  case Expression_Kind::Paren: {
    auto *paren = expression_cast<const Expression::Paren *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&paren->child_, 1);
  }
  case Expression_Kind::Optional: {
    auto *optional = expression_cast<const Expression::Optional *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&optional->child_, 1);
  }
  case Expression_Kind::RW_Unary_Suffix: {
    auto *rw_unary_suffix =
        expression_cast<const Expression::RW_Unary_Suffix *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&rw_unary_suffix->child_,
                                                     1);
  }
  case Expression_Kind::Satisfies: {
    auto *satisfies = expression_cast<const Expression::Satisfies *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&satisfies->child_, 1);
  }
  case Expression_Kind::Tagged_Template_Literal:
    return expression_cast<const Expression::Tagged_Template_Literal *>(this)
        ->tag_and_template_children_;
  case Expression_Kind::Trailing_Comma:
    return expression_cast<const Expression::Trailing_Comma *>(this)->children_;
  case Expression_Kind::Type_Annotated: {
    auto *annotated = expression_cast<const Expression::Type_Annotated *>(this);
    return Expression_Arena::Array_Ptr<Expression *>(&annotated->child_, 1);
  }

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline Expression *Expression::without_paren() const {
  const Expression *ast = this;
  while (ast->kind_ == Expression_Kind::Paren) {
    ast = expression_cast<const Paren *>(ast)->child_;
  }
  // TODO(strager): Remove const_cast.
  return const_cast<Expression *>(ast);
}

inline Span_Size Expression::object_entry_count() const {
  switch (this->kind_) {
  case Expression_Kind::Object:
    return expression_cast<const Expression::Object *>(this)->entries_.size();

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline Object_Property_Value_Pair Expression::object_entry(
    Span_Size index) const {
  switch (this->kind_) {
  case Expression_Kind::Object:
    return expression_cast<const Expression::Object *>(this)->entries_[index];

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline Source_Code_Span Expression::span() const {
  switch (this->kind_) {
  case Expression_Kind::Assignment:
  case Expression_Kind::Compound_Assignment:
  case Expression_Kind::Conditional_Assignment: {
    auto *assignment = expression_cast<const Expression::Assignment *>(this);
    return Source_Code_Span(assignment->children_.front()->span().begin(),
                            assignment->children_.back()->span().end());
  }

  case Expression_Kind::Delete:
  case Expression_Kind::Typeof:
  case Expression_Kind::Await:
  case Expression_Kind::RW_Unary_Prefix:
  case Expression_Kind::Spread:
  case Expression_Kind::Unary_Operator:
  case Expression_Kind::Yield_Many:
  case Expression_Kind::Yield_One: {
    auto *prefix = expression_cast<
        const Expression::Expression_With_Prefix_Operator_Base *>(this);
    return Source_Code_Span(prefix->unary_operator_begin_,
                            prefix->child_->span().end());
  }

  case Expression_Kind::JSX_Element:
  case Expression_Kind::JSX_Element_With_Members:
  case Expression_Kind::JSX_Element_With_Namespace:
  case Expression_Kind::JSX_Fragment:
    return expression_cast<const JSX_Base *>(this)->span;

  case Expression_Kind::Class:
    return expression_cast<const Class *>(this)->span_;
  case Expression_Kind::Invalid:
    return expression_cast<const Invalid *>(this)->span_;
  case Expression_Kind::Missing:
    return expression_cast<const Missing *>(this)->span_;
  case Expression_Kind::New:
    return expression_cast<const New *>(this)->span_;
  case Expression_Kind::Template:
    return expression_cast<const Template *>(this)->span_;
  case Expression_Kind::Angle_Type_Assertion: {
    auto *assertion = expression_cast<const Angle_Type_Assertion *>(this);
    return Source_Code_Span(assertion->bracketed_type_span_.begin(),
                            assertion->child_->span().end());
  }
  case Expression_Kind::Array:
    return expression_cast<const Array *>(this)->span_;
  case Expression_Kind::Arrow_Function: {
    auto *arrow = expression_cast<const Expression::Arrow_Function *>(this);
    if (arrow->parameter_list_begin_) {
      return Source_Code_Span(arrow->parameter_list_begin_, arrow->span_end_);
    } else {
      return Source_Code_Span(arrow->children_.front()->span().begin(),
                              arrow->span_end_);
    }
  }
  case Expression_Kind::As_Type_Assertion: {
    auto *assertion = expression_cast<const As_Type_Assertion *>(this);
    return Source_Code_Span(assertion->child_->span().begin(),
                            assertion->span_end_);
  }
  case Expression_Kind::Binary_Operator: {
    auto *binary = expression_cast<const Expression::Binary_Operator *>(this);
    return Source_Code_Span(binary->children_.front()->span().begin(),
                            binary->children_.back()->span().end());
  }
  case Expression_Kind::Call: {
    auto *call = expression_cast<const Expression::Call *>(this);
    return Source_Code_Span(call->children_.front()->span().begin(),
                            call->span_end_);
  }
  case Expression_Kind::Conditional: {
    auto *conditional = expression_cast<const Expression::Conditional *>(this);
    return Source_Code_Span(conditional->children_.front()->span().begin(),
                            conditional->children_.back()->span().end());
  }
  case Expression_Kind::Dot: {
    auto *dot = expression_cast<const Expression::Dot *>(this);
    return Source_Code_Span(dot->child_0()->span().begin(),
                            dot->variable_identifier_.span().end());
  }
  case Expression_Kind::Function:
    return expression_cast<const Function *>(this)->span_;
  case Expression_Kind::Import:
    return expression_cast<const Import *>(this)->span_;
  case Expression_Kind::Index: {
    auto *index = expression_cast<const Expression::Index *>(this);
    return Source_Code_Span(index->child_0()->span().begin(),
                            index->index_subscript_end_);
  }
  case Expression_Kind::Literal:
    return expression_cast<const Literal *>(this)->span_;
  case Expression_Kind::Named_Function:
    return expression_cast<const Named_Function *>(this)->span_;
  case Expression_Kind::New_Target:
    return expression_cast<const New_Target *>(this)->span_;
  case Expression_Kind::Non_Null_Assertion: {
    auto *assertion = expression_cast<const Non_Null_Assertion *>(this);
    return Source_Code_Span(assertion->child_->span().begin(),
                            assertion->bang_end_);
  }
  case Expression_Kind::Object:
    return expression_cast<const Object *>(this)->span_;
  case Expression_Kind::Optional: {
    auto *optional = expression_cast<const Expression::Optional *>(this);
    return Source_Code_Span(optional->child_->span().begin(),
                            optional->question_end_);
  }
  case Expression_Kind::Paren:
    return expression_cast<const Paren *>(this)->span_;
  case Expression_Kind::Paren_Empty:
    return expression_cast<const Paren_Empty *>(this)->span_;
  case Expression_Kind::Private_Variable:
    return expression_cast<const Private_Variable *>(this)
        ->variable_identifier_.span();
  case Expression_Kind::RW_Unary_Suffix: {
    auto *suffix = expression_cast<const RW_Unary_Suffix *>(this);
    return Source_Code_Span(suffix->child_->span().begin(),
                            suffix->unary_operator_end_);
  }
  case Expression_Kind::Satisfies: {
    auto *s = expression_cast<const Satisfies *>(this);
    return Source_Code_Span(s->child_->span().begin(), s->span_end_);
  }
  case Expression_Kind::Super:
    return expression_cast<const Super *>(this)->span_;
  case Expression_Kind::Tagged_Template_Literal: {
    auto *literal = expression_cast<const Tagged_Template_Literal *>(this);
    return Source_Code_Span(
        literal->tag_and_template_children_[0]->span().begin(),
        literal->template_span_end_);
  }
  case Expression_Kind::This_Variable:
    return expression_cast<const This_Variable *>(this)->span_;
  case Expression_Kind::Trailing_Comma: {
    auto *comma = expression_cast<const Trailing_Comma *>(this);
    return Source_Code_Span(comma->children_.front()->span().begin(),
                            comma->comma_end_);
  }
  case Expression_Kind::Type_Annotated: {
    auto *annotated = expression_cast<const Type_Annotated *>(this);
    return Source_Code_Span(annotated->child_->span().begin(),
                            annotated->span_end_);
  }
  case Expression_Kind::Variable:
    return expression_cast<const Variable *>(this)->variable_identifier_.span();
  case Expression_Kind::Yield_None:
    return expression_cast<const Yield_None *>(this)->span_;
  }
  QLJS_UNREACHABLE();
}

inline const Char8 *Expression::span_begin() const {
  return this->span().begin();
}

inline const Char8 *Expression::span_end() const { return this->span().end(); }

inline Function_Attributes Expression::attributes() const {
  switch (this->kind_) {
  case Expression_Kind::Arrow_Function:
    return expression_cast<const Expression::Arrow_Function *>(this)
        ->function_attributes_;
  case Expression_Kind::Function:
    return expression_cast<const Expression::Function *>(this)
        ->function_attributes_;
  case Expression_Kind::Named_Function:
    return expression_cast<const Expression::Named_Function *>(this)
        ->function_attributes_;

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}
}

QLJS_WARNING_POP

#undef QLJS_UNEXPECTED_EXPRESSION_KIND

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
