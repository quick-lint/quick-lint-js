// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_EXPRESSION_H
#define QUICK_LINT_JS_EXPRESSION_H

#include <array>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/monotonic-allocator.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/parse-visitor.h>
#include <quick-lint-js/string-view.h>
#include <quick-lint-js/token.h>
#include <quick-lint-js/unreachable.h>
#include <quick-lint-js/vector.h>
#include <quick-lint-js/warning.h>
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
class expression;

enum class expression_kind {
  _class,
  _delete,
  _invalid,
  _missing,
  _new,
  _template,
  _typeof,
  array,
  arrow_function_with_expression,
  arrow_function_with_statements,
  assignment,
  await,
  binary_operator,
  call,
  compound_assignment,
  conditional,
  conditional_assignment,
  dot,
  function,
  import,
  index,
  jsx_element,
  jsx_element_with_members,
  jsx_element_with_namespace,
  jsx_fragment,
  literal,
  named_function,
  new_target,
  object,
  paren,
  private_variable,
  rw_unary_prefix,
  rw_unary_suffix,
  spread,
  super,
  tagged_template_literal,
  trailing_comma,
  unary_operator,
  variable,
  yield_many,
  yield_none,
  yield_one,
};

struct object_property_value_pair {
  explicit object_property_value_pair(expression *property,
                                      expression *value) noexcept
      : property(property), value(value) {}

  explicit object_property_value_pair(expression *value) noexcept
      : property(std::nullopt), value(value) {}

  std::optional<expression *> property;
  expression *value;
};

class expression_arena {
 public:
  template <class>
  class array_ptr;

  using buffering_visitor_ptr = buffering_visitor *;

  template <class T>
  using vector = bump_vector<T, monotonic_allocator>;

  template <class T>
  static inline constexpr bool is_allocatable =
      std::is_trivially_destructible_v<std::remove_reference_t<T>>;

  template <class Expression, class... Args>
  expression *make_expression(Args &&... args);

  template <class T>
  array_ptr<T> make_array(bump_vector<T, monotonic_allocator> &&);

  template <class T>
  array_ptr<T> make_array(T *begin, T *end);

  template <class T, std::size_t Size>
  array_ptr<T> make_array(std::array<T, Size> &&);

  monotonic_allocator *allocator() noexcept { return &this->allocator_; }

 private:
  template <class T, class... Args>
  T *allocate(Args &&... args) {
    static_assert(is_allocatable<T>);
    return this->allocator_.new_object<T>(std::forward<Args>(args)...);
  }

  template <class T>
  T *allocate_array_move(T *begin, T *end) {
    static_assert(is_allocatable<T>);
    T *result = this->allocator_.allocate_uninitialized_array<T>(
        narrow_cast<std::size_t>(end - begin));
    std::uninitialized_move(begin, end, result);
    return result;
  }

  monotonic_allocator allocator_;
};

class expression {
 public:
  class expression_with_prefix_operator_base;
  class jsx_base;

  class _class;
  class _delete;
  class _invalid;
  class _missing;
  class _new;
  class _template;
  class _typeof;
  class array;
  class arrow_function_with_expression;
  class arrow_function_with_statements;
  class assignment;
  class await;
  class binary_operator;
  class call;
  class conditional;
  class dot;
  class function;
  class import;
  class index;
  class jsx_element;
  class jsx_element_with_members;
  class jsx_element_with_namespace;
  class jsx_fragment;
  class literal;
  class named_function;
  class new_target;
  class object;
  class paren;
  class private_variable;
  class rw_unary_prefix;
  class rw_unary_suffix;
  class spread;
  class super;
  class tagged_template_literal;
  class trailing_comma;
  class unary_operator;
  class variable;
  class yield_many;
  class yield_none;
  class yield_one;

  expression_kind kind() const noexcept { return this->kind_; }

  identifier variable_identifier() const noexcept;
  token_type variable_identifier_token_type() const noexcept;

  int child_count() const noexcept;

  expression *child_0() const noexcept { return this->child(0); }
  expression *child_1() const noexcept { return this->child(1); }
  expression *child_2() const noexcept { return this->child(2); }

  expression *child(int) const noexcept;

  expression_arena::array_ptr<expression *> children() const noexcept;

  // Remove wrapping paren expressions, if any.e
  expression *without_paren() const noexcept;

  int object_entry_count() const noexcept;

  object_property_value_pair object_entry(int) const noexcept;

  source_code_span span() const noexcept;

  function_attributes attributes() const noexcept;

 protected:
  explicit expression(expression_kind kind) noexcept : kind_(kind) {}

 private:
  expression_kind kind_;
};

template <class Derived>
Derived *expression_cast(expression *p) noexcept {
  // TODO(strager): Assert that Derived matches the expression's run-time
  // type.
  return static_cast<Derived *>(&*p);
}

// Prevent expression_cast<array>((call*)p).
template <class Derived, class Expression>
Derived *expression_cast(Expression *) noexcept = delete;

template <class T>
class expression_arena::array_ptr {
 public:
  explicit array_ptr() noexcept : data_(nullptr), size_(0) {}

  explicit array_ptr(const T *data, int size) noexcept
      : data_(data), size_(size) {}

  T operator[](int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index < this->size());
    return this->data_[index];
  }

  T front() const noexcept { return (*this)[0]; }

  T back() const noexcept { return (*this)[this->size() - 1]; }

  const T *data() const noexcept { return this->data_; }

  int size() const noexcept { return this->size_; }

  const T *begin() const noexcept { return this->data_; }
  const T *end() const noexcept { return this->data_ + this->size_; }

  bool empty() const noexcept { return this->size() == 0; }

 private:
  const T *data_;
  int size_;
};

template <class Expression, class... Args>
expression *expression_arena::make_expression(Args &&... args) {
  expression *result(this->allocate<Expression>(std::forward<Args>(args)...));
  static_assert(is_allocatable<Expression>);
  return result;
}

template <class T>
inline expression_arena::array_ptr<T> expression_arena::make_array(
    bump_vector<T, monotonic_allocator> &&elements) {
  QLJS_ASSERT(elements.get_allocator() == &this->allocator_);
  // NOTE(strager): Adopt the pointer instead of copying.
  array_ptr<T> result(elements.data(), narrow_cast<int>(elements.size()));
  elements.release();
  return result;
}

template <class T>
inline expression_arena::array_ptr<T> expression_arena::make_array(T *begin,
                                                                   T *end) {
  T *result_begin = this->allocate_array_move(begin, end);
  int size = narrow_cast<int>(end - begin);
  return array_ptr<T>(result_begin, size);
}

template <class T, std::size_t Size>
inline expression_arena::array_ptr<T> expression_arena::make_array(
    std::array<T, Size> &&elements) {
  return this->make_array(elements.data(), elements.data() + elements.size());
}

class expression::expression_with_prefix_operator_base : public expression {
 public:
  explicit expression_with_prefix_operator_base(
      expression_kind kind, expression *child,
      source_code_span operator_span) noexcept
      : expression(kind),
        unary_operator_begin_(operator_span.begin()),
        child_(child) {}

  const char8 *unary_operator_begin_;
  expression *child_;
};

class expression::_delete final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::_delete;

  explicit _delete(expression *child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {
    QLJS_ASSERT(operator_span.string_view() == u8"delete");
  }

  source_code_span unary_operator_span() {
    const char8 *operator_begin = unary_operator_begin_;
    return source_code_span(operator_begin,
                            operator_begin + strlen(u8"delete"));
  }
};
static_assert(expression_arena::is_allocatable<expression::_delete>);

class expression::_class : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_class;

  explicit _class(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::_class>);

class expression::_invalid final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_invalid;

  explicit _invalid(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::_invalid>);

class expression::_missing final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_missing;

  explicit _missing(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::_missing>);

class expression::_new final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_new;

  explicit _new(expression_arena::array_ptr<expression *> children,
                source_code_span span) noexcept
      : expression(kind), span_(span), children_(children) {}

  source_code_span span_;
  expression_arena::array_ptr<expression *> children_;
};
static_assert(expression_arena::is_allocatable<expression::_new>);

class expression::_template final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_template;

  explicit _template(expression_arena::array_ptr<expression *> children,
                     source_code_span span) noexcept
      : expression(kind), span_(span), children_(children) {}

  source_code_span span_;
  expression_arena::array_ptr<expression *> children_;
};
static_assert(expression_arena::is_allocatable<expression::_template>);

class expression::_typeof final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::_typeof;

  explicit _typeof(expression *child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::_typeof>);

class expression::array final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::array;

  explicit array(expression_arena::array_ptr<expression *> children,
                 source_code_span span) noexcept
      : expression(kind), span_(span), children_(children) {}

  source_code_span span_;
  expression_arena::array_ptr<expression *> children_;
};
static_assert(expression_arena::is_allocatable<expression::array>);

class expression::arrow_function_with_expression final : public expression {
 public:
  static constexpr expression_kind kind =
      expression_kind::arrow_function_with_expression;

  explicit arrow_function_with_expression(
      function_attributes attributes, expression *body,
      const char8 *parameter_list_begin) noexcept
      : expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        body_(body) {}

  explicit arrow_function_with_expression(
      function_attributes attributes,
      expression_arena::array_ptr<expression *> parameters, expression *body,
      const char8 *parameter_list_begin) noexcept
      : expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        parameters_(parameters),
        body_(body) {
    if (!this->parameter_list_begin_) {
      QLJS_ASSERT(!this->parameters_.empty());
    }
  }

  function_attributes function_attributes_;
  const char8 *parameter_list_begin_;
  expression_arena::array_ptr<expression *> parameters_;
  expression *body_;
};
static_assert(expression_arena::is_allocatable<
              expression::arrow_function_with_expression>);

class expression::arrow_function_with_statements final : public expression {
 public:
  static constexpr expression_kind kind =
      expression_kind::arrow_function_with_statements;

  explicit arrow_function_with_statements(function_attributes attributes,
                                          const char8 *parameter_list_begin,
                                          const char8 *span_end) noexcept
      : expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        span_end_(span_end) {
    QLJS_ASSERT(this->parameter_list_begin_);
  }

  explicit arrow_function_with_statements(
      function_attributes attributes,
      expression_arena::array_ptr<expression *> parameters,
      const char8 *parameter_list_begin, const char8 *span_end) noexcept
      : expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        span_end_(span_end),
        children_(parameters) {
    if (!this->parameter_list_begin_) {
      QLJS_ASSERT(!this->children_.empty());
    }
  }

  function_attributes function_attributes_;
  const char8 *parameter_list_begin_;
  const char8 *span_end_;
  expression_arena::array_ptr<expression *> children_;
};
static_assert(expression_arena::is_allocatable<
              expression::arrow_function_with_statements>);

class expression::assignment final : public expression {
 public:
  explicit assignment(expression_kind kind, expression *lhs, expression *rhs,
                      source_code_span operator_span) noexcept
      : expression(kind), children_{lhs, rhs}, operator_span_(operator_span) {
    QLJS_ASSERT(kind == expression_kind::assignment ||
                kind == expression_kind::compound_assignment ||
                kind == expression_kind::conditional_assignment);
  }

  std::array<expression *, 2> children_;
  source_code_span operator_span_;
};
static_assert(expression_arena::is_allocatable<expression::assignment>);

class expression::await final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::await;

  explicit await(expression *child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}

  source_code_span unary_operator_span() const noexcept {
    return source_code_span(this->unary_operator_begin_,
                            this->unary_operator_begin_ + 5);
  }
};
static_assert(expression_arena::is_allocatable<expression::await>);

class expression::binary_operator final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::binary_operator;

  explicit binary_operator(
      expression_arena::array_ptr<expression *> children) noexcept
      : expression(kind), children_(children) {}

  expression_arena::array_ptr<expression *> children_;
};
static_assert(expression_arena::is_allocatable<expression::binary_operator>);

class expression::call final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::call;

  explicit call(expression_arena::array_ptr<expression *> children,
                source_code_span left_paren_span,
                const char8 *span_end) noexcept
      : expression(kind),
        call_left_paren_begin_(left_paren_span.begin()),
        span_end_(span_end),
        children_(children) {
    QLJS_ASSERT(left_paren_span.size() == 1);
  }

  source_code_span left_paren_span() const noexcept {
    return source_code_span(this->call_left_paren_begin_,
                            this->call_left_paren_begin_ + 1);
  }

  const char8 *call_left_paren_begin_;
  const char8 *span_end_;
  expression_arena::array_ptr<expression *> children_;
};
static_assert(expression_arena::is_allocatable<expression::call>);

class expression::conditional final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::conditional;

  explicit conditional(expression *condition, expression *true_branch,
                       expression *false_branch) noexcept
      : expression(kind), children_{condition, true_branch, false_branch} {}

  std::array<expression *, 3> children_;
};
static_assert(expression_arena::is_allocatable<expression::conditional>);

class expression::dot final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::dot;

  explicit dot(expression *lhs, identifier rhs) noexcept
      : expression(kind), variable_identifier_(rhs), child_(lhs) {}

  identifier variable_identifier_;
  expression *child_;
};
static_assert(expression_arena::is_allocatable<expression::dot>);

class expression::function final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::function;

  explicit function(function_attributes attributes,
                    source_code_span span) noexcept
      : expression(kind), function_attributes_(attributes), span_(span) {}

  function_attributes function_attributes_;
  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::function>);

class expression::import final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::import;

  explicit import(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::import>);

class expression::index final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::index;

  explicit index(expression *container, expression *subscript,
                 const char8 *subscript_end) noexcept
      : expression(kind),
        index_subscript_end_(subscript_end),
        children_{container, subscript} {}

  const char8 *index_subscript_end_;
  std::array<expression *, 2> children_;
};
static_assert(expression_arena::is_allocatable<expression::index>);

class expression::jsx_base : public expression {
 public:
  explicit jsx_base(expression_kind kind, source_code_span span,
                    expression_arena::array_ptr<expression *> children) noexcept
      : expression(kind), span(span), children(children) {}

  source_code_span span;
  expression_arena::array_ptr<expression *> children;
};
static_assert(expression_arena::is_allocatable<expression::jsx_base>);

class expression::jsx_element final : public jsx_base {
 public:
  static constexpr expression_kind kind = expression_kind::jsx_element;

  explicit jsx_element(
      source_code_span span, const identifier &tag,
      expression_arena::array_ptr<expression *> children) noexcept
      : jsx_base(kind, span, children), tag(tag) {}

  bool is_intrinsic() const noexcept {
    // TODO(strager): Have the lexer do this work for us.
    string8_view name = tag.normalized_name();
    QLJS_ASSERT(!name.empty());
    char8 first_char = name[0];
    return (u8'a' <= first_char && first_char <= u8'z') ||
           contains(name, u8'-');
  }

  identifier tag;
};
static_assert(expression_arena::is_allocatable<expression::jsx_element>);

class expression::jsx_element_with_members final : public jsx_base {
 public:
  static constexpr expression_kind kind =
      expression_kind::jsx_element_with_members;

  explicit jsx_element_with_members(
      source_code_span span, expression_arena::array_ptr<identifier> members,
      expression_arena::array_ptr<expression *> children) noexcept
      : jsx_base(kind, span, children), members(members) {}

  bool is_intrinsic() const noexcept { return false; }

  expression_arena::array_ptr<identifier> members;
};
static_assert(
    expression_arena::is_allocatable<expression::jsx_element_with_members>);

class expression::jsx_element_with_namespace final : public jsx_base {
 public:
  static constexpr expression_kind kind =
      expression_kind::jsx_element_with_namespace;

  explicit jsx_element_with_namespace(
      source_code_span span, const identifier &ns, const identifier &tag,
      expression_arena::array_ptr<expression *> children) noexcept
      : jsx_base(kind, span, children), ns(ns), tag(tag) {}

  bool is_intrinsic() const noexcept { return true; }

  identifier ns;   // Namespace (before ':').
  identifier tag;  // After ':'.
};
static_assert(
    expression_arena::is_allocatable<expression::jsx_element_with_namespace>);

class expression::jsx_fragment final : public jsx_base {
 public:
  static constexpr expression_kind kind = expression_kind::jsx_fragment;

  explicit jsx_fragment(
      source_code_span span,
      expression_arena::array_ptr<expression *> children) noexcept
      : jsx_base(kind, span, children) {}
};
static_assert(expression_arena::is_allocatable<expression::jsx_fragment>);

class expression::literal final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::literal;

  explicit literal(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::literal>);

class expression::named_function final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::named_function;

  explicit named_function(function_attributes attributes, identifier name,
                          source_code_span span) noexcept
      : expression(kind),
        function_attributes_(attributes),
        variable_identifier_(name),
        span_(span) {}

  function_attributes function_attributes_;
  identifier variable_identifier_;
  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::named_function>);

class expression::new_target final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::new_target;

  explicit new_target(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::new_target>);

class expression::object final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::object;

  explicit object(
      expression_arena::array_ptr<object_property_value_pair> entries,
      source_code_span span) noexcept
      : expression(kind), span_(span), entries_(entries) {}

  source_code_span span_;
  expression_arena::array_ptr<object_property_value_pair> entries_;
};
static_assert(expression_arena::is_allocatable<expression::object>);

class expression::paren final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::paren;

  explicit paren(source_code_span span, expression *child) noexcept
      : expression(kind), span_(span), child_(child) {}

  source_code_span span_;
  expression *child_;
};
static_assert(expression_arena::is_allocatable<expression::paren>);

class expression::private_variable final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::private_variable;

  explicit private_variable(identifier variable_identifier) noexcept
      : expression(kind), variable_identifier_(variable_identifier) {}

  identifier variable_identifier_;
};
static_assert(expression_arena::is_allocatable<expression::private_variable>);

class expression::rw_unary_prefix final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::rw_unary_prefix;

  explicit rw_unary_prefix(expression *child,
                           source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::rw_unary_prefix>);

class expression::rw_unary_suffix final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::rw_unary_suffix;

  explicit rw_unary_suffix(expression *child,
                           source_code_span operator_span) noexcept
      : expression(kind),
        unary_operator_end_(operator_span.end()),
        child_(child) {}

  const char8 *unary_operator_end_;
  expression *child_;
};
static_assert(expression_arena::is_allocatable<expression::rw_unary_suffix>);

class expression::spread final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::spread;

  explicit spread(expression *child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::spread>);

class expression::super final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::super;

  explicit super(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::super>);

class expression::tagged_template_literal final : public expression {
 public:
  explicit tagged_template_literal(
      expression_arena::array_ptr<expression *> tag_and_template_children,
      source_code_span template_span) noexcept
      : expression(expression_kind::tagged_template_literal),
        tag_and_template_children_(tag_and_template_children),
        template_span_end_(template_span.end()) {
    QLJS_ASSERT(!tag_and_template_children.empty());
  }

  expression_arena::array_ptr<expression *> tag_and_template_children_;
  const char8 *template_span_end_;
};
static_assert(
    expression_arena::is_allocatable<expression::tagged_template_literal>);

class expression::trailing_comma final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::trailing_comma;

  explicit trailing_comma(expression_arena::array_ptr<expression *> children,
                          source_code_span comma_span) noexcept
      : expression(kind), children_(children), comma_end_(comma_span.end()) {
    QLJS_ASSERT(comma_span.end() == comma_span.begin() + 1);
  }

  source_code_span comma_span() const noexcept {
    return source_code_span(this->comma_end_ - 1, this->comma_end_);
  }

  expression_arena::array_ptr<expression *> children_;
  const char8 *comma_end_;
};
static_assert(expression_arena::is_allocatable<expression::trailing_comma>);

class expression::unary_operator final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::unary_operator;

  explicit unary_operator(expression *child,
                          source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::unary_operator>);

class expression::variable final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::variable;

  explicit variable(identifier variable_identifier, token_type type) noexcept
      : expression(kind),
        type_(type),
        variable_identifier_(variable_identifier) {}

  token_type type_;
  identifier variable_identifier_;
};
static_assert(expression_arena::is_allocatable<expression::variable>);

class expression::yield_many final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::yield_many;

  explicit yield_many(expression *child,
                      source_code_span yield_operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         yield_operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::yield_many>);

class expression::yield_none final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::yield_none;

  explicit yield_none(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::yield_none>);

class expression::yield_one final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::yield_one;

  explicit yield_one(expression *child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::yield_one>);

inline identifier expression::variable_identifier() const noexcept {
  switch (this->kind_) {
  case expression_kind::dot:
    return static_cast<const expression::dot *>(this)->variable_identifier_;
  case expression_kind::jsx_element:
    return static_cast<const expression::jsx_element *>(this)->tag;
  case expression_kind::named_function:
    return static_cast<const expression::named_function *>(this)
        ->variable_identifier_;
  case expression_kind::private_variable:
    return static_cast<const expression::private_variable *>(this)
        ->variable_identifier_;
  case expression_kind::variable:
    return static_cast<const expression::variable *>(this)
        ->variable_identifier_;

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline token_type expression::variable_identifier_token_type() const noexcept {
  switch (this->kind_) {
  case expression_kind::variable:
    return static_cast<const expression::variable *>(this)->type_;

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline int expression::child_count() const noexcept {
  return this->children().size();
}

inline expression *expression::child(int index) const noexcept {
  expression_arena::array_ptr<expression *> children = this->children();
  QLJS_ASSERT(index >= 0);
  QLJS_ASSERT(index < children.size());
  return children[index];
}

inline expression_arena::array_ptr<expression *> expression::children() const
    noexcept {
  switch (this->kind_) {
  case expression_kind::assignment:
  case expression_kind::compound_assignment:
  case expression_kind::conditional_assignment: {
    auto *assignment = static_cast<const expression::assignment *>(this);
    return expression_arena::array_ptr<expression *>(
        assignment->children_.data(),
        narrow_cast<int>(assignment->children_.size()));
  }

  case expression_kind::_delete:
  case expression_kind::_typeof:
  case expression_kind::await:
  case expression_kind::rw_unary_prefix:
  case expression_kind::spread:
  case expression_kind::unary_operator:
  case expression_kind::yield_many:
  case expression_kind::yield_one: {
    auto *ast =
        static_cast<const expression::expression_with_prefix_operator_base *>(
            this);
    return expression_arena::array_ptr<expression *>(&ast->child_, 1);
  }

  case expression_kind::jsx_element:
  case expression_kind::jsx_element_with_members:
  case expression_kind::jsx_element_with_namespace:
  case expression_kind::jsx_fragment: {
    auto *jsx = static_cast<const expression::jsx_base *>(this);
    return jsx->children;
  }

  case expression_kind::_new:
    return static_cast<const expression::_new *>(this)->children_;
  case expression_kind::_template:
    return static_cast<const expression::_template *>(this)->children_;
  case expression_kind::array:
    return static_cast<const expression::array *>(this)->children_;
  case expression_kind::arrow_function_with_expression:
    return static_cast<const expression::arrow_function_with_expression *>(this)
        ->parameters_;
  case expression_kind::arrow_function_with_statements:
    return static_cast<const expression::arrow_function_with_statements *>(this)
        ->children_;
  case expression_kind::binary_operator:
    return static_cast<const expression::binary_operator *>(this)->children_;
  case expression_kind::call:
    return static_cast<const expression::call *>(this)->children_;
  case expression_kind::conditional: {
    auto *conditional = static_cast<const expression::conditional *>(this);
    return expression_arena::array_ptr<expression *>(
        conditional->children_.data(),
        narrow_cast<int>(conditional->children_.size()));
  }
  case expression_kind::dot: {
    auto *dot = static_cast<const expression::dot *>(this);
    return expression_arena::array_ptr<expression *>(&dot->child_, 1);
  }
  case expression_kind::index: {
    auto *index = static_cast<const expression::index *>(this);
    return expression_arena::array_ptr<expression *>(
        index->children_.data(), narrow_cast<int>(index->children_.size()));
  }
  case expression_kind::paren: {
    auto *paren = static_cast<const expression::paren *>(this);
    return expression_arena::array_ptr<expression *>(&paren->child_, 1);
  }
  case expression_kind::rw_unary_suffix: {
    auto *rw_unary_suffix =
        static_cast<const expression::rw_unary_suffix *>(this);
    return expression_arena::array_ptr<expression *>(&rw_unary_suffix->child_,
                                                     1);
  }
  case expression_kind::tagged_template_literal:
    return static_cast<const expression::tagged_template_literal *>(this)
        ->tag_and_template_children_;
  case expression_kind::trailing_comma:
    return static_cast<const expression::trailing_comma *>(this)->children_;

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline expression *expression::without_paren() const noexcept {
  const expression *ast = this;
  while (ast->kind_ == expression_kind::paren) {
    ast = static_cast<const paren *>(ast)->child_;
  }
  // TODO(strager): Remove const_cast.
  return const_cast<expression *>(ast);
}

inline int expression::object_entry_count() const noexcept {
  switch (this->kind_) {
  case expression_kind::object:
    return static_cast<const expression::object *>(this)->entries_.size();

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline object_property_value_pair expression::object_entry(int index) const
    noexcept {
  switch (this->kind_) {
  case expression_kind::object:
    return static_cast<const expression::object *>(this)->entries_[index];

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}

inline source_code_span expression::span() const noexcept {
  switch (this->kind_) {
  case expression_kind::assignment:
  case expression_kind::compound_assignment:
  case expression_kind::conditional_assignment: {
    auto *assignment = static_cast<const expression::assignment *>(this);
    return source_code_span(assignment->children_.front()->span().begin(),
                            assignment->children_.back()->span().end());
  }

  case expression_kind::_delete:
  case expression_kind::_typeof:
  case expression_kind::await:
  case expression_kind::rw_unary_prefix:
  case expression_kind::spread:
  case expression_kind::unary_operator:
  case expression_kind::yield_many:
  case expression_kind::yield_one: {
    auto *prefix =
        static_cast<const expression::expression_with_prefix_operator_base *>(
            this);
    return source_code_span(prefix->unary_operator_begin_,
                            prefix->child_->span().end());
  }

  case expression_kind::jsx_element:
  case expression_kind::jsx_element_with_members:
  case expression_kind::jsx_element_with_namespace:
  case expression_kind::jsx_fragment:
    return static_cast<const jsx_base *>(this)->span;

  case expression_kind::_class:
    return static_cast<const _class *>(this)->span_;
  case expression_kind::_invalid:
    return static_cast<const _invalid *>(this)->span_;
  case expression_kind::_missing:
    return static_cast<const _missing *>(this)->span_;
  case expression_kind::_new:
    return static_cast<const _new *>(this)->span_;
  case expression_kind::_template:
    return static_cast<const _template *>(this)->span_;
  case expression_kind::array:
    return static_cast<const array *>(this)->span_;
  case expression_kind::arrow_function_with_expression: {
    auto *arrow =
        static_cast<const expression::arrow_function_with_expression *>(this);
    if (arrow->parameter_list_begin_) {
      return source_code_span(arrow->parameter_list_begin_,
                              arrow->body_->span().end());
    } else {
      return source_code_span(arrow->parameters_.front()->span().begin(),
                              arrow->body_->span().end());
    }
  }
  case expression_kind::arrow_function_with_statements: {
    auto *arrow =
        static_cast<const expression::arrow_function_with_statements *>(this);
    if (arrow->parameter_list_begin_) {
      return source_code_span(arrow->parameter_list_begin_, arrow->span_end_);
    } else {
      return source_code_span(arrow->children_.front()->span().begin(),
                              arrow->span_end_);
    }
  }
  case expression_kind::binary_operator: {
    auto *binary = static_cast<const expression::binary_operator *>(this);
    return source_code_span(binary->children_.front()->span().begin(),
                            binary->children_.back()->span().end());
  }
  case expression_kind::call: {
    auto *call = static_cast<const expression::call *>(this);
    return source_code_span(call->children_.front()->span().begin(),
                            call->span_end_);
  }
  case expression_kind::conditional: {
    auto *conditional = static_cast<const expression::conditional *>(this);
    return source_code_span(conditional->children_.front()->span().begin(),
                            conditional->children_.back()->span().end());
  }
  case expression_kind::dot: {
    auto *dot = static_cast<const expression::dot *>(this);
    return source_code_span(dot->child_0()->span().begin(),
                            dot->variable_identifier_.span().end());
  }
  case expression_kind::function:
    return static_cast<const function *>(this)->span_;
  case expression_kind::import:
    return static_cast<const import *>(this)->span_;
  case expression_kind::index: {
    auto *index = static_cast<const expression::index *>(this);
    return source_code_span(index->child_0()->span().begin(),
                            index->index_subscript_end_);
  }
  case expression_kind::literal:
    return static_cast<const literal *>(this)->span_;
  case expression_kind::named_function:
    return static_cast<const named_function *>(this)->span_;
  case expression_kind::new_target:
    return static_cast<const new_target *>(this)->span_;
  case expression_kind::object:
    return static_cast<const object *>(this)->span_;
  case expression_kind::paren:
    return static_cast<const paren *>(this)->span_;
  case expression_kind::private_variable:
    return static_cast<const private_variable *>(this)
        ->variable_identifier_.span();
  case expression_kind::rw_unary_suffix: {
    auto *suffix = static_cast<const rw_unary_suffix *>(this);
    return source_code_span(suffix->child_->span().begin(),
                            suffix->unary_operator_end_);
  }
  case expression_kind::super:
    return static_cast<const super *>(this)->span_;
  case expression_kind::tagged_template_literal: {
    auto *literal = static_cast<const tagged_template_literal *>(this);
    return source_code_span(
        literal->tag_and_template_children_[0]->span().begin(),
        literal->template_span_end_);
  }
  case expression_kind::trailing_comma: {
    auto *comma = static_cast<const trailing_comma *>(this);
    return source_code_span(comma->children_.front()->span().begin(),
                            comma->comma_end_);
  }
  case expression_kind::variable:
    return static_cast<const variable *>(this)->variable_identifier_.span();
  case expression_kind::yield_none:
    return static_cast<const yield_none *>(this)->span_;
  }
  QLJS_UNREACHABLE();
}

inline function_attributes expression::attributes() const noexcept {
  switch (this->kind_) {
  case expression_kind::arrow_function_with_expression:
    return static_cast<const expression::arrow_function_with_expression *>(this)
        ->function_attributes_;
  case expression_kind::arrow_function_with_statements:
    return static_cast<const expression::arrow_function_with_statements *>(this)
        ->function_attributes_;
  case expression_kind::function:
    return static_cast<const expression::function *>(this)
        ->function_attributes_;
  case expression_kind::named_function:
    return static_cast<const expression::named_function *>(this)
        ->function_attributes_;

  default:
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }
}
}

QLJS_WARNING_POP

#undef QLJS_UNEXPECTED_EXPRESSION_KIND

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
