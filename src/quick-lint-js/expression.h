// Copyright (C) 2020  Matthew Glazar
// See end of file for extended copyright information.

#ifndef QUICK_LINT_JS_EXPRESSION_H
#define QUICK_LINT_JS_EXPRESSION_H

#include <array>
#include <deque>
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

namespace quick_lint_js {
class expression;

enum class expression_kind {
  _class,
  _invalid,
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
  dot,
  function,
  import,
  index,
  literal,
  named_function,
  new_target,
  object,
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
  static inline constexpr bool is_allocatable =
      std::is_trivially_destructible_v<std::remove_reference_t<T>>;

  template <class Expression, class... Args>
  expression *make_expression(Args &&... args);

  template <class T, std::size_t InSituCapacity>
  array_ptr<T> make_array(vector<T, InSituCapacity> &&);

  template <class T>
  array_ptr<T> make_array(T *begin, T *end);

  template <class T, std::size_t Size>
  array_ptr<T> make_array(std::array<T, Size> &&);

  buffering_visitor_ptr make_buffering_visitor() {
    // See matching 'delete' in delete_buffering_visitor.
    return new buffering_visitor();
  }

  void delete_buffering_visitor(buffering_visitor_ptr visitor) {
    // See matching 'new' in make_buffering_visitor.
    delete visitor;
  }

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

  class _class;
  class _invalid;
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
  class literal;
  class named_function;
  class new_target;
  class object;
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

  // Can be called at most once.
  template <QLJS_PARSE_VISITOR Visitor>
  void visit_children(Visitor &v, expression_arena &arena) {
    buffering_visitor *child_visits = this->with_derived(
        [](auto &self) { return self.take_child_visits_impl(); });
    QLJS_ASSERT(
        child_visits &&
        "visit_children can be called at most once, but it was called twice");
    child_visits->move_into(v);
    arena.delete_buffering_visitor(child_visits);
  }

  int object_entry_count() const noexcept;

  object_property_value_pair object_entry(int) const noexcept;

  source_code_span span() const noexcept;

  function_attributes attributes() const noexcept;

 protected:
  explicit expression(expression_kind kind) noexcept : kind_(kind) {}

  identifier variable_identifier_impl() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  token_type variable_identifier_token_type_impl() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  int child_count_impl() const noexcept { QLJS_UNEXPECTED_EXPRESSION_KIND(); }

  expression *child_impl(int) const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  int object_entry_count_impl() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  object_property_value_pair object_entry_impl(int) const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  source_code_span span_impl() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  function_attributes attributes_impl() const noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

  // Returns expression_arena::buffering_visitor_ptr.
  buffering_visitor *take_child_visits_impl() noexcept {
    QLJS_UNEXPECTED_EXPRESSION_KIND();
  }

 private:
  template <class Func>
  auto with_derived(Func &&func);

  template <class Func>
  auto with_derived(Func &&func) const;

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

template <class T, std::size_t InSituCapacity>
inline expression_arena::array_ptr<T> expression_arena::make_array(
    vector<T, InSituCapacity> &&elements) {
  array_ptr<T> result =
      this->make_array(elements.data(), elements.data() + elements.size());
  elements.clear();
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

  int child_count_impl() const noexcept { return 1; }

  expression *child_impl([[maybe_unused]] int index) const noexcept {
    QLJS_ASSERT(index == 0);
    return this->child_;
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->unary_operator_begin_,
                            this->child_->span().end());
  }

 protected:
  const char8 *unary_operator_begin_;
  expression *child_;
};

class expression::_class : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_class;

  explicit _class(expression_arena::buffering_visitor_ptr child_visits,
                  source_code_span span) noexcept
      : expression(kind), child_visits_(child_visits), span_(span) {}

  source_code_span span_impl() const noexcept { return this->span_; }

  expression_arena::buffering_visitor_ptr take_child_visits_impl() noexcept {
    return std::exchange(this->child_visits_, nullptr);
  }

 private:
  expression_arena::buffering_visitor_ptr child_visits_;
  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::_class>);

class expression::_invalid final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_invalid;

  explicit _invalid(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::_invalid>);

class expression::_new final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_new;

  explicit _new(expression_arena::array_ptr<expression *> children,
                source_code_span span) noexcept
      : expression(kind), span_(span), children_(children) {}

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression *child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
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

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression *child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
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

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression *child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
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
        parameter_list_begin_(parameter_list_begin),
        function_attributes_(attributes),
        body_(body) {}

  explicit arrow_function_with_expression(
      function_attributes attributes,
      expression_arena::array_ptr<expression *> parameters, expression *body,
      const char8 *parameter_list_begin) noexcept
      : expression(kind),
        parameter_list_begin_(parameter_list_begin),
        function_attributes_(attributes),
        parameters_(parameters),
        body_(body) {
    if (!this->parameter_list_begin_) {
      QLJS_ASSERT(!this->parameters_.empty());
    }
  }

  int child_count_impl() const noexcept { return this->parameters_.size() + 1; }

  expression *child_impl(int index) const noexcept {
    if (index == this->parameters_.size()) {
      return this->body_;
    } else {
      return this->parameters_[index];
    }
  }

  source_code_span span_impl() const noexcept {
    if (this->parameter_list_begin_) {
      return source_code_span(this->parameter_list_begin_,
                              this->body_->span().end());
    } else {
      return source_code_span(this->parameters_.front()->span().begin(),
                              this->body_->span().end());
    }
  }

  function_attributes attributes_impl() const noexcept {
    return this->function_attributes_;
  }

 private:
  const char8 *parameter_list_begin_;
  function_attributes function_attributes_;
  expression_arena::array_ptr<expression *> parameters_;
  expression *body_;
};
static_assert(expression_arena::is_allocatable<
              expression::arrow_function_with_expression>);

class expression::arrow_function_with_statements final : public expression {
 public:
  static constexpr expression_kind kind =
      expression_kind::arrow_function_with_statements;

  explicit arrow_function_with_statements(
      function_attributes attributes,
      expression_arena::buffering_visitor_ptr child_visits,
      const char8 *parameter_list_begin, const char8 *span_end) noexcept
      : expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        span_end_(span_end),
        child_visits_(child_visits) {
    QLJS_ASSERT(this->parameter_list_begin_);
  }

  explicit arrow_function_with_statements(
      function_attributes attributes,
      expression_arena::array_ptr<expression *> parameters,
      expression_arena::buffering_visitor_ptr child_visits,
      const char8 *parameter_list_begin, const char8 *span_end) noexcept
      : expression(kind),
        function_attributes_(attributes),
        parameter_list_begin_(parameter_list_begin),
        span_end_(span_end),
        child_visits_(child_visits),
        children_(parameters) {
    if (!this->parameter_list_begin_) {
      QLJS_ASSERT(!this->children_.empty());
    }
  }

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression *child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept {
    if (this->parameter_list_begin_) {
      return source_code_span(this->parameter_list_begin_, this->span_end_);
    } else {
      return source_code_span(this->children_.front()->span().begin(),
                              span_end_);
    }
  }

  function_attributes attributes_impl() const noexcept {
    return this->function_attributes_;
  }

  expression_arena::buffering_visitor_ptr take_child_visits_impl() noexcept {
    return std::exchange(this->child_visits_, nullptr);
  }

 private:
  function_attributes function_attributes_;
  const char8 *parameter_list_begin_;
  const char8 *span_end_;
  expression_arena::buffering_visitor_ptr child_visits_;
  expression_arena::array_ptr<expression *> children_;
};
static_assert(expression_arena::is_allocatable<
              expression::arrow_function_with_statements>);

class expression::assignment final : public expression {
 public:
  explicit assignment(expression_kind kind, expression *lhs,
                      expression *rhs) noexcept
      : expression(kind), children_{lhs, rhs} {
    QLJS_ASSERT(kind == expression_kind::assignment ||
                kind == expression_kind::compound_assignment);
  }

  int child_count_impl() const noexcept {
    return narrow_cast<int>(this->children_.size());
  }

  expression *child_impl(int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index < static_cast<int>(this->children_.size()));
    return this->children_[narrow_cast<unsigned>(index)];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  std::array<expression *, 2> children_;
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

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression *child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
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

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression *child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->span_end_);
  }

  source_code_span left_paren_span() const noexcept {
    return source_code_span(this->call_left_paren_begin_,
                            this->call_left_paren_begin_ + 1);
  }

 private:
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

  int child_count_impl() const noexcept {
    return narrow_cast<int>(this->children_.size());
  }

  expression *child_impl(int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index < static_cast<int>(this->children_.size()));
    return this->children_[narrow_cast<unsigned>(index)];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  std::array<expression *, 3> children_;
};
static_assert(expression_arena::is_allocatable<expression::conditional>);

class expression::dot final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::dot;

  explicit dot(expression *lhs, identifier rhs) noexcept
      : expression(kind), variable_identifier_(rhs), child_(lhs) {}

  int child_count_impl() const noexcept { return 1; }

  expression *child_impl([[maybe_unused]] int index) const noexcept {
    QLJS_ASSERT(index == 0);
    return this->child_;
  }

  identifier variable_identifier_impl() const noexcept {
    return this->variable_identifier_;
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->child_0()->span().begin(),
                            this->variable_identifier_.span().end());
  }

 private:
  identifier variable_identifier_;
  expression *child_;
};
static_assert(expression_arena::is_allocatable<expression::dot>);

class expression::function final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::function;

  explicit function(function_attributes attributes,
                    expression_arena::buffering_visitor_ptr child_visits,
                    source_code_span span) noexcept
      : expression(kind),
        function_attributes_(attributes),
        child_visits_(child_visits),
        span_(span) {}

  source_code_span span_impl() const noexcept { return this->span_; }

  function_attributes attributes_impl() const noexcept {
    return this->function_attributes_;
  }

  expression_arena::buffering_visitor_ptr take_child_visits_impl() noexcept {
    return std::exchange(this->child_visits_, nullptr);
  }

 private:
  function_attributes function_attributes_;
  expression_arena::buffering_visitor_ptr child_visits_;
  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::function>);

class expression::import final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::import;

  explicit import(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
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

  int child_count_impl() const noexcept {
    return narrow_cast<int>(this->children_.size());
  }

  expression *child_impl(int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index < static_cast<int>(this->children_.size()));
    return this->children_[narrow_cast<unsigned>(index)];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->child_0()->span().begin(),
                            this->index_subscript_end_);
  }

 private:
  const char8 *index_subscript_end_;
  std::array<expression *, 2> children_;
};
static_assert(expression_arena::is_allocatable<expression::index>);

class expression::literal final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::literal;

  explicit literal(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::literal>);

class expression::named_function final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::named_function;

  explicit named_function(function_attributes attributes, identifier name,
                          expression_arena::buffering_visitor_ptr child_visits,
                          source_code_span span) noexcept
      : expression(kind),
        function_attributes_(attributes),
        child_visits_(child_visits),
        variable_identifier_(name),
        span_(span) {}

  identifier variable_identifier_impl() const noexcept {
    return this->variable_identifier_;
  }

  source_code_span span_impl() const noexcept { return this->span_; }

  function_attributes attributes_impl() const noexcept {
    return this->function_attributes_;
  }

  expression_arena::buffering_visitor_ptr take_child_visits_impl() noexcept {
    return std::exchange(this->child_visits_, nullptr);
  }

 private:
  function_attributes function_attributes_;
  expression_arena::buffering_visitor_ptr child_visits_;
  identifier variable_identifier_;
  source_code_span span_;
};
static_assert(expression_arena::is_allocatable<expression::named_function>);

class expression::new_target final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::new_target;

  explicit new_target(source_code_span span) noexcept
      : expression(kind), span_(span) {}

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
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

  int object_entry_count_impl() const noexcept { return this->entries_.size(); }

  object_property_value_pair object_entry_impl(int index) const noexcept {
    return this->entries_[index];
  }

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
  source_code_span span_;
  expression_arena::array_ptr<object_property_value_pair> entries_;
};
static_assert(expression_arena::is_allocatable<expression::object>);

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

  int child_count_impl() const noexcept { return 1; }

  expression *child_impl([[maybe_unused]] int index) const noexcept {
    QLJS_ASSERT(index == 0);
    return this->child_;
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->child_->span().begin(),
                            this->unary_operator_end_);
  }

 private:
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

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
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

  int child_count_impl() const noexcept {
    return this->tag_and_template_children_.size();
  }

  expression *child_impl(int index) const noexcept {
    return this->tag_and_template_children_[index];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->tag_and_template_children_[0]->span().begin(),
                            this->template_span_end_);
  }

 private:
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

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression *child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->comma_end_);
  }

  source_code_span comma_span() const noexcept {
    return source_code_span(this->comma_end_ - 1, this->comma_end_);
  }

 private:
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

  identifier variable_identifier_impl() const noexcept {
    return this->variable_identifier_;
  }

  token_type variable_identifier_token_type_impl() const noexcept {
    return this->type_;
  }

  source_code_span span_impl() const noexcept {
    return this->variable_identifier_.span();
  }

 private:
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

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
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

template <class Func>
inline auto expression::with_derived(Func &&func) {
  using compound_assignment = assignment;

#define QLJS_EXPRESSION_CASE(kind) \
  case expression_kind::kind:      \
    return func(static_cast<kind &>(*this));

  switch (this->kind()) {
    QLJS_EXPRESSION_CASE(_class)
    QLJS_EXPRESSION_CASE(_invalid)
    QLJS_EXPRESSION_CASE(_new)
    QLJS_EXPRESSION_CASE(_template)
    QLJS_EXPRESSION_CASE(_typeof)
    QLJS_EXPRESSION_CASE(array)
    QLJS_EXPRESSION_CASE(arrow_function_with_expression)
    QLJS_EXPRESSION_CASE(arrow_function_with_statements)
    QLJS_EXPRESSION_CASE(assignment)
    QLJS_EXPRESSION_CASE(await)
    QLJS_EXPRESSION_CASE(binary_operator)
    QLJS_EXPRESSION_CASE(call)
    QLJS_EXPRESSION_CASE(compound_assignment)
    QLJS_EXPRESSION_CASE(conditional)
    QLJS_EXPRESSION_CASE(dot)
    QLJS_EXPRESSION_CASE(function)
    QLJS_EXPRESSION_CASE(import)
    QLJS_EXPRESSION_CASE(index)
    QLJS_EXPRESSION_CASE(literal)
    QLJS_EXPRESSION_CASE(named_function)
    QLJS_EXPRESSION_CASE(new_target)
    QLJS_EXPRESSION_CASE(object)
    QLJS_EXPRESSION_CASE(rw_unary_prefix)
    QLJS_EXPRESSION_CASE(rw_unary_suffix)
    QLJS_EXPRESSION_CASE(spread)
    QLJS_EXPRESSION_CASE(super)
    QLJS_EXPRESSION_CASE(tagged_template_literal)
    QLJS_EXPRESSION_CASE(trailing_comma)
    QLJS_EXPRESSION_CASE(unary_operator)
    QLJS_EXPRESSION_CASE(variable)
    QLJS_EXPRESSION_CASE(yield_many)
    QLJS_EXPRESSION_CASE(yield_none)
    QLJS_EXPRESSION_CASE(yield_one)
  }
  QLJS_UNREACHABLE();

#undef QLJS_EXPRESSION_CASE
}

template <class Func>
inline auto expression::with_derived(Func &&func) const {
  return const_cast<expression *>(this)->with_derived(
      [&func](const auto &self) { return func(self); });
}

inline identifier expression::variable_identifier() const noexcept {
  return this->with_derived(
      [](const auto &self) { return self.variable_identifier_impl(); });
}

inline token_type expression::variable_identifier_token_type() const noexcept {
  return this->with_derived([](const auto &self) {
    return self.variable_identifier_token_type_impl();
  });
}

inline int expression::child_count() const noexcept {
  return this->with_derived(
      [](const auto &self) { return self.child_count_impl(); });
}

inline expression *expression::child(int index) const noexcept {
  return this->with_derived(
      [&](const auto &self) { return self.child_impl(index); });
}

inline int expression::object_entry_count() const noexcept {
  return this->with_derived(
      [](const auto &self) { return self.object_entry_count_impl(); });
}

inline object_property_value_pair expression::object_entry(int index) const
    noexcept {
  return this->with_derived(
      [&](const auto &self) { return self.object_entry_impl(index); });
}

inline source_code_span expression::span() const noexcept {
  return this->with_derived([](const auto &self) { return self.span_impl(); });
}

inline function_attributes expression::attributes() const noexcept {
  return this->with_derived(
      [](const auto &self) { return self.attributes_impl(); });
}
}

#undef QLJS_UNEXPECTED_EXPRESSION_KIND

#endif

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew Glazar
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
