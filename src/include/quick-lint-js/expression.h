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

#ifndef QUICK_LINT_JS_EXPRESSION_H
#define QUICK_LINT_JS_EXPRESSION_H

#include <array>
#include <boost/container/pmr/monotonic_buffer_resource.hpp>
#include <boost/container/pmr/polymorphic_allocator.hpp>
#include <deque>
#include <memory>
#include <optional>
#include <quick-lint-js/assert.h>
#include <quick-lint-js/buffering-visitor.h>
#include <quick-lint-js/char8.h>
#include <quick-lint-js/lex.h>
#include <quick-lint-js/location.h>
#include <quick-lint-js/narrow-cast.h>
#include <quick-lint-js/parse-visitor.h>
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

class expression_ptr {
 public:
  explicit expression_ptr(expression *ptr) noexcept : ptr_(ptr) {}

  expression *operator->() const noexcept { return this->ptr_; }
  expression &operator*() const noexcept { return *this->ptr_; }

  bool operator==(expression_ptr other) const noexcept {
    return this->ptr_ == other.ptr_;
  }

  bool operator!=(expression_ptr other) const noexcept {
    return !(*this == other);
  }

 private:
  expression *ptr_;
};

enum class expression_kind {
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
  object,
  rw_unary_prefix,
  rw_unary_suffix,
  spread,
  super,
  unary_operator,
  variable,
};

struct object_property_value_pair {
  explicit object_property_value_pair(expression_ptr property,
                                      expression_ptr value) noexcept
      : property(property), value(value) {}

  explicit object_property_value_pair(expression_ptr value) noexcept
      : property(std::nullopt), value(value) {}

  std::optional<expression_ptr> property;
  expression_ptr value;
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
  expression_ptr make_expression(Args &&... args);

  template <class T>
  array_ptr<T> make_array(std::vector<T> &&);

  template <class T, std::size_t InSituCapacity>
  array_ptr<T> make_array(vector<T, InSituCapacity> &&);

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
    boost::container::pmr::polymorphic_allocator<T> allocator(&this->memory_);
    T *result = allocator.allocate(1);
    result = new (result) T(std::forward<Args>(args)...);
    return result;
  }

  template <class T>
  T *allocate_array_move(T *begin, T *end) {
    static_assert(is_allocatable<T>);
    boost::container::pmr::polymorphic_allocator<T> allocator(&this->memory_);
    T *result = allocator.allocate(narrow_cast<std::size_t>(end - begin));
    std::uninitialized_move(begin, end, result);
    return result;
  }

  boost::container::pmr::monotonic_buffer_resource memory_;
};

class expression {
 public:
  class expression_with_prefix_operator_base;

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
  class object;
  class rw_unary_prefix;
  class rw_unary_suffix;
  class spread;
  class super;
  class unary_operator;
  class variable;

  expression_kind kind() const noexcept { return this->kind_; }

  identifier variable_identifier() const noexcept;

  int child_count() const noexcept;

  expression_ptr child_0() const noexcept { return this->child(0); }
  expression_ptr child_1() const noexcept { return this->child(1); }
  expression_ptr child_2() const noexcept { return this->child(2); }

  expression_ptr child(int) const noexcept;

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

  int child_count_impl() const noexcept { QLJS_UNEXPECTED_EXPRESSION_KIND(); }

  expression_ptr child_impl(int) const noexcept {
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
expression_ptr expression_arena::make_expression(Args &&... args) {
  expression_ptr result(
      this->allocate<Expression>(std::forward<Args>(args)...));
  static_assert(is_allocatable<Expression>);
  return result;
}

template <class T>
inline expression_arena::array_ptr<T> expression_arena::make_array(
    std::vector<T> &&elements) {
  T *result_begin = this->allocate_array_move(
      elements.data(), elements.data() + elements.size());
  int size = narrow_cast<int>(elements.size());
  elements.clear();
  return array_ptr<T>(result_begin, size);
}

template <class T, std::size_t InSituCapacity>
inline expression_arena::array_ptr<T> expression_arena::make_array(
    vector<T, InSituCapacity> &&elements) {
  T *result_begin = this->allocate_array_move(
      elements.data(), elements.data() + elements.size());
  int size = narrow_cast<int>(elements.size());
  elements.clear();
  return array_ptr<T>(result_begin, size);
}

class expression::expression_with_prefix_operator_base : public expression {
 public:
  explicit expression_with_prefix_operator_base(
      expression_kind kind, expression_ptr child,
      source_code_span operator_span) noexcept
      : expression(kind),
        unary_operator_begin_(operator_span.begin()),
        child_(child) {}

  int child_count_impl() const noexcept { return 1; }

  expression_ptr child_impl([[maybe_unused]] int index) const noexcept {
    QLJS_ASSERT(index == 0);
    return this->child_;
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->unary_operator_begin_,
                            this->child_->span().end());
  }

 private:
  const char8 *unary_operator_begin_;
  expression_ptr child_;
};

class expression::_invalid final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_invalid;

  explicit _invalid() noexcept : expression(kind) {}

  source_code_span span_impl() const noexcept {
    QLJS_ASSERT(false && "Not yet implemented");
    QLJS_UNREACHABLE();
  }
};
static_assert(expression_arena::is_allocatable<expression::_invalid>);

class expression::_new final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_new;

  explicit _new(expression_arena::array_ptr<expression_ptr> children,
                source_code_span span) noexcept
      : expression(kind), span_(span), children_(children) {}

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression_ptr child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
  source_code_span span_;
  expression_arena::array_ptr<expression_ptr> children_;
};
static_assert(expression_arena::is_allocatable<expression::_new>);

class expression::_template final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::_template;

  explicit _template(expression_arena::array_ptr<expression_ptr> children,
                     source_code_span span) noexcept
      : expression(kind), span_(span), children_(children) {}

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression_ptr child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
  source_code_span span_;
  expression_arena::array_ptr<expression_ptr> children_;
};
static_assert(expression_arena::is_allocatable<expression::_template>);

class expression::_typeof final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::_typeof;

  explicit _typeof(expression_ptr child,
                   source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::_typeof>);

class expression::array final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::array;

  explicit array(expression_arena::array_ptr<expression_ptr> children,
                 source_code_span span) noexcept
      : expression(kind), span_(span), children_(children) {}

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression_ptr child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept { return this->span_; }

 private:
  source_code_span span_;
  expression_arena::array_ptr<expression_ptr> children_;
};
static_assert(expression_arena::is_allocatable<expression::array>);

class expression::arrow_function_with_expression final : public expression {
 public:
  static constexpr expression_kind kind =
      expression_kind::arrow_function_with_expression;

  explicit arrow_function_with_expression(
      function_attributes attributes, expression_ptr body,
      const char8 *parameter_list_begin) noexcept
      : expression(kind),
        parameter_list_begin_(parameter_list_begin),
        function_attributes_(attributes),
        body_(body) {}

  explicit arrow_function_with_expression(
      function_attributes attributes,
      expression_arena::array_ptr<expression_ptr> parameters,
      expression_ptr body, const char8 *parameter_list_begin) noexcept
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

  expression_ptr child_impl(int index) const noexcept {
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
  expression_arena::array_ptr<expression_ptr> parameters_;
  expression_ptr body_;
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
      expression_arena::array_ptr<expression_ptr> parameters,
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

  expression_ptr child_impl(int index) const noexcept {
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
  expression_arena::array_ptr<expression_ptr> children_;
};
static_assert(expression_arena::is_allocatable<
              expression::arrow_function_with_statements>);

class expression::assignment final : public expression {
 public:
  explicit assignment(expression_kind kind, expression_ptr lhs,
                      expression_ptr rhs) noexcept
      : expression(kind), children_{lhs, rhs} {
    QLJS_ASSERT(kind == expression_kind::assignment ||
                kind == expression_kind::compound_assignment);
  }

  int child_count_impl() const noexcept {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child_impl(int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index < static_cast<int>(this->children_.size()));
    return this->children_[narrow_cast<unsigned>(index)];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  std::array<expression_ptr, 2> children_;
};
static_assert(expression_arena::is_allocatable<expression::assignment>);

class expression::await final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::await;

  explicit await(expression_ptr child, source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::await>);

class expression::binary_operator final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::binary_operator;

  explicit binary_operator(
      expression_arena::array_ptr<expression_ptr> children) noexcept
      : expression(kind), children_(children) {}

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression_ptr child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  expression_arena::array_ptr<expression_ptr> children_;
};
static_assert(expression_arena::is_allocatable<expression::binary_operator>);

class expression::call final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::call;

  explicit call(expression_arena::array_ptr<expression_ptr> children,
                source_code_span span) noexcept
      : expression(kind),
        call_right_paren_end_(span.end()),
        children_(children) {}

  int child_count_impl() const noexcept { return this->children_.size(); }

  expression_ptr child_impl(int index) const noexcept {
    return this->children_[index];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->call_right_paren_end_);
  }

 private:
  const char8 *call_right_paren_end_;
  expression_arena::array_ptr<expression_ptr> children_;
};
static_assert(expression_arena::is_allocatable<expression::call>);

class expression::conditional final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::conditional;

  explicit conditional(expression_ptr condition, expression_ptr true_branch,
                       expression_ptr false_branch) noexcept
      : expression(kind), children_{condition, true_branch, false_branch} {}

  int child_count_impl() const noexcept {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child_impl(int index) const noexcept {
    QLJS_ASSERT(index >= 0);
    QLJS_ASSERT(index < static_cast<int>(this->children_.size()));
    return this->children_[narrow_cast<unsigned>(index)];
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->children_.front()->span().begin(),
                            this->children_.back()->span().end());
  }

 private:
  std::array<expression_ptr, 3> children_;
};
static_assert(expression_arena::is_allocatable<expression::conditional>);

class expression::dot final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::dot;

  explicit dot(expression_ptr lhs, identifier rhs) noexcept
      : expression(kind), variable_identifier_(rhs), child_(lhs) {}

  int child_count_impl() const noexcept { return 1; }

  expression_ptr child_impl([[maybe_unused]] int index) const noexcept {
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
  expression_ptr child_;
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

  explicit index(expression_ptr container, expression_ptr subscript,
                 const char8 *subscript_end) noexcept
      : expression(kind),
        index_subscript_end_(subscript_end),
        children_{container, subscript} {}

  int child_count_impl() const noexcept {
    return narrow_cast<int>(this->children_.size());
  }

  expression_ptr child_impl(int index) const noexcept {
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
  std::array<expression_ptr, 2> children_;
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

  explicit rw_unary_prefix(expression_ptr child,
                           source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::rw_unary_prefix>);

class expression::rw_unary_suffix final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::rw_unary_suffix;

  explicit rw_unary_suffix(expression_ptr child,
                           source_code_span operator_span) noexcept
      : expression(kind),
        unary_operator_end_(operator_span.end()),
        child_(child) {}

  int child_count_impl() const noexcept { return 1; }

  expression_ptr child_impl([[maybe_unused]] int index) const noexcept {
    QLJS_ASSERT(index == 0);
    return this->child_;
  }

  source_code_span span_impl() const noexcept {
    return source_code_span(this->child_->span().begin(),
                            this->unary_operator_end_);
  }

 private:
  const char8 *unary_operator_end_;
  expression_ptr child_;
};
static_assert(expression_arena::is_allocatable<expression::rw_unary_suffix>);

class expression::spread final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::spread;

  explicit spread(expression_ptr child, source_code_span operator_span) noexcept
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

class expression::unary_operator final
    : public expression::expression_with_prefix_operator_base {
 public:
  static constexpr expression_kind kind = expression_kind::unary_operator;

  explicit unary_operator(expression_ptr child,
                          source_code_span operator_span) noexcept
      : expression::expression_with_prefix_operator_base(kind, child,
                                                         operator_span) {}
};
static_assert(expression_arena::is_allocatable<expression::unary_operator>);

class expression::variable final : public expression {
 public:
  static constexpr expression_kind kind = expression_kind::variable;

  explicit variable(identifier variable_identifier) noexcept
      : expression(kind), variable_identifier_(variable_identifier) {}

  identifier variable_identifier_impl() const noexcept {
    return this->variable_identifier_;
  }

  source_code_span span_impl() const noexcept {
    return this->variable_identifier_.span();
  }

 private:
  identifier variable_identifier_;
};
static_assert(expression_arena::is_allocatable<expression::variable>);

template <class Func>
inline auto expression::with_derived(Func &&func) {
  using compound_assignment = assignment;

#define QLJS_EXPRESSION_CASE(kind) \
  case expression_kind::kind:      \
    return func(static_cast<kind &>(*this));

  switch (this->kind()) {
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
    QLJS_EXPRESSION_CASE(object)
    QLJS_EXPRESSION_CASE(rw_unary_prefix)
    QLJS_EXPRESSION_CASE(rw_unary_suffix)
    QLJS_EXPRESSION_CASE(spread)
    QLJS_EXPRESSION_CASE(super)
    QLJS_EXPRESSION_CASE(unary_operator)
    QLJS_EXPRESSION_CASE(variable)
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

inline int expression::child_count() const noexcept {
  return this->with_derived(
      [](const auto &self) { return self.child_count_impl(); });
}

inline expression_ptr expression::child(int index) const noexcept {
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
