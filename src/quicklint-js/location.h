#ifndef QUICKLINT_JS_LOCATION_H
#define QUICKLINT_JS_LOCATION_H

#include <cstddef>
#include <string_view>

namespace quicklint_js {
class source_range {
 public:
  using offset = std::size_t;

  source_range(offset begin_offset, offset end_offset) noexcept
      : begin_offset_(begin_offset), end_offset_(end_offset) {}

  offset begin_offset() const noexcept { return this->begin_offset_; }
  offset end_offset() const noexcept { return this->end_offset_; }

 private:
  offset begin_offset_;
  offset end_offset_;
};

class source_code_span {
 public:
  explicit source_code_span(const char* begin, const char* end) noexcept
      : begin_(begin), end_(end) {}

  const char* begin() const noexcept { return this->begin_; }

  const char* end() const noexcept { return this->end_; }

  std::string_view string_view() const noexcept {
    return std::string_view(this->begin(), this->end() - this->begin());
  }

 private:
  const char* begin_;
  const char* end_;
};

class locator {
 public:
  explicit locator(const char* input) noexcept : input_(input) {}

  source_range range(source_code_span) const;

 private:
  const char* input_;
};
}

#endif
