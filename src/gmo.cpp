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

#include <cstdint>
#include <quick-lint-js/gmo.h>
#include <string_view>

namespace quick_lint_js {
gmo_file::gmo_file(const void *data) noexcept
    : data_(reinterpret_cast<const std::uint8_t *>(data)) {}

gmo_file::word_type gmo_file::string_count() const noexcept {
  return this->read_word(/*offset=*/0x08);
}

std::string_view gmo_file::original_string_at(word_type index) const noexcept {
  return this->string_at(/*table_offset=*/this->original_strings_offset(),
                         /*index=*/index);
}

std::string_view gmo_file::translated_string_at(word_type index) const
    noexcept {
  return this->string_at(/*table_offset=*/this->translated_strings_offset(),
                         /*index=*/index);
}

std::string_view gmo_file::find_translation(std::string_view original) const
    noexcept {
  if (this->hash_table_size() == 0) {
    return this->find_translation_scanning(original);
  } else {
    return this->find_translation_hashing(original);
  }
}

std::string_view gmo_file::find_translation_scanning(
    std::string_view original) const noexcept {
  for (word_type i = 0; i < this->string_count(); ++i) {
    if (this->original_string_at(i) == original) {
      return this->translated_string_at(i);
    }
  }
  return original;
}

std::string_view gmo_file::find_translation_hashing(
    std::string_view original) const noexcept {
  offset_type hash_table_offset = this->read_word(/*offset=*/0x18);
  word_type hash_table_size = this->hash_table_size();

  word_type hash = this->hash_string(original);
  word_type bucket_index = hash % hash_table_size;
  word_type probe_increment = 1 + (hash % (hash_table_size - 2));

  for (;;) {
    word_type string_number =
        this->read_word(/*offset=*/hash_table_offset + bucket_index * 4);
    if (string_number == 0) {
      break;
    }
    word_type string_index = string_number - 1;
    if (this->original_string_at(string_index) == original) {
      return this->translated_string_at(string_number - 1);
    }
    bucket_index = (bucket_index + probe_increment) % hash_table_size;
  }
  return original;
}

gmo_file::word_type gmo_file::hash_string(std::string_view s) noexcept {
  // This function implements the hashpjw routine documented in:
  //
  // Compilers: Principles, Techniques, and Tools, first edition (1986),
  // by Alfred V. Aho, Monica S. Lam, Ravi Sethi, and Jeffrey D. Ullman;
  // chapter 7 Run-Time Environments,
  // section 6 Symbol Tables,
  // figure 7.35,
  // page 436.

  std::uint32_t hash = 0;
  for (char c : s) {
    hash = (hash << 4) + static_cast<unsigned char>(c);
    std::uint32_t g = hash & 0xf0000000;
    hash ^= (g >> 24);
    hash ^= g;
  }
  return hash;
}

gmo_file::word_type gmo_file::read_word(word_type offset) const noexcept {
  if (this->is_little_endian()) {
    return this->read_word_little_endian(offset);
  } else {
    return this->read_word_big_endian(offset);
  }
}

gmo_file::word_type gmo_file::read_word_little_endian(word_type offset) const
    noexcept {
  return (word_type{this->data_[offset + 0]} << 0) |
         (word_type{this->data_[offset + 1]} << 8) |
         (word_type{this->data_[offset + 2]} << 16) |
         (word_type{this->data_[offset + 3]} << 24);
}

gmo_file::word_type gmo_file::read_word_big_endian(word_type offset) const
    noexcept {
  return (word_type{this->data_[offset + 3]} << 0) |
         (word_type{this->data_[offset + 2]} << 8) |
         (word_type{this->data_[offset + 1]} << 16) |
         (word_type{this->data_[offset + 0]} << 24);
}

gmo_file::word_type gmo_file::hash_table_size() const noexcept {
  return this->read_word(/*offset=*/0x14);
}

gmo_file::offset_type gmo_file::original_strings_offset() const noexcept {
  return this->read_word(/*offset=*/0x0c);
}

gmo_file::offset_type gmo_file::translated_strings_offset() const noexcept {
  return this->read_word(/*offset=*/0x10);
}

std::string_view gmo_file::string_at(offset_type table_offset,
                                     word_type index) const noexcept {
  offset_type table_entry_offset = table_offset + index * 0x8;
  word_type length = this->read_word(table_entry_offset + 0x0);
  word_type offset = this->read_word(table_entry_offset + 0x4);
  return std::string_view(reinterpret_cast<const char *>(&this->data_[offset]),
                          length);
}

bool gmo_file::is_little_endian() const noexcept {
  word_type magic = this->read_word_little_endian(/*offset=*/0);
  return magic == 0x950412de;
}
}
