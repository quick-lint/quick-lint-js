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
namespace {
template <endian E>
class native_gmo_file {
 public:
  using offset_type = gmo_file::offset_type;
  using word_type = gmo_file::word_type;

  explicit native_gmo_file(const std::uint8_t *data) noexcept : data_(data) {}

  word_type string_count() const noexcept {
    return this->read_word(/*offset=*/0x08);
  }

  std::string_view original_string_at(word_type index) const noexcept {
    return this->string_at(/*table_offset=*/this->read_word(/*offset=*/0x0c),
                           /*index=*/index);
  }

  std::string_view translated_string_at(word_type index) const noexcept {
    return this->string_at(/*table_offset=*/this->read_word(/*offset=*/0x10),
                           /*index=*/index);
  }

  std::string_view find_translation(gmo_message original) const noexcept {
    if (this->hash_table_size() == 0) {
      return this->find_translation_scanning(original.message);
    } else {
      return this->find_translation_hashing(original);
    }
  }

  std::string_view find_translation_scanning(std::string_view original) const
      noexcept {
    for (word_type i = 0; i < this->string_count(); ++i) {
      if (this->original_string_at(i) == original) {
        return this->translated_string_at(i);
      }
    }
    return original;
  }

  std::string_view find_translation_hashing(gmo_message original) const
      noexcept {
    offset_type hash_table_offset = this->read_word(/*offset=*/0x18);
    word_type hash_table_size = this->hash_table_size();

    word_type bucket_index = original.hash % hash_table_size;
    word_type probe_increment = 1 + (original.hash % (hash_table_size - 2));

    for (;;) {
      word_type string_number =
          this->read_word(/*offset=*/hash_table_offset + bucket_index * 4);
      if (string_number == 0) {
        break;
      }
      word_type string_index = string_number - 1;
      if (this->original_string_at(string_index) == original.message) {
        return this->translated_string_at(string_number - 1);
      }
      bucket_index = (bucket_index + probe_increment) % hash_table_size;
    }
    return original.message;
  }

  word_type read_word(word_type offset) const noexcept {
    const std::uint8_t *d = &this->data_[offset];
    if constexpr (E == endian::little) {
      return (word_type{d[0]} << 0) |   //
             (word_type{d[1]} << 8) |   //
             (word_type{d[2]} << 16) |  //
             (word_type{d[3]} << 24);
    } else {
      return (word_type{d[3]} << 0) |   //
             (word_type{d[2]} << 8) |   //
             (word_type{d[1]} << 16) |  //
             (word_type{d[0]} << 24);
    }
  }

  word_type hash_table_size() const noexcept {
    return this->read_word(/*offset=*/0x14);
  }

  std::string_view string_at(offset_type table_offset, word_type index) const
      noexcept {
    offset_type table_entry_offset = table_offset + index * 0x8;
    word_type length = this->read_word(table_entry_offset + 0x0);
    word_type offset = this->read_word(table_entry_offset + 0x4);
    return std::string_view(
        reinterpret_cast<const char *>(&this->data_[offset]), length);
  }

 private:
  const std::uint8_t *data_;
};
}

gmo_file::gmo_file(const void *data) noexcept
    : data_(reinterpret_cast<const std::uint8_t *>(data)) {}

#define QLJS_ENDIAN_DISPATCH(call)                              \
  do {                                                          \
    if (this->get_endian() == endian::little) {                 \
      return native_gmo_file<endian::little>(this->data_) call; \
    } else {                                                    \
      return native_gmo_file<endian::big>(this->data_) call;    \
    }                                                           \
  } while (false)

gmo_file::word_type gmo_file::string_count() const noexcept {
  QLJS_ENDIAN_DISPATCH(.string_count());
}

std::string_view gmo_file::original_string_at(word_type index) const noexcept {
  QLJS_ENDIAN_DISPATCH(.original_string_at(index));
}

std::string_view gmo_file::translated_string_at(word_type index) const
    noexcept {
  QLJS_ENDIAN_DISPATCH(.translated_string_at(index));
}

std::string_view gmo_file::find_translation(gmo_message original) const
    noexcept {
  QLJS_ENDIAN_DISPATCH(.find_translation(original));
}

endian gmo_file::get_endian() const noexcept {
  word_type magic =
      native_gmo_file<endian::little>(this->data_).read_word(/*offset=*/0);
  return magic == 0x950412de ? endian::little : endian::big;
}
}
