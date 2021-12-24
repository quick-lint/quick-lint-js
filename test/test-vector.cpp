// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <boost/container/pmr/global_resource.hpp>
#include <cstddef>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <map>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/vector.h>
#include <sstream>

#if QLJS_FEATURE_VECTOR_PROFILING
#define FIELD_EQ(_class, _member, ...) \
  FIELD(_class, _member, decltype(_class::_member){__VA_ARGS__})

#define FIELD(_class, _member, ...) \
  (::testing::Field(#_member, &_class::_member, __VA_ARGS__))
#endif

using boost::container::pmr::new_delete_resource;
using ::testing::AllOf;
using ::testing::ElementsAre;
using ::testing::Ge;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Key;
using ::testing::UnorderedElementsAre;

namespace quick_lint_js {
namespace {
#if QLJS_FEATURE_VECTOR_PROFILING
class test_vector_instrumentation : public ::testing::Test {
 public:
  void SetUp() override { vector_instrumentation::instance.clear(); }
};

TEST_F(test_vector_instrumentation,
       creating_and_destroying_empty_vector_adds_entries) {
  const char *owner = "test vector";
  std::uintptr_t v_object_id;
  {
    vector<int> v(owner, new_delete_resource());
    v_object_id = reinterpret_cast<std::uintptr_t>(&v);
  }

  EXPECT_THAT(
      vector_instrumentation::instance.entries(),
      ElementsAre(
          AllOf(FIELD_EQ(vector_instrumentation::entry, object_id, v_object_id),
                FIELD_EQ(vector_instrumentation::entry, owner, owner),
                FIELD_EQ(vector_instrumentation::entry, event,
                         vector_instrumentation::event::create),
                FIELD_EQ(vector_instrumentation::entry, size, 0)),
          AllOf(FIELD_EQ(vector_instrumentation::entry, object_id, v_object_id),
                FIELD_EQ(vector_instrumentation::entry, owner, owner),
                FIELD_EQ(vector_instrumentation::entry, event,
                         vector_instrumentation::event::destroy))));
}

TEST_F(test_vector_instrumentation, creating_vector_from_range_adds_entry) {
  int data[3] = {1, 2, 3};
  const char *owner = "test vector";

  vector<int> v(owner, new_delete_resource(), &data[0], &data[3]);

  std::uintptr_t v_object_id = reinterpret_cast<std::uintptr_t>(&v);
  EXPECT_THAT(
      vector_instrumentation::instance.entries(),
      ElementsAre(
          AllOf(FIELD_EQ(vector_instrumentation::entry, object_id, v_object_id),
                FIELD_EQ(vector_instrumentation::entry, owner, owner),
                FIELD_EQ(vector_instrumentation::entry, event,
                         vector_instrumentation::event::create),
                FIELD_EQ(vector_instrumentation::entry, size, 3),
                FIELD(vector_instrumentation::entry, capacity, Ge(3)))));
}

TEST_F(test_vector_instrumentation, append_to_vector_adds_entries) {
  vector<int> v("test vector", new_delete_resource());
  vector_instrumentation::instance.clear();

  v.emplace_back(100);
  v.emplace_back(200);
  v.emplace_back(300);
  v.emplace_back(400);

  EXPECT_THAT(
      vector_instrumentation::instance.entries(),
      ElementsAre(AllOf(FIELD_EQ(vector_instrumentation::entry, event,
                                 vector_instrumentation::event::append),
                        FIELD_EQ(vector_instrumentation::entry, size, 1)),
                  AllOf(FIELD_EQ(vector_instrumentation::entry, event,
                                 vector_instrumentation::event::append),
                        FIELD_EQ(vector_instrumentation::entry, size, 2)),
                  AllOf(FIELD_EQ(vector_instrumentation::entry, event,
                                 vector_instrumentation::event::append),
                        FIELD_EQ(vector_instrumentation::entry, size, 3)),
                  AllOf(FIELD_EQ(vector_instrumentation::entry, event,
                                 vector_instrumentation::event::append),
                        FIELD_EQ(vector_instrumentation::entry, size, 4))));
}

TEST_F(test_vector_instrumentation, clearing_vector_adds_entry) {
  vector<int> v("test vector", new_delete_resource());
  v.emplace_back(100);
  v.emplace_back(200);
  vector_instrumentation::instance.clear();

  v.clear();

  EXPECT_THAT(
      vector_instrumentation::instance.entries(),
      ElementsAre(AllOf(FIELD_EQ(vector_instrumentation::entry, event,
                                 vector_instrumentation::event::clear),
                        FIELD_EQ(vector_instrumentation::entry, size, 0))));
}

TEST_F(test_vector_instrumentation, moving_vector_with_new_owner_adds_entries) {
  const char *v_1_owner = "v1";
  vector<int> v_1(v_1_owner, new_delete_resource());
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  v_1.emplace_back(200);
  vector_instrumentation::instance.clear();

  const char *v_2_owner = "v2";
  vector<int> v_2(v_2_owner, std::move(v_1));
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);

  EXPECT_THAT(
      vector_instrumentation::instance.entries(),
      ElementsAre(
          AllOf(
              FIELD_EQ(vector_instrumentation::entry, owner, v_2_owner),
              FIELD_EQ(vector_instrumentation::entry, object_id, v_2_object_id),
              FIELD_EQ(vector_instrumentation::entry, event,
                       vector_instrumentation::event::create),
              FIELD_EQ(vector_instrumentation::entry, size, 2)),
          AllOf(
              FIELD_EQ(vector_instrumentation::entry, owner, v_1_owner),
              FIELD_EQ(vector_instrumentation::entry, object_id, v_1_object_id),
              FIELD_EQ(vector_instrumentation::entry, event,
                       vector_instrumentation::event::clear),
              FIELD_EQ(vector_instrumentation::entry, size, 0))));
}

TEST_F(test_vector_instrumentation, moving_vector_with_no_owner_adds_entries) {
  const char *v_1_owner = "v1";
  vector<int> v_1(v_1_owner, new_delete_resource());
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  v_1.emplace_back(200);
  vector_instrumentation::instance.clear();

  vector<int> v_2(std::move(v_1));
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);

  EXPECT_THAT(
      vector_instrumentation::instance.entries(),
      ElementsAre(
          AllOf(
              FIELD_EQ(vector_instrumentation::entry, owner, v_1_owner),
              FIELD_EQ(vector_instrumentation::entry, object_id, v_2_object_id),
              FIELD_EQ(vector_instrumentation::entry, event,
                       vector_instrumentation::event::create),
              FIELD_EQ(vector_instrumentation::entry, size, 2)),
          AllOf(
              FIELD_EQ(vector_instrumentation::entry, owner, v_1_owner),
              FIELD_EQ(vector_instrumentation::entry, object_id, v_1_object_id),
              FIELD_EQ(vector_instrumentation::entry, event,
                       vector_instrumentation::event::clear),
              FIELD_EQ(vector_instrumentation::entry, size, 0))));
}

TEST_F(test_vector_instrumentation, move_assigning_vector_adds_entries) {
  const char *v_1_owner = "v1";
  vector<int> v_1(v_1_owner, new_delete_resource());
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  const char *v_2_owner = "v2";
  vector<int> v_2(v_2_owner, new_delete_resource());
  v_2.emplace_back(200);
  v_2.emplace_back(300);
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);
  vector_instrumentation::instance.clear();

  v_1 = std::move(v_2);

  EXPECT_THAT(
      vector_instrumentation::instance.entries(),
      ElementsAre(
          AllOf(
              FIELD_EQ(vector_instrumentation::entry, owner, v_1_owner),
              FIELD_EQ(vector_instrumentation::entry, object_id, v_1_object_id),
              FIELD_EQ(vector_instrumentation::entry, event,
                       vector_instrumentation::event::assign),
              FIELD_EQ(vector_instrumentation::entry, size, 2)),
          AllOf(
              FIELD_EQ(vector_instrumentation::entry, owner, v_2_owner),
              FIELD_EQ(vector_instrumentation::entry, object_id, v_2_object_id),
              FIELD_EQ(vector_instrumentation::entry, event,
                       vector_instrumentation::event::clear),
              FIELD_EQ(vector_instrumentation::entry, size, 0))));
}
#else
// Indicate that the real tests have been disabled because
// QLJS_FEATURE_VECTOR_PROFILING is disabled.
TEST(test_vector_instrumentation, DISABLED_) {}
#endif

TEST(test_vector_instrumentation_max_size_histogram_by_owner, no_events) {
  vector_instrumentation data;
  auto histogram = data.max_size_histogram_by_owner();
  EXPECT_THAT(histogram, IsEmpty());
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner,
     distinctly_owned_vectors_with_one_event_each) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"first",
      /*event=*/vector_instrumentation::event::create,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/2,
      /*owner=*/"second",
      /*event=*/vector_instrumentation::event::create,
      /*size=*/5,
      /*capacity=*/5);
  data.add_entry(
      /*object_id=*/3,
      /*owner=*/"third",
      /*event=*/vector_instrumentation::event::create,
      /*size=*/0,
      /*capacity=*/0);

  auto histogram = data.max_size_histogram_by_owner();
  EXPECT_THAT(histogram,
              UnorderedElementsAre(Key("first"), Key("second"), Key("third")));
  EXPECT_THAT(histogram["first"], UnorderedElementsAre(std::pair(3, 1)));
  EXPECT_THAT(histogram["second"], UnorderedElementsAre(std::pair(5, 1)));
  EXPECT_THAT(histogram["third"], UnorderedElementsAre(std::pair(0, 1)));
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner,
     resizing_vector_keeps_maximum_size) {
  std::uint64_t object_id = 42;
  const char *owner = "test vector";
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::append,
      /*size=*/4,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::append,
      /*size=*/5,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::clear,
      /*size=*/0,
      /*capacity=*/10);

  auto histogram = data.max_size_histogram_by_owner();
  EXPECT_THAT(histogram[owner], UnorderedElementsAre(std::pair(5, 1)));
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner,
     resizing_different_vectors_with_same_owner_keeps_maximum_size_of_each) {
  std::uint64_t object_id_1 = 42;
  std::uint64_t object_id_2 = 69;
  const char *owner = "test vector";
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id_2,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/10,
      /*capacity=*/10);

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::append,
      /*size=*/4,
      /*capacity=*/4);
  data.add_entry(
      /*object_id=*/object_id_2,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/11,
      /*capacity=*/11);

  auto histogram = data.max_size_histogram_by_owner();
  EXPECT_THAT(histogram[owner],
              UnorderedElementsAre(std::pair(4, 1), std::pair(11, 1)));
}

TEST(
    test_vector_instrumentation_max_size_histogram_by_owner,
    different_vectors_with_same_owner_and_object_id_keeps_maximum_size_of_each) {
  std::uint64_t object_id = 42;
  const char *owner = "test vector";
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*size=*/3,
      /*capacity=*/3);

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/2,
      /*capacity=*/2);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*size=*/2,
      /*capacity=*/2);

  auto histogram = data.max_size_histogram_by_owner();
  EXPECT_THAT(histogram[owner],
              UnorderedElementsAre(std::pair(3, 1), std::pair(2, 1)));
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner,
     different_vectors_with_same_owner_and_size_count_separately) {
  std::uint64_t object_id_1 = 42;
  std::uint64_t object_id_2_and_3 = 69;
  std::size_t size = 8;
  const char *owner = "test vector";
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/size,
      /*capacity=*/size);

  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/size - 3,
      /*capacity=*/size);
  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*size=*/size,
      /*capacity=*/size);

  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*size=*/size - 5,
      /*capacity=*/size);
  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*size=*/size,
      /*capacity=*/size);

  auto histogram = data.max_size_histogram_by_owner();
  EXPECT_THAT(histogram[owner], UnorderedElementsAre(std::pair(size, 3)));
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_empty_histogram) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(histogram, stream);
  EXPECT_EQ(stream.str(), "");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_histogram_with_one_group) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  histogram["test group"][0] = 3;
  histogram["test group"][1] = 2;
  histogram["test group"][2] = 1;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(histogram, stream);
  EXPECT_EQ(stream.str(), R"(Max sizes for test group:
0  (50%)  ***
1  (33%)  **
2  (17%)  *
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_histogram_with_one_data_point_per_group) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  histogram["test group"][0] = 2;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(histogram, stream);
  EXPECT_EQ(stream.str(), R"(Max sizes for test group:
0  (ALL)  **
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_histogram_with_multiple_groups) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  histogram["group A"][0] = 3;
  histogram["group A"][1] = 3;
  histogram["group B"][0] = 2;
  histogram["group B"][1] = 2;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(histogram, stream);
  EXPECT_EQ(stream.str(), R"(Max sizes for group A:
0  (50%)  ***
1  (50%)  ***

Max sizes for group B:
0  (50%)  **
1  (50%)  **
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_sparse_histogram) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  histogram["test group"][1] = 1;
  histogram["test group"][4] = 1;
  histogram["test group"][5] = 1;
  histogram["test group"][9] = 1;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(histogram, stream);
  EXPECT_EQ(stream.str(), R"(Max sizes for test group:
0  ( 0%)
1  (25%)  *
2  ( 0%)
3  ( 0%)
4  (25%)  *
5  (25%)  *
6  ( 0%)
7  ( 0%)
8  ( 0%)
9  (25%)  *
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     histogram_legend_is_padded_with_spaces) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  histogram["test group"][3] = 1;
  histogram["test group"][100] = 1;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(histogram, stream);
  EXPECT_THAT(stream.str(), HasSubstr("\n  3  ("));
  EXPECT_THAT(stream.str(), HasSubstr("\n100  ("));
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     histogram_is_limited_to_max_screen_width) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  histogram["test group"][0] = 100;
  histogram["test group"][1] = 50;
  histogram["test group"][2] = 25;
  histogram["test group"][3] = 1;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(
      histogram, stream,
      vector_instrumentation::dump_options{
          .maximum_line_length = 20,
      });
  EXPECT_EQ(stream.str(), R"(Max sizes for test group:
0  (57%)  **********
1  (28%)  *****
2  (14%)  **
3  ( 1%)  *
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     histogram_including_legend_is_limited_to_max_screen_width) {
  std::map<std::string, std::map<std::size_t, int>> histogram;
  histogram["test group"][100] = 99999;
  std::ostringstream stream;
  vector_instrumentation::dump_max_size_histogram(
      histogram, stream,
      vector_instrumentation::dump_options{
          .maximum_line_length = 20,
      });
  EXPECT_THAT(stream.str(), HasSubstr("\n100  (ALL)  ********\n"));
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
