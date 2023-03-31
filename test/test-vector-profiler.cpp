// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <map>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/feature.h>
#include <sstream>

#if QLJS_FEATURE_VECTOR_PROFILING
#define FIELD_EQ(_class, _member, ...) \
  FIELD(_class, _member, decltype(_class::_member){__VA_ARGS__})

#define FIELD(_class, _member, ...) \
  (::testing::Field(#_member, &_class::_member, __VA_ARGS__))
#endif

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
template <class T>
using test_vector = instrumented_vector<std::vector<int>>;

class test_instrumented_vector : public ::testing::Test {
 public:
  void SetUp() override { vector_instrumentation::instance.clear(); }
};

TEST_F(test_instrumented_vector,
       creating_and_destroying_empty_vector_adds_entries) {
  const char *owner = "test vector";
  std::uintptr_t v_object_id;
  {
    test_vector<int> v(owner, {});
    v_object_id = reinterpret_cast<std::uintptr_t>(&v);
  }

  EXPECT_THAT(
      vector_instrumentation::instance.take_entries(),
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

TEST_F(test_instrumented_vector, creating_vector_from_range_adds_entry) {
  int data[3] = {1, 2, 3};
  const char *owner = "test vector";

  test_vector<int> v(owner, {}, &data[0], &data[3]);

  std::uintptr_t v_object_id = reinterpret_cast<std::uintptr_t>(&v);
  EXPECT_THAT(
      vector_instrumentation::instance.take_entries(),
      ElementsAre(
          AllOf(FIELD_EQ(vector_instrumentation::entry, object_id, v_object_id),
                FIELD_EQ(vector_instrumentation::entry, owner, owner),
                FIELD_EQ(vector_instrumentation::entry, event,
                         vector_instrumentation::event::create),
                FIELD_EQ(vector_instrumentation::entry, size, 3),
                FIELD(vector_instrumentation::entry, capacity, Ge(3)))));
}

TEST_F(test_instrumented_vector, append_to_vector_adds_entries) {
  test_vector<int> v("test vector", {});
  vector_instrumentation::instance.clear();

  v.emplace_back(100);
  v.emplace_back(200);
  v.emplace_back(300);
  v.emplace_back(400);

  EXPECT_THAT(
      vector_instrumentation::instance.take_entries(),
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

TEST_F(test_instrumented_vector, clearing_vector_adds_entry) {
  test_vector<int> v("test vector", {});
  v.emplace_back(100);
  v.emplace_back(200);
  vector_instrumentation::instance.clear();

  v.clear();

  EXPECT_THAT(
      vector_instrumentation::instance.take_entries(),
      ElementsAre(AllOf(FIELD_EQ(vector_instrumentation::entry, event,
                                 vector_instrumentation::event::clear),
                        FIELD_EQ(vector_instrumentation::entry, size, 0))));
}

TEST_F(test_instrumented_vector, moving_vector_with_new_owner_adds_entries) {
  const char *v_1_owner = "v1";
  test_vector<int> v_1(v_1_owner, {});
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  v_1.emplace_back(200);
  vector_instrumentation::instance.clear();

  const char *v_2_owner = "v2";
  test_vector<int> v_2(v_2_owner, std::move(v_1));
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);

  EXPECT_THAT(
      vector_instrumentation::instance.take_entries(),
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

TEST_F(test_instrumented_vector, moving_vector_with_no_owner_adds_entries) {
  const char *v_1_owner = "v1";
  test_vector<int> v_1(v_1_owner, {});
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  v_1.emplace_back(200);
  vector_instrumentation::instance.clear();

  test_vector<int> v_2(std::move(v_1));
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);

  EXPECT_THAT(
      vector_instrumentation::instance.take_entries(),
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

TEST_F(test_instrumented_vector, move_assigning_vector_adds_entries) {
  const char *v_1_owner = "v1";
  test_vector<int> v_1(v_1_owner, {});
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  const char *v_2_owner = "v2";
  test_vector<int> v_2(v_2_owner, {});
  v_2.emplace_back(200);
  v_2.emplace_back(300);
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);
  vector_instrumentation::instance.clear();

  v_1 = std::move(v_2);

  EXPECT_THAT(
      vector_instrumentation::instance.take_entries(),
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
#endif

TEST(test_vector_instrumentation, take_no_entries) {
  vector_instrumentation data;
  EXPECT_THAT(data.take_entries(), IsEmpty());
  EXPECT_THAT(data.take_entries(), IsEmpty());
}

TEST(test_vector_instrumentation, take_one_entry) {
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"first",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/1,
      /*capacity=*/1);
  auto entries_1 = data.take_entries();
  ASSERT_THAT(entries_1, ElementsAre(::testing::_));
  EXPECT_STREQ(entries_1[0].owner, "first");

  data.add_entry(
      /*object_id=*/2,
      /*owner=*/"second",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/200,
      /*size=*/1,
      /*capacity=*/1);
  auto entries_2 = data.take_entries();
  ASSERT_THAT(entries_2, ElementsAre(::testing::_));
  EXPECT_STREQ(entries_2[0].owner, "second");

  EXPECT_THAT(data.take_entries(), IsEmpty());
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner, no_events) {
  vector_instrumentation data;
  vector_max_size_histogram_by_owner histogram;
  EXPECT_THAT(histogram.histogram(), IsEmpty());
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner,
     distinctly_owned_vectors_with_one_event_each) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"first",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/2,
      /*owner=*/"second",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/200,
      /*size=*/5,
      /*capacity=*/5);
  data.add_entry(
      /*object_id=*/3,
      /*owner=*/"third",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/300,
      /*size=*/0,
      /*capacity=*/0);

  vector_max_size_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist,
              UnorderedElementsAre(Key("first"), Key("second"), Key("third")));
  EXPECT_THAT(hist["first"], UnorderedElementsAre(std::pair(3, 1)));
  EXPECT_THAT(hist["second"], UnorderedElementsAre(std::pair(5, 1)));
  EXPECT_THAT(hist["third"], UnorderedElementsAre(std::pair(0, 1)));
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner,
     appending_to_vector_keeps_maximum_size) {
  std::uint64_t object_id = 42;
  const char *owner = "test vector";
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/200,
      /*size=*/4,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/200,
      /*size=*/5,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::clear,
      /*data_pointer=*/200,
      /*size=*/0,
      /*capacity=*/10);

  vector_max_size_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist[owner], UnorderedElementsAre(std::pair(5, 1)));
}

TEST(test_vector_instrumentation_max_size_histogram_by_owner,
     growing_vector_and_shrinking_keeps_maximum_size) {
  std::uint64_t object_id = 42;
  const char *owner = "test vector";
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::resize,
      /*data_pointer=*/200,
      /*size=*/10,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::resize,
      /*data_pointer=*/200,
      /*size=*/3,
      /*capacity=*/10);

  vector_max_size_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist[owner], UnorderedElementsAre(std::pair(10, 1)));
}

TEST(
    test_vector_instrumentation_max_size_histogram_by_owner,
    appending_to_different_vectors_with_same_owner_keeps_maximum_size_of_each) {
  std::uint64_t object_id_1 = 42;
  std::uint64_t object_id_2 = 69;
  const char *owner = "test vector";
  vector_instrumentation data;

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id_2,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/200,
      /*size=*/10,
      /*capacity=*/10);

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/110,
      /*size=*/4,
      /*capacity=*/4);
  data.add_entry(
      /*object_id=*/object_id_2,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/210,
      /*size=*/11,
      /*capacity=*/11);

  vector_max_size_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist[owner],
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
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/2);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/2);

  vector_max_size_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist[owner],
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
      /*data_pointer=*/100,
      /*size=*/size,
      /*capacity=*/size);

  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/200,
      /*size=*/size - 3,
      /*capacity=*/size);
  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*data_pointer=*/200,
      /*size=*/size,
      /*capacity=*/size);

  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/200,
      /*size=*/size - 5,
      /*capacity=*/size);
  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/vector_instrumentation::event::destroy,
      /*data_pointer=*/200,
      /*size=*/size,
      /*capacity=*/size);

  vector_max_size_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist[owner], UnorderedElementsAre(std::pair(size, 3)));
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_empty_histogram) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(histogram, stream);
  EXPECT_EQ(stream.str(), "");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_histogram_with_one_group) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["test group"][0] = 3;
  histogram["test group"][1] = 2;
  histogram["test group"][2] = 1;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(histogram, stream);
  EXPECT_EQ(stream.str(), R"(Max sizes for test group:
0  (50%)  ***
1  (33%)  **
2  (17%)  *
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_histogram_with_one_data_point_per_group) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["test group"][0] = 2;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(histogram, stream);
  EXPECT_EQ(stream.str(), R"(Max sizes for test group:
0  (ALL)  **
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     dump_histogram_with_multiple_groups) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["group A"][0] = 3;
  histogram["group A"][1] = 3;
  histogram["group B"][0] = 2;
  histogram["group B"][1] = 2;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(histogram, stream);
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
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["test group"][1] = 1;
  histogram["test group"][4] = 1;
  histogram["test group"][5] = 1;
  histogram["test group"][9] = 1;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(histogram, stream);
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
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["test group"][3] = 1;
  histogram["test group"][100] = 1;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(histogram, stream);
  EXPECT_THAT(stream.str(), HasSubstr("\n  3  ("));
  EXPECT_THAT(stream.str(), HasSubstr("\n100  ("));
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     histogram_is_limited_to_max_screen_width) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["test group"][0] = 100;
  histogram["test group"][1] = 50;
  histogram["test group"][2] = 25;
  histogram["test group"][3] = 1;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(
      histogram, stream,
      vector_max_size_histogram_by_owner::dump_options{
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
     histogram_skips_many_empty_rows) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  auto &test_group = histogram["test group"];
  test_group[0] = 1;
  test_group[1] = 2;
  test_group[2] = 1;
  test_group[8] = 1;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(
      histogram, stream,
      vector_max_size_histogram_by_owner::dump_options{
          .max_adjacent_empty_rows = 3,
      });
  EXPECT_EQ(stream.str(), R"(Max sizes for test group:
0  (20%)  *
1  (40%)  **
2  (20%)  *
...
8  (20%)  *
)");
}

TEST(test_vector_instrumentation_dump_max_size_histogram,
     histogram_including_legend_is_limited_to_max_screen_width) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["test group"][100] = 99999;
  std::ostringstream stream;
  vector_max_size_histogram_by_owner::dump(
      histogram, stream,
      vector_max_size_histogram_by_owner::dump_options{
          .maximum_line_length = 20,
      });
  EXPECT_THAT(stream.str(), HasSubstr("\n100  (ALL)  ********\n"));
}

TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     no_events) {
  vector_capacity_change_histogram_by_owner histogram;
  EXPECT_THAT(histogram.histogram(), IsEmpty());
}

TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     new_vectors_have_no_appends) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"first",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/2,
      /*owner=*/"second",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/200,
      /*size=*/5,
      /*capacity=*/5);
  data.add_entry(
      /*object_id=*/3,
      /*owner=*/"third",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/300,
      /*size=*/0,
      /*capacity=*/0);

  vector_capacity_change_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist,
              UnorderedElementsAre(Key("first"), Key("second"), Key("third")));
  EXPECT_EQ(hist["first"].appends_initial_capacity, 0);
  EXPECT_EQ(hist["first"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["first"].appends_growing_capacity, 0);
  EXPECT_EQ(hist["second"].appends_initial_capacity, 0);
  EXPECT_EQ(hist["second"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["second"].appends_growing_capacity, 0);
  EXPECT_EQ(hist["third"].appends_initial_capacity, 0);
  EXPECT_EQ(hist["third"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["third"].appends_growing_capacity, 0);
}

TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     append_into_existing_capacity) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/100,
      /*size=*/1,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);

  vector_capacity_change_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 3);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 0);
}

TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     append_growing_capacity) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/110,
      /*size=*/1,
      /*capacity=*/1);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/120,
      /*size=*/2,
      /*capacity=*/2);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/130,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/140,
      /*size=*/4,
      /*capacity=*/4);

  vector_capacity_change_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 3);
}

TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     append_after_moving) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/100,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/1,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/100,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/200,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::assign,
      /*data_pointer=*/200,
      /*size=*/2,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/100,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::clear,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/200,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/280,
      /*size=*/4,
      /*capacity=*/8);

  vector_capacity_change_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 2);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 1);
}

TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     initial_allocation) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/200,
      /*size=*/1,
      /*capacity=*/4);

  vector_capacity_change_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 1);
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 0);
}

// For some reason, boost::small_vector<T, 0>'s initial capacity after default
// construction claims to be 1, even though the in-situ capacity is 0 and no
// heap allocation was made. This test makes sure that such events don't confuse
// our analysis.
TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     initial_allocation_for_boost_small_vector) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/1);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/200,
      /*size=*/1,
      /*capacity=*/4);

  vector_capacity_change_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 1);
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 0);
}

TEST(test_vector_instrumentation_capacity_change_histogram_by_owner,
     initial_allocation_reusing_object_id) {
  vector_instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/110,
      /*size=*/10,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/116,
      /*size=*/11,
      /*capacity=*/16);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::destroy,
      /*data_pointer=*/116,
      /*size=*/11,
      /*capacity=*/16);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/vector_instrumentation::event::append,
      /*data_pointer=*/104,
      /*size=*/1,
      /*capacity=*/4);

  vector_capacity_change_histogram_by_owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 1) << "second vector";
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 1) << "first vector";
}

std::string dump_capacity_change_header = R"(vector capacity changes:
(C=copied; z=initial alloc; -=used internal capacity)
)";

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     dump_empty_histogram) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  std::ostringstream stream;
  vector_capacity_change_histogram_by_owner::dump(
      histogram, stream,
      vector_capacity_change_histogram_by_owner::dump_options{});
  EXPECT_EQ(stream.str(), dump_capacity_change_header);
}

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     dump_histogram_with_only_reusing_capacity) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  histogram["myvector"].appends_reusing_capacity = 10;
  std::ostringstream stream;
  vector_capacity_change_histogram_by_owner::dump(
      histogram, stream,
      vector_capacity_change_histogram_by_owner::dump_options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.str(), dump_capacity_change_header + R"(myvector:
 0C  0z 10_ |____________________|
)");
}

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     dump_histogram_with_only_growing_capacity) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  histogram["myvector"].appends_growing_capacity = 10;
  std::ostringstream stream;
  vector_capacity_change_histogram_by_owner::dump(
      histogram, stream,
      vector_capacity_change_histogram_by_owner::dump_options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.str(), dump_capacity_change_header + R"(myvector:
10C  0z  0_ |CCCCCCCCCCCCCCCCCCCC|
)");
}

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     dump_histogram_with_only_initial_capacity) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  histogram["myvector"].appends_initial_capacity = 10;
  std::ostringstream stream;
  vector_capacity_change_histogram_by_owner::dump(
      histogram, stream,
      vector_capacity_change_histogram_by_owner::dump_options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.str(), dump_capacity_change_header + R"(myvector:
 0C 10z  0_ |zzzzzzzzzzzzzzzzzzzz|
)");
}

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     dump_histogram_with_mixed_growing_and_initial_and_reusing) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  histogram["myvector"].appends_growing_capacity = 5;
  histogram["myvector"].appends_initial_capacity = 5;
  histogram["myvector"].appends_reusing_capacity = 10;
  std::ostringstream stream;
  vector_capacity_change_histogram_by_owner::dump(
      histogram, stream,
      vector_capacity_change_histogram_by_owner::dump_options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.str(), dump_capacity_change_header + R"(myvector:
 5C  5z 10_ |CCCCCzzzzz__________|
)");
}

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     different_widths) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  histogram["myvector"].appends_growing_capacity = 9001;

  {
    std::ostringstream stream;
    vector_capacity_change_histogram_by_owner::dump(
        histogram, stream,
        vector_capacity_change_histogram_by_owner::dump_options{
            .maximum_line_length = 30,
        });
    EXPECT_EQ(stream.str(), dump_capacity_change_header + R"(myvector:
9001C    0z    0_ |CCCCCCCCCC|
)");
  }

  {
    std::ostringstream stream;
    vector_capacity_change_histogram_by_owner::dump(
        histogram, stream,
        vector_capacity_change_histogram_by_owner::dump_options{
            .maximum_line_length = 50,
        });
    EXPECT_EQ(stream.str(), dump_capacity_change_header + R"(myvector:
9001C    0z    0_ |CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC|
)");
  }
}

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     multiple_owners_align_counts) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  histogram["first"].appends_growing_capacity = 1;
  histogram["first"].appends_reusing_capacity = 1;
  histogram["second"].appends_growing_capacity = 30;
  histogram["second"].appends_reusing_capacity = 30;
  std::ostringstream stream;
  vector_capacity_change_histogram_by_owner::dump(
      histogram, stream,
      vector_capacity_change_histogram_by_owner::dump_options{
          .maximum_line_length = 30,
      });
  EXPECT_EQ(stream.str(), dump_capacity_change_header + R"(first:
 1C  0z  1_ |CCCCCCCC________|
second:
30C  0z 30_ |CCCCCCCC________|
)");
}

TEST(test_vector_instrumentation_dump_capacity_change_histogram,
     hides_vectors_with_no_appends) {
  std::map<std::string_view,
           vector_capacity_change_histogram_by_owner::capacity_change_histogram>
      histogram;
  histogram["first"];   // Zeroes.
  histogram["second"];  // Zeroes.
  std::ostringstream stream;
  vector_capacity_change_histogram_by_owner::dump(
      histogram, stream,
      vector_capacity_change_histogram_by_owner::dump_options{});
  EXPECT_EQ(stream.str(), dump_capacity_change_header);
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
