// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <cstddef>
#include <cstdint>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <map>
#include <quick-lint-js/container/concat.h>
#include <quick-lint-js/container/linked-bump-allocator.h>
#include <quick-lint-js/container/monotonic-allocator.h>
#include <quick-lint-js/container/vector-profiler.h>
#include <quick-lint-js/container/vector.h>
#include <quick-lint-js/feature.h>
#include <quick-lint-js/logging/trace-writer.h>
#include <sstream>

#if QLJS_FEATURE_VECTOR_PROFILING
#define FIELD_EQ(_class, _member, ...) \
  FIELD(_class, _member, decltype(_class::_member){__VA_ARGS__})

#define FIELD(_class, _member, ...) \
  (::testing::Field(#_member, &_class::_member, __VA_ARGS__))
#endif

using ::testing::AllOf;
using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::Ge;
using ::testing::HasSubstr;
using ::testing::IsEmpty;
using ::testing::Key;
using ::testing::UnorderedElementsAreArray;

namespace quick_lint_js {
namespace {
#if QLJS_FEATURE_VECTOR_PROFILING
template <class T>
using Test_Vector = Instrumented_Vector<std::vector<int>>;

class Test_Instrumented_Vector : public ::testing::Test {
 public:
  void SetUp() override { Vector_Instrumentation::instance.clear(); }
};

TEST_F(Test_Instrumented_Vector,
       creating_and_destroying_empty_vector_adds_entries) {
  const char *owner = "test vector";
  std::uintptr_t v_object_id;
  {
    Test_Vector<int> v(owner, {});
    v_object_id = reinterpret_cast<std::uintptr_t>(&v);
  }

  EXPECT_THAT(
      Vector_Instrumentation::instance.take_entries(),
      ElementsAre(
          AllOf(FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_object_id),
                FIELD_EQ(Vector_Instrumentation::Entry, owner, owner),
                FIELD_EQ(Vector_Instrumentation::Entry, event,
                         Vector_Instrumentation::Event::create),
                FIELD_EQ(Vector_Instrumentation::Entry, size, 0)),
          AllOf(FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_object_id),
                FIELD_EQ(Vector_Instrumentation::Entry, owner, owner),
                FIELD_EQ(Vector_Instrumentation::Entry, event,
                         Vector_Instrumentation::Event::destroy))));
}

TEST_F(Test_Instrumented_Vector, creating_vector_from_range_adds_entry) {
  int data[3] = {1, 2, 3};
  const char *owner = "test vector";

  Test_Vector<int> v(owner, {}, &data[0], &data[3]);

  std::uintptr_t v_object_id = reinterpret_cast<std::uintptr_t>(&v);
  EXPECT_THAT(
      Vector_Instrumentation::instance.take_entries(),
      ElementsAreArray({
          AllOf(FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_object_id),
                FIELD_EQ(Vector_Instrumentation::Entry, owner, owner),
                FIELD_EQ(Vector_Instrumentation::Entry, event,
                         Vector_Instrumentation::Event::create),
                FIELD_EQ(Vector_Instrumentation::Entry, size, 3),
                FIELD(Vector_Instrumentation::Entry, capacity, Ge(3))),
      }));
}

TEST_F(Test_Instrumented_Vector, append_to_vector_adds_entries) {
  Test_Vector<int> v("test vector", {});
  Vector_Instrumentation::instance.clear();

  v.emplace_back(100);
  v.emplace_back(200);
  v.emplace_back(300);
  v.emplace_back(400);

  EXPECT_THAT(
      Vector_Instrumentation::instance.take_entries(),
      ElementsAre(AllOf(FIELD_EQ(Vector_Instrumentation::Entry, event,
                                 Vector_Instrumentation::Event::append),
                        FIELD_EQ(Vector_Instrumentation::Entry, size, 1)),
                  AllOf(FIELD_EQ(Vector_Instrumentation::Entry, event,
                                 Vector_Instrumentation::Event::append),
                        FIELD_EQ(Vector_Instrumentation::Entry, size, 2)),
                  AllOf(FIELD_EQ(Vector_Instrumentation::Entry, event,
                                 Vector_Instrumentation::Event::append),
                        FIELD_EQ(Vector_Instrumentation::Entry, size, 3)),
                  AllOf(FIELD_EQ(Vector_Instrumentation::Entry, event,
                                 Vector_Instrumentation::Event::append),
                        FIELD_EQ(Vector_Instrumentation::Entry, size, 4))));
}

TEST_F(Test_Instrumented_Vector, clearing_vector_adds_entry) {
  Test_Vector<int> v("test vector", {});
  v.emplace_back(100);
  v.emplace_back(200);
  Vector_Instrumentation::instance.clear();

  v.clear();

  EXPECT_THAT(Vector_Instrumentation::instance.take_entries(),
              ElementsAreArray({
                  AllOf(FIELD_EQ(Vector_Instrumentation::Entry, event,
                                 Vector_Instrumentation::Event::clear),
                        FIELD_EQ(Vector_Instrumentation::Entry, size, 0)),
              }));
}

TEST_F(Test_Instrumented_Vector, moving_vector_with_new_owner_adds_entries) {
  const char *v_1_owner = "v1";
  Test_Vector<int> v_1(v_1_owner, {});
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  v_1.emplace_back(200);
  Vector_Instrumentation::instance.clear();

  const char *v_2_owner = "v2";
  Test_Vector<int> v_2(v_2_owner, std::move(v_1));
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);

  EXPECT_THAT(
      Vector_Instrumentation::instance.take_entries(),
      ElementsAre(
          AllOf(
              FIELD_EQ(Vector_Instrumentation::Entry, owner, v_2_owner),
              FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_2_object_id),
              FIELD_EQ(Vector_Instrumentation::Entry, event,
                       Vector_Instrumentation::Event::create),
              FIELD_EQ(Vector_Instrumentation::Entry, size, 2)),
          AllOf(
              FIELD_EQ(Vector_Instrumentation::Entry, owner, v_1_owner),
              FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_1_object_id),
              FIELD_EQ(Vector_Instrumentation::Entry, event,
                       Vector_Instrumentation::Event::clear),
              FIELD_EQ(Vector_Instrumentation::Entry, size, 0))));
}

TEST_F(Test_Instrumented_Vector, moving_vector_with_no_owner_adds_entries) {
  const char *v_1_owner = "v1";
  Test_Vector<int> v_1(v_1_owner, {});
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  v_1.emplace_back(200);
  Vector_Instrumentation::instance.clear();

  Test_Vector<int> v_2(std::move(v_1));
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);

  EXPECT_THAT(
      Vector_Instrumentation::instance.take_entries(),
      ElementsAre(
          AllOf(
              FIELD_EQ(Vector_Instrumentation::Entry, owner, v_1_owner),
              FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_2_object_id),
              FIELD_EQ(Vector_Instrumentation::Entry, event,
                       Vector_Instrumentation::Event::create),
              FIELD_EQ(Vector_Instrumentation::Entry, size, 2)),
          AllOf(
              FIELD_EQ(Vector_Instrumentation::Entry, owner, v_1_owner),
              FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_1_object_id),
              FIELD_EQ(Vector_Instrumentation::Entry, event,
                       Vector_Instrumentation::Event::clear),
              FIELD_EQ(Vector_Instrumentation::Entry, size, 0))));
}

TEST_F(Test_Instrumented_Vector, move_assigning_vector_adds_entries) {
  const char *v_1_owner = "v1";
  Test_Vector<int> v_1(v_1_owner, {});
  std::uintptr_t v_1_object_id = reinterpret_cast<std::uintptr_t>(&v_1);
  v_1.emplace_back(100);
  const char *v_2_owner = "v2";
  Test_Vector<int> v_2(v_2_owner, {});
  v_2.emplace_back(200);
  v_2.emplace_back(300);
  std::uintptr_t v_2_object_id = reinterpret_cast<std::uintptr_t>(&v_2);
  Vector_Instrumentation::instance.clear();

  v_1 = std::move(v_2);

  EXPECT_THAT(
      Vector_Instrumentation::instance.take_entries(),
      ElementsAre(
          AllOf(
              FIELD_EQ(Vector_Instrumentation::Entry, owner, v_1_owner),
              FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_1_object_id),
              FIELD_EQ(Vector_Instrumentation::Entry, event,
                       Vector_Instrumentation::Event::assign),
              FIELD_EQ(Vector_Instrumentation::Entry, size, 2)),
          AllOf(
              FIELD_EQ(Vector_Instrumentation::Entry, owner, v_2_owner),
              FIELD_EQ(Vector_Instrumentation::Entry, object_id, v_2_object_id),
              FIELD_EQ(Vector_Instrumentation::Entry, event,
                       Vector_Instrumentation::Event::clear),
              FIELD_EQ(Vector_Instrumentation::Entry, size, 0))));
}
#endif

TEST(Test_Vector_Instrumentation, take_no_entries) {
  Vector_Instrumentation data;
  EXPECT_THAT(data.take_entries(), IsEmpty());
  EXPECT_THAT(data.take_entries(), IsEmpty());
}

TEST(Test_Vector_Instrumentation, take_one_entry) {
  Vector_Instrumentation data;

  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"first",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/1,
      /*capacity=*/1);
  auto entries_1 = data.take_entries();
  ASSERT_THAT(entries_1, ElementsAreArray({::testing::_}));
  EXPECT_STREQ(entries_1[0].owner, "first");

  data.add_entry(
      /*object_id=*/2,
      /*owner=*/"second",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/200,
      /*size=*/1,
      /*capacity=*/1);
  auto entries_2 = data.take_entries();
  ASSERT_THAT(entries_2, ElementsAreArray({::testing::_}));
  EXPECT_STREQ(entries_2[0].owner, "second");

  EXPECT_THAT(data.take_entries(), IsEmpty());
}

class Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner
    : public ::testing::Test {
 public:
  Monotonic_Allocator memory{
      "Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner"};
};

TEST_F(Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner, no_events) {
  Vector_Instrumentation data;
  Vector_Max_Size_Histogram_By_Owner histogram;
  EXPECT_THAT(histogram.histogram(&this->memory), IsEmpty());
}

TEST_F(Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner,
       distinctly_owned_vectors_with_one_event_each) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"first",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/2,
      /*owner=*/"second",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/200,
      /*size=*/5,
      /*capacity=*/5);
  data.add_entry(
      /*object_id=*/3,
      /*owner=*/"third",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/300,
      /*size=*/0,
      /*capacity=*/0);

  Vector_Max_Size_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram(&this->memory);
  EXPECT_THAT(hist,
              ElementsAreArray({
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = u8"first"_sv,
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = 3, .count = 1},
                          }),
                  },
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = u8"second"_sv,
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = 5, .count = 1},
                          }),
                  },
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = u8"third"_sv,
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = 0, .count = 1},
                          }),
                  },
              }));
}

TEST_F(Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner,
       appending_to_vector_keeps_maximum_size) {
  std::uint64_t object_id = 42;
  const char *owner = "test vector";
  Vector_Instrumentation data;

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/200,
      /*size=*/4,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/200,
      /*size=*/5,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::clear,
      /*data_pointer=*/200,
      /*size=*/0,
      /*capacity=*/10);

  Vector_Max_Size_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram(&this->memory);
  EXPECT_THAT(hist,
              ElementsAreArray({
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = to_string8_view(owner),
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = 5, .count = 1},
                          }),
                  },
              }));
}

TEST_F(Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner,
       growing_vector_and_shrinking_keeps_maximum_size) {
  std::uint64_t object_id = 42;
  const char *owner = "test vector";
  Vector_Instrumentation data;

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::resize,
      /*data_pointer=*/200,
      /*size=*/10,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::resize,
      /*data_pointer=*/200,
      /*size=*/3,
      /*capacity=*/10);

  Vector_Max_Size_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram(&this->memory);
  EXPECT_THAT(hist,
              ElementsAreArray({
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = to_string8_view(owner),
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = 10, .count = 1},
                          }),
                  },
              }));
}

TEST_F(
    Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner,
    appending_to_different_vectors_with_same_owner_keeps_maximum_size_of_each) {
  std::uint64_t object_id_1 = 42;
  std::uint64_t object_id_2 = 69;
  const char *owner = "test vector";
  Vector_Instrumentation data;

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id_2,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/200,
      /*size=*/10,
      /*capacity=*/10);

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/110,
      /*size=*/4,
      /*capacity=*/4);
  data.add_entry(
      /*object_id=*/object_id_2,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/210,
      /*size=*/11,
      /*capacity=*/11);

  Vector_Max_Size_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram(&this->memory);
  EXPECT_THAT(hist,
              ElementsAreArray({
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = to_string8_view(owner),
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = 4, .count = 1},
                              {.max_size = 11, .count = 1},
                          }),
                  },
              }));
}

TEST_F(
    Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner,
    different_vectors_with_same_owner_and_object_id_keeps_maximum_size_of_each) {
  std::uint64_t object_id = 42;
  const char *owner = "test vector";
  Vector_Instrumentation data;

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::destroy,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);

  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/2);
  data.add_entry(
      /*object_id=*/object_id,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::destroy,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/2);

  Vector_Max_Size_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram(&this->memory);
  EXPECT_THAT(hist,
              ElementsAreArray({
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = to_string8_view(owner),
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = 2, .count = 1},
                              {.max_size = 3, .count = 1},
                          }),
                  },
              }));
}

TEST_F(Test_Vector_Instrumentation_Max_Size_Histogram_By_Owner,
       different_vectors_with_same_owner_and_size_count_separately) {
  std::uint64_t object_id_1 = 42;
  std::uint64_t object_id_2_and_3 = 69;
  std::size_t size = 8;
  const char *owner = "test vector";
  Vector_Instrumentation data;

  data.add_entry(
      /*object_id=*/object_id_1,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/size,
      /*capacity=*/size);

  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/200,
      /*size=*/size - 3,
      /*capacity=*/size);
  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::destroy,
      /*data_pointer=*/200,
      /*size=*/size,
      /*capacity=*/size);

  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/200,
      /*size=*/size - 5,
      /*capacity=*/size);
  data.add_entry(
      /*object_id=*/object_id_2_and_3,
      /*owner=*/owner,
      /*event=*/Vector_Instrumentation::Event::destroy,
      /*data_pointer=*/200,
      /*size=*/size,
      /*capacity=*/size);

  Vector_Max_Size_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram(&this->memory);
  EXPECT_THAT(hist,
              ElementsAreArray({
                  Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
                      .owner = to_string8_view(owner),
                      .max_size_entries =
                          Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                              {.max_size = size, .count = 3},
                          }),
                  },
              }));
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     dump_empty_histogram) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>(), stream);
  EXPECT_EQ(stream.get_flushed_string8(), u8""_sv);
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     dump_histogram_with_one_group) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"test group"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 0, .count = 3},
                      {.max_size = 1, .count = 2},
                      {.max_size = 2, .count = 1},
                  }),
          },
      }),
      stream);
  EXPECT_EQ(stream.get_flushed_string8(),
            u8R"(Max sizes for test group:
0  (50%)  ***
1  (33%)  **
2  (17%)  *
)"_sv);
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     dump_histogram_with_one_data_point_per_group) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"test group"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 0, .count = 2},
                  }),
          },
      }),
      stream);
  EXPECT_EQ(stream.get_flushed_string8(),
            u8R"(Max sizes for test group:
0  (ALL)  **
)"_sv);
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     dump_histogram_with_multiple_groups) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"group A"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 0, .count = 3},
                      {.max_size = 1, .count = 3},
                  }),
          },
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"group B"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 0, .count = 2},
                      {.max_size = 1, .count = 2},
                  }),
          },
      }),
      stream);
  EXPECT_EQ(stream.get_flushed_string8(),
            u8R"(Max sizes for group A:
0  (50%)  ***
1  (50%)  ***

Max sizes for group B:
0  (50%)  **
1  (50%)  **
)"_sv);
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     dump_sparse_histogram) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"test group"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 1, .count = 1},
                      {.max_size = 4, .count = 1},
                      {.max_size = 5, .count = 1},
                      {.max_size = 9, .count = 1},
                  }),
          },
      }),
      stream);
  EXPECT_EQ(stream.get_flushed_string8(),
            u8R"(Max sizes for test group:
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
)"_sv);
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     histogram_legend_is_padded_with_spaces) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"test group"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 3, .count = 1},
                      {.max_size = 100, .count = 1},
                  }),
          },
      }),
      stream);
  EXPECT_THAT(to_string(stream.get_flushed_string8()), HasSubstr("\n  3  ("));
  EXPECT_THAT(to_string(stream.get_flushed_string8()), HasSubstr("\n100  ("));
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     histogram_is_limited_to_max_screen_width) {
  std::map<std::string_view, std::map<std::size_t, int>> histogram;
  histogram["test group"][0] = 100;
  histogram["test group"][1] = 50;
  histogram["test group"][2] = 25;
  histogram["test group"][3] = 1;
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"test group"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 0, .count = 100},
                      {.max_size = 1, .count = 50},
                      {.max_size = 2, .count = 25},
                      {.max_size = 3, .count = 1},
                  }),
          },
      }),
      stream,
      Vector_Max_Size_Histogram_By_Owner::Dump_Options{
          .maximum_line_length = 20,
      });
  EXPECT_EQ(stream.get_flushed_string8(),
            u8R"(Max sizes for test group:
0  (57%)  **********
1  (28%)  *****
2  (14%)  **
3  ( 1%)  *
)"_sv);
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     histogram_skips_many_empty_rows) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"test group"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 0, .count = 1},
                      {.max_size = 1, .count = 2},
                      {.max_size = 2, .count = 1},
                      {.max_size = 8, .count = 1},
                  }),
          },
      }),
      stream,
      Vector_Max_Size_Histogram_By_Owner::Dump_Options{
          .max_adjacent_empty_rows = 3,
      });
  EXPECT_EQ(stream.get_flushed_string8(),
            u8R"(Max sizes for test group:
0  (20%)  *
1  (40%)  **
2  (20%)  *
...
8  (20%)  *
)"_sv);
}

TEST(Test_Vector_Instrumentation_Dump_Max_Size_Histogram,
     histogram_including_legend_is_limited_to_max_screen_width) {
  Memory_Output_Stream stream;
  Vector_Max_Size_Histogram_By_Owner::dump(
      Span<const Trace_Vector_Max_Size_Histogram_By_Owner_Entry>({
          Trace_Vector_Max_Size_Histogram_By_Owner_Entry{
              .owner = u8"test group"_sv,
              .max_size_entries =
                  Span<const Trace_Vector_Max_Size_Histogram_Entry>({
                      {.max_size = 100, .count = 99999},
                  }),
          },
      }),
      stream,
      Vector_Max_Size_Histogram_By_Owner::Dump_Options{
          .maximum_line_length = 20,
      });
  EXPECT_THAT(to_string(stream.get_flushed_string8()),
              HasSubstr("\n100  (ALL)  ********\n"));
}

TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     no_events) {
  Vector_Capacity_Change_Histogram_By_Owner histogram;
  EXPECT_THAT(histogram.histogram(), IsEmpty());
}

TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     new_vectors_have_no_appends) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"first",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/2,
      /*owner=*/"second",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/200,
      /*size=*/5,
      /*capacity=*/5);
  data.add_entry(
      /*object_id=*/3,
      /*owner=*/"third",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/300,
      /*size=*/0,
      /*capacity=*/0);

  Vector_Capacity_Change_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_THAT(hist, UnorderedElementsAreArray({
                        Key("first"),
                        Key("second"),
                        Key("third"),
                    }));
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

TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     append_into_existing_capacity) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/100,
      /*size=*/1,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/100,
      /*size=*/3,
      /*capacity=*/3);

  Vector_Capacity_Change_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 3);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 0);
}

TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     append_growing_capacity) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/110,
      /*size=*/1,
      /*capacity=*/1);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/120,
      /*size=*/2,
      /*capacity=*/2);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/130,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/140,
      /*size=*/4,
      /*capacity=*/4);

  Vector_Capacity_Change_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 3);
}

TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     append_after_moving) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/100,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/1,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/100,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/100,
      /*size=*/2,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/200,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::assign,
      /*data_pointer=*/200,
      /*size=*/2,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/100,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::clear,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/200,
      /*size=*/3,
      /*capacity=*/3);
  data.add_entry(
      /*object_id=*/200,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/280,
      /*size=*/4,
      /*capacity=*/8);

  Vector_Capacity_Change_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 2);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 1);
}

TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     initial_allocation) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/200,
      /*size=*/1,
      /*capacity=*/4);

  Vector_Capacity_Change_Histogram_By_Owner histogram;
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
TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     initial_allocation_for_boost_small_vector) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/1);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/200,
      /*size=*/1,
      /*capacity=*/4);

  Vector_Capacity_Change_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 1);
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 0);
}

TEST(Test_Vector_Instrumentation_Capacity_Change_Histogram_By_Owner,
     initial_allocation_reusing_object_id) {
  Vector_Instrumentation data;
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/110,
      /*size=*/10,
      /*capacity=*/10);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/116,
      /*size=*/11,
      /*capacity=*/16);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::destroy,
      /*data_pointer=*/116,
      /*size=*/11,
      /*capacity=*/16);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::create,
      /*data_pointer=*/100,
      /*size=*/0,
      /*capacity=*/0);
  data.add_entry(
      /*object_id=*/1,
      /*owner=*/"myvector",
      /*event=*/Vector_Instrumentation::Event::append,
      /*data_pointer=*/104,
      /*size=*/1,
      /*capacity=*/4);

  Vector_Capacity_Change_Histogram_By_Owner histogram;
  histogram.add_entries(data.take_entries());
  auto hist = histogram.histogram();
  EXPECT_EQ(hist["myvector"].appends_initial_capacity, 1) << "second vector";
  EXPECT_EQ(hist["myvector"].appends_reusing_capacity, 0);
  EXPECT_EQ(hist["myvector"].appends_growing_capacity, 1) << "first vector";
}

constexpr String8_View dump_capacity_change_header =
    u8R"(vector capacity changes:
(C=copied; z=initial alloc; -=used internal capacity)
)"_sv;

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     dump_empty_histogram) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  Memory_Output_Stream stream;
  Vector_Capacity_Change_Histogram_By_Owner::dump(
      histogram, stream,
      Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{});
  EXPECT_EQ(stream.get_flushed_string8(), dump_capacity_change_header);
}

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     dump_histogram_with_only_reusing_capacity) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  histogram["myvector"].appends_reusing_capacity = 10;
  Memory_Output_Stream stream;
  Vector_Capacity_Change_Histogram_By_Owner::dump(
      histogram, stream,
      Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.get_flushed_string8(), concat(dump_capacity_change_header,
                                                 u8R"(myvector:
 0C  0z 10_ |____________________|
)"_sv));
}

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     dump_histogram_with_only_growing_capacity) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  histogram["myvector"].appends_growing_capacity = 10;
  Memory_Output_Stream stream;
  Vector_Capacity_Change_Histogram_By_Owner::dump(
      histogram, stream,
      Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.get_flushed_string8(), concat(dump_capacity_change_header,
                                                 u8R"(myvector:
10C  0z  0_ |CCCCCCCCCCCCCCCCCCCC|
)"_sv));
}

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     dump_histogram_with_only_initial_capacity) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  histogram["myvector"].appends_initial_capacity = 10;
  Memory_Output_Stream stream;
  Vector_Capacity_Change_Histogram_By_Owner::dump(
      histogram, stream,
      Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.get_flushed_string8(), concat(dump_capacity_change_header,
                                                 u8R"(myvector:
 0C 10z  0_ |zzzzzzzzzzzzzzzzzzzz|
)"_sv));
}

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     dump_histogram_with_mixed_growing_and_initial_and_reusing) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  histogram["myvector"].appends_growing_capacity = 5;
  histogram["myvector"].appends_initial_capacity = 5;
  histogram["myvector"].appends_reusing_capacity = 10;
  Memory_Output_Stream stream;
  Vector_Capacity_Change_Histogram_By_Owner::dump(
      histogram, stream,
      Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
          .maximum_line_length = 34,
      });
  EXPECT_EQ(stream.get_flushed_string8(), concat(dump_capacity_change_header,
                                                 u8R"(myvector:
 5C  5z 10_ |CCCCCzzzzz__________|
)"_sv));
}

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     different_widths) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  histogram["myvector"].appends_growing_capacity = 9001;

  {
    Memory_Output_Stream stream;
    Vector_Capacity_Change_Histogram_By_Owner::dump(
        histogram, stream,
        Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
            .maximum_line_length = 30,
        });
    EXPECT_EQ(stream.get_flushed_string8(), concat(dump_capacity_change_header,
                                                   u8R"(myvector:
9001C    0z    0_ |CCCCCCCCCC|
)"_sv));
  }

  {
    Memory_Output_Stream stream;
    Vector_Capacity_Change_Histogram_By_Owner::dump(
        histogram, stream,
        Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
            .maximum_line_length = 50,
        });
    EXPECT_EQ(stream.get_flushed_string8(), concat(dump_capacity_change_header,
                                                   u8R"(myvector:
9001C    0z    0_ |CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC|
)"_sv));
  }
}

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     multiple_owners_align_counts) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  histogram["first"].appends_growing_capacity = 1;
  histogram["first"].appends_reusing_capacity = 1;
  histogram["second"].appends_growing_capacity = 30;
  histogram["second"].appends_reusing_capacity = 30;
  Memory_Output_Stream stream;
  Vector_Capacity_Change_Histogram_By_Owner::dump(
      histogram, stream,
      Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{
          .maximum_line_length = 30,
      });
  EXPECT_EQ(stream.get_flushed_string8(), concat(dump_capacity_change_header,
                                                 u8R"(first:
 1C  0z  1_ |CCCCCCCC________|
second:
30C  0z 30_ |CCCCCCCC________|
)"_sv));
}

TEST(Test_Vector_Instrumentation_Dump_Capacity_Change_Histogram,
     hides_vectors_with_no_appends) {
  std::map<std::string_view,
           Vector_Capacity_Change_Histogram_By_Owner::Capacity_Change_Histogram>
      histogram;
  histogram["first"];   // Zeroes.
  histogram["second"];  // Zeroes.
  Memory_Output_Stream stream;
  Vector_Capacity_Change_Histogram_By_Owner::dump(
      histogram, stream,
      Vector_Capacity_Change_Histogram_By_Owner::Dump_Options{});
  EXPECT_EQ(stream.get_flushed_string8(), dump_capacity_change_header);
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
