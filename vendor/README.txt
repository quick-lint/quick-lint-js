# Vendor

This vendor directory contains third-party software.

## benchmark

The benchmark directory contains Google Benchmark, a performance measurement
framework for C++ code.

Copyright: Copyright 2015 Google Inc. All rights reserved.; Copyright 2016 Ismael Jimenez Martinez. All rights reserved.
Download URL: https://codeload.github.com/google/benchmark/tar.gz/v1.5.1
Download date: August 20, 2020
Git commit: 8039b4030795b1c9b8cedb78e3a2a6fb89574b6e
License file: LICENSE
License type: Apache-2.0
Location: benchmark
Project URL: https://github.com/google/benchmark
Release URL: https://github.com/google/benchmark/releases/tag/v1.5.1
Release date: June 9, 2020
Version: v1.5.1

## boost

The boost directory contains Boost, free peer-reviewed portable C++ source
libraries.

The following directories and files have been deleted to reduce storage
consumption:

* boost/*/accumulators/
* boost/*/algorithm/
* boost/*/align/
* boost/*/any/
* boost/*/archive/
* boost/*/array/
* boost/*/asio/
* boost/*/assert/
* boost/*/assign/
* boost/*/atomic/
* boost/*/beast/
* boost/*/bimap/
* boost/*/bind/
* boost/*/callable_traits/
* boost/*/chrono/
* boost/*/circular_buffer/
* boost/*/compatibility/
* boost/*/compute/
* boost/*/concept/detail/
* boost/*/concept_check/
* boost/*/config/abi/
* boost/*/config/no_tr1/
* boost/*/container_hash/detail/
* boost/*/context/
* boost/*/contract/
* boost/*/conversion/
* boost/*/convert/
* boost/*/coroutine/
* boost/*/coroutine2/
* boost/*/crc/
* boost/*/date_time/
* boost/*/detail/winapi/detail/
* boost/*/dll/
* boost/*/dynamic_bitset/
* boost/*/endian/
* boost/*/exception/
* boost/*/fiber/
* boost/*/filesystem/
* boost/*/flyweight/
* boost/*/foreach/
* boost/*/format/
* boost/*/function/
* boost/*/function_types/
* boost/*/functional/
* boost/*/fusion/
* boost/*/geometry/
* boost/*/gil/
* boost/*/graph/
* boost/*/graph_parallel/
* boost/*/hana/
* boost/*/headers/
* boost/*/heap/
* boost/*/histogram/
* boost/*/hof/
* boost/*/icl/
* boost/*/index.html/
* boost/*/integer/
* boost/*/interprocess/
* boost/*/io/
* boost/*/iostreams/
* boost/*/iterator/
* boost/*/lambda/
* boost/*/lexical_cast/
* boost/*/libraries.htm/
* boost/*/local_function/
* boost/*/locale/
* boost/*/lockfree/
* boost/*/log/
* boost/*/logic/
* boost/*/maintainers.txt/
* boost/*/math/
* boost/*/metaparse/
* boost/*/move/algo/detail/
* boost/*/mp11/
* boost/*/mpi/
* boost/*/mpl/
* boost/*/msm/
* boost/*/multi_array/
* boost/*/multi_index/
* boost/*/multiprecision/
* boost/*/nowide/
* boost/*/numeric/
* boost/*/optional/
* boost/*/outcome/
* boost/*/parameter/
* boost/*/parameter_python/
* boost/*/pending/detail/
* boost/*/phoenix/
* boost/*/platform_maintainers.txt/
* boost/*/poly_collection/
* boost/*/polygon/
* boost/*/pool/
* boost/*/predef/
* boost/*/preprocessor/arithmetic/detail/
* boost/*/preprocessor/array/detail/
* boost/*/preprocessor/comparison/
* boost/*/preprocessor/config/
* boost/*/preprocessor/control/detail/dmc/
* boost/*/preprocessor/control/detail/edg/
* boost/*/preprocessor/control/detail/msvc/
* boost/*/preprocessor/debug/
* boost/*/preprocessor/detail/dmc/
* boost/*/preprocessor/facilities/detail/
* boost/*/preprocessor/iteration/detail/bounds/
* boost/*/preprocessor/iteration/detail/iter/
* boost/*/preprocessor/list/detail/dmc/
* boost/*/preprocessor/list/detail/edg/
* boost/*/preprocessor/logical/
* boost/*/preprocessor/punctuation/detail/
* boost/*/preprocessor/repetition/detail/dmc/
* boost/*/preprocessor/repetition/detail/edg/
* boost/*/preprocessor/repetition/detail/msvc/
* boost/*/preprocessor/selection/
* boost/*/preprocessor/seq/detail/
* boost/*/preprocessor/slot/detail/
* boost/*/preprocessor/tuple/detail/
* boost/*/preprocessor/variadic/detail/
* boost/*/process/
* boost/*/program_options/
* boost/*/property_map/
* boost/*/property_tree/
* boost/*/proto/
* boost/*/ptr_container/
* boost/*/python/
* boost/*/qvm/
* boost/*/random/
* boost/*/range/
* boost/*/ratio/
* boost/*/rational/
* boost/*/regex/
* boost/*/safe_numerics/
* boost/*/scope_exit/
* boost/*/serialization/
* boost/*/signals2/
* boost/*/smart_ptr/
* boost/*/sort/
* boost/*/spirit/
* boost/*/stacktrace/
* boost/*/statechart/
* boost/*/static_string/
* boost/*/stl_interfaces/
* boost/*/system/
* boost/*/test/
* boost/*/thread/
* boost/*/timer/
* boost/*/tokenizer/
* boost/*/tti/
* boost/*/tuple/
* boost/*/type_erasure/
* boost/*/type_index/
* boost/*/type_traits/detail/
* boost/*/typeof/
* boost/*/units/
* boost/*/unordered/
* boost/*/utility/
* boost/*/uuid/
* boost/*/variant/
* boost/*/variant2/
* boost/*/vmd/
* boost/*/wave/
* boost/*/winapi/
* boost/*/xpressive/
* boost/*/yap/
* boost/boost/align.hpp
* boost/boost/aligned_storage.hpp
* boost/boost/any.hpp
* boost/boost/array.hpp
* boost/boost/asio.hpp
* boost/boost/assign.hpp
* boost/boost/atomic.hpp
* boost/boost/beast.hpp
* boost/boost/bimap.hpp
* boost/boost/bind.hpp
* boost/boost/blank.hpp
* boost/boost/blank_fwd.hpp
* boost/boost/call_traits.hpp
* boost/boost/callable_traits.hpp
* boost/boost/cast.hpp
* boost/boost/cerrno.hpp
* boost/boost/checked_delete.hpp
* boost/boost/chrono.hpp
* boost/boost/circular_buffer.hpp
* boost/boost/circular_buffer_fwd.hpp
* boost/boost/compressed_pair.hpp
* boost/boost/compute.hpp
* boost/boost/concept/
* boost/boost/concept_archetype.hpp
* boost/boost/concept_check.hpp
* boost/boost/container/adaptive_pool.hpp
* boost/boost/container/allocator.hpp
* boost/boost/container/deque.hpp
* boost/boost/container/detail/adaptive_node_pool.hpp
* boost/boost/container/detail/adaptive_node_pool_impl.hpp
* boost/boost/container/detail/advanced_insert_int.hpp
* boost/boost/container/detail/algorithm.hpp
* boost/boost/container/detail/alloc_helpers.hpp
* boost/boost/container/detail/allocation_type.hpp
* boost/boost/container/detail/allocator_version_traits.hpp
* boost/boost/container/detail/compare_functors.hpp
* boost/boost/container/detail/construct_in_place.hpp
* boost/boost/container/detail/container_or_allocator_rebind.hpp
* boost/boost/container/detail/container_rebind.hpp
* boost/boost/container/detail/copy_move_algo.hpp
* boost/boost/container/detail/destroyers.hpp
* boost/boost/container/detail/flat_tree.hpp
* boost/boost/container/detail/function_detector.hpp
* boost/boost/container/detail/is_container.hpp
* boost/boost/container/detail/is_contiguous_container.hpp
* boost/boost/container/detail/is_sorted.hpp
* boost/boost/container/detail/iterator.hpp
* boost/boost/container/detail/iterator_to_raw_pointer.hpp
* boost/boost/container/detail/iterators.hpp
* boost/boost/container/detail/math_functions.hpp
* boost/boost/container/detail/minimal_char_traits_header.hpp
* boost/boost/container/detail/multiallocation_chain.hpp
* boost/boost/container/detail/mutex.hpp
* boost/boost/container/detail/next_capacity.hpp
* boost/boost/container/detail/node_alloc_holder.hpp
* boost/boost/container/detail/node_pool.hpp
* boost/boost/container/detail/node_pool_impl.hpp
* boost/boost/container/detail/pair_key_mapped_of_value.hpp
* boost/boost/container/detail/pool_common.hpp
* boost/boost/container/detail/pool_common_alloc.hpp
* boost/boost/container/detail/transform_iterator.hpp
* boost/boost/container/detail/tree.hpp
* boost/boost/container/detail/value_functors.hpp
* boost/boost/container/detail/value_init.hpp
* boost/boost/container/detail/version_type.hpp
* boost/boost/container/flat_map.hpp
* boost/boost/container/flat_set.hpp
* boost/boost/container/list.hpp
* boost/boost/container/map.hpp
* boost/boost/container/node_allocator.hpp
* boost/boost/container/node_handle.hpp
* boost/boost/container/options.hpp
* boost/boost/container/pmr/deque.hpp
* boost/boost/container/pmr/flat_map.hpp
* boost/boost/container/pmr/flat_set.hpp
* boost/boost/container/pmr/list.hpp
* boost/boost/container/pmr/map.hpp
* boost/boost/container/pmr/resource_adaptor.hpp
* boost/boost/container/pmr/set.hpp
* boost/boost/container/pmr/slist.hpp
* boost/boost/container/pmr/small_vector.hpp
* boost/boost/container/pmr/stable_vector.hpp
* boost/boost/container/pmr/string.hpp
* boost/boost/container/pmr/vector.hpp
* boost/boost/container/scoped_allocator.hpp
* boost/boost/container/scoped_allocator_fwd.hpp
* boost/boost/container/set.hpp
* boost/boost/container/slist.hpp
* boost/boost/container/small_vector.hpp
* boost/boost/container/stable_vector.hpp
* boost/boost/container/static_vector.hpp
* boost/boost/container/string.hpp
* boost/boost/container/vector.hpp
* boost/boost/container_hash/
* boost/boost/contract.hpp
* boost/boost/contract_macro.hpp
* boost/boost/convert.hpp
* boost/boost/core/addressof.hpp
* boost/boost/core/alloc_construct.hpp
* boost/boost/core/allocator_access.hpp
* boost/boost/core/checked_delete.hpp
* boost/boost/core/default_allocator.hpp
* boost/boost/core/demangle.hpp
* boost/boost/core/empty_value.hpp
* boost/boost/core/enable_if.hpp
* boost/boost/core/exchange.hpp
* boost/boost/core/explicit_operator_bool.hpp
* boost/boost/core/first_scalar.hpp
* boost/boost/core/ignore_unused.hpp
* boost/boost/core/is_same.hpp
* boost/boost/core/lightweight_test.hpp
* boost/boost/core/lightweight_test_trait.hpp
* boost/boost/core/noinit_adaptor.hpp
* boost/boost/core/noncopyable.hpp
* boost/boost/core/null_deleter.hpp
* boost/boost/core/nvp.hpp
* boost/boost/core/pointer_traits.hpp
* boost/boost/core/quick_exit.hpp
* boost/boost/core/ref.hpp
* boost/boost/core/scoped_enum.hpp
* boost/boost/core/swap.hpp
* boost/boost/core/typeinfo.hpp
* boost/boost/core/uncaught_exceptions.hpp
* boost/boost/core/underlying_type.hpp
* boost/boost/core/use_default.hpp
* boost/boost/crc.hpp
* boost/boost/cregex.hpp
* boost/boost/cstdfloat.hpp
* boost/boost/cstdlib.hpp
* boost/boost/current_function.hpp
* boost/boost/cxx11_char_types.hpp
* boost/boost/date_time.hpp
* boost/boost/detail/algorithm.hpp
* boost/boost/detail/allocator_utilities.hpp
* boost/boost/detail/atomic_count.hpp
* boost/boost/detail/basic_pointerbuf.hpp
* boost/boost/detail/binary_search.hpp
* boost/boost/detail/bitmask.hpp
* boost/boost/detail/call_traits.hpp
* boost/boost/detail/catch_exceptions.hpp
* boost/boost/detail/compressed_pair.hpp
* boost/boost/detail/container_fwd.hpp
* boost/boost/detail/fenv.hpp
* boost/boost/detail/has_default_constructor.hpp
* boost/boost/detail/identifier.hpp
* boost/boost/detail/indirect_traits.hpp
* boost/boost/detail/interlocked.hpp
* boost/boost/detail/is_incrementable.hpp
* boost/boost/detail/is_sorted.hpp
* boost/boost/detail/is_xxx.hpp
* boost/boost/detail/iterator.hpp
* boost/boost/detail/lcast_precision.hpp
* boost/boost/detail/lightweight_main.hpp
* boost/boost/detail/lightweight_mutex.hpp
* boost/boost/detail/lightweight_test.hpp
* boost/boost/detail/lightweight_test_report.hpp
* boost/boost/detail/lightweight_thread.hpp
* boost/boost/detail/named_template_params.hpp
* boost/boost/detail/no_exceptions_support.hpp
* boost/boost/detail/numeric_traits.hpp
* boost/boost/detail/ob_compressed_pair.hpp
* boost/boost/detail/quick_allocator.hpp
* boost/boost/detail/reference_content.hpp
* boost/boost/detail/scoped_enum_emulation.hpp
* boost/boost/detail/select_type.hpp
* boost/boost/detail/sp_typeinfo.hpp
* boost/boost/detail/templated_streams.hpp
* boost/boost/detail/utf8_codecvt_facet.hpp
* boost/boost/detail/utf8_codecvt_facet.ipp
* boost/boost/detail/winapi/
* boost/boost/dll.hpp
* boost/boost/dynamic_bitset.hpp
* boost/boost/dynamic_bitset_fwd.hpp
* boost/boost/enable_shared_from_this.hpp
* boost/boost/endian.hpp
* boost/boost/exception_ptr.hpp
* boost/boost/filesystem.hpp
* boost/boost/flyweight.hpp
* boost/boost/foreach.hpp
* boost/boost/foreach_fwd.hpp
* boost/boost/format.hpp
* boost/boost/function.hpp
* boost/boost/function_equal.hpp
* boost/boost/function_output_iterator.hpp
* boost/boost/functional.hpp
* boost/boost/generator_iterator.hpp
* boost/boost/geometry.hpp
* boost/boost/get_pointer.hpp
* boost/boost/gil.hpp
* boost/boost/hana.hpp
* boost/boost/histogram.hpp
* boost/boost/hof.hpp
* boost/boost/implicit_cast.hpp
* boost/boost/indirect_reference.hpp
* boost/boost/integer.hpp
* boost/boost/integer_fwd.hpp
* boost/boost/integer_traits.hpp
* boost/boost/intrusive/any_hook.hpp
* boost/boost/intrusive/avl_set.hpp
* boost/boost/intrusive/avl_set_hook.hpp
* boost/boost/intrusive/avltree.hpp
* boost/boost/intrusive/avltree_algorithms.hpp
* boost/boost/intrusive/bs_set.hpp
* boost/boost/intrusive/bs_set_hook.hpp
* boost/boost/intrusive/bstree.hpp
* boost/boost/intrusive/bstree_algorithms.hpp
* boost/boost/intrusive/circular_slist_algorithms.hpp
* boost/boost/intrusive/derivation_value_traits.hpp
* boost/boost/intrusive/detail/algorithm.hpp
* boost/boost/intrusive/detail/any_node_and_algorithms.hpp
* boost/boost/intrusive/detail/array_initializer.hpp
* boost/boost/intrusive/detail/avltree_node.hpp
* boost/boost/intrusive/detail/bstree_algorithms_base.hpp
* boost/boost/intrusive/detail/default_header_holder.hpp
* boost/boost/intrusive/detail/ebo_functor_holder.hpp
* boost/boost/intrusive/detail/empty_node_checker.hpp
* boost/boost/intrusive/detail/equal_to_value.hpp
* boost/boost/intrusive/detail/exception_disposer.hpp
* boost/boost/intrusive/detail/function_detector.hpp
* boost/boost/intrusive/detail/generic_hook.hpp
* boost/boost/intrusive/detail/get_value_traits.hpp
* boost/boost/intrusive/detail/hashtable_node.hpp
* boost/boost/intrusive/detail/hook_traits.hpp
* boost/boost/intrusive/detail/iiterator.hpp
* boost/boost/intrusive/detail/is_stateful_value_traits.hpp
* boost/boost/intrusive/detail/iterator.hpp
* boost/boost/intrusive/detail/key_nodeptr_comp.hpp
* boost/boost/intrusive/detail/list_iterator.hpp
* boost/boost/intrusive/detail/list_node.hpp
* boost/boost/intrusive/detail/minimal_less_equal_header.hpp
* boost/boost/intrusive/detail/node_cloner_disposer.hpp
* boost/boost/intrusive/detail/node_holder.hpp
* boost/boost/intrusive/detail/node_to_value.hpp
* boost/boost/intrusive/detail/parent_from_member.hpp
* boost/boost/intrusive/detail/rbtree_node.hpp
* boost/boost/intrusive/detail/reverse_iterator.hpp
* boost/boost/intrusive/detail/simple_disposers.hpp
* boost/boost/intrusive/detail/size_holder.hpp
* boost/boost/intrusive/detail/slist_iterator.hpp
* boost/boost/intrusive/detail/slist_node.hpp
* boost/boost/intrusive/detail/std_fwd.hpp
* boost/boost/intrusive/detail/transform_iterator.hpp
* boost/boost/intrusive/detail/tree_iterator.hpp
* boost/boost/intrusive/detail/tree_node.hpp
* boost/boost/intrusive/detail/tree_value_compare.hpp
* boost/boost/intrusive/detail/uncast.hpp
* boost/boost/intrusive/hashtable.hpp
* boost/boost/intrusive/list.hpp
* boost/boost/intrusive/list_hook.hpp
* boost/boost/intrusive/member_value_traits.hpp
* boost/boost/intrusive/options.hpp
* boost/boost/intrusive/pack_options.hpp
* boost/boost/intrusive/parent_from_member.hpp
* boost/boost/intrusive/pointer_plus_bits.hpp
* boost/boost/intrusive/priority_compare.hpp
* boost/boost/intrusive/rbtree.hpp
* boost/boost/intrusive/rbtree_algorithms.hpp
* boost/boost/intrusive/set.hpp
* boost/boost/intrusive/set_hook.hpp
* boost/boost/intrusive/sg_set.hpp
* boost/boost/intrusive/sgtree.hpp
* boost/boost/intrusive/sgtree_algorithms.hpp
* boost/boost/intrusive/slist.hpp
* boost/boost/intrusive/slist_hook.hpp
* boost/boost/intrusive/splay_set.hpp
* boost/boost/intrusive/splaytree.hpp
* boost/boost/intrusive/splaytree_algorithms.hpp
* boost/boost/intrusive/treap.hpp
* boost/boost/intrusive/treap_algorithms.hpp
* boost/boost/intrusive/treap_set.hpp
* boost/boost/intrusive/trivial_value_traits.hpp
* boost/boost/intrusive/unordered_set.hpp
* boost/boost/intrusive/unordered_set_hook.hpp
* boost/boost/intrusive_ptr.hpp
* boost/boost/io_fwd.hpp
* boost/boost/is_placeholder.hpp
* boost/boost/iterator.hpp
* boost/boost/iterator_adaptors.hpp
* boost/boost/lexical_cast.hpp
* boost/boost/limits.hpp
* boost/boost/local_function.hpp
* boost/boost/locale.hpp
* boost/boost/make_default.hpp
* boost/boost/make_shared.hpp
* boost/boost/make_unique.hpp
* boost/boost/math_fwd.hpp
* boost/boost/mem_fn.hpp
* boost/boost/memory_order.hpp
* boost/boost/metaparse.hpp
* boost/boost/move/algo/
* boost/boost/move/algorithm.hpp
* boost/boost/move/default_delete.hpp
* boost/boost/move/detail/destruct_n.hpp
* boost/boost/move/detail/iterator_to_raw_pointer.hpp
* boost/boost/move/detail/iterator_traits.hpp
* boost/boost/move/detail/move_helpers.hpp
* boost/boost/move/detail/placement_new.hpp
* boost/boost/move/detail/reverse_iterator.hpp
* boost/boost/move/detail/to_raw_pointer.hpp
* boost/boost/move/detail/unique_ptr_meta_utils.hpp
* boost/boost/move/iterator.hpp
* boost/boost/move/make_unique.hpp
* boost/boost/move/move.hpp
* boost/boost/move/traits.hpp
* boost/boost/move/unique_ptr.hpp
* boost/boost/move/utility.hpp
* boost/boost/mp11.hpp
* boost/boost/mpi.hpp
* boost/boost/multi_array.hpp
* boost/boost/multi_index_container.hpp
* boost/boost/multi_index_container_fwd.hpp
* boost/boost/next_prior.hpp
* boost/boost/non_type.hpp
* boost/boost/noncopyable.hpp
* boost/boost/nondet_random.hpp
* boost/boost/none.hpp
* boost/boost/none_t.hpp
* boost/boost/operators.hpp
* boost/boost/operators_v1.hpp
* boost/boost/optional.hpp
* boost/boost/outcome.hpp
* boost/boost/parameter.hpp
* boost/boost/pending/
* boost/boost/phoenix.hpp
* boost/boost/pointee.hpp
* boost/boost/pointer_cast.hpp
* boost/boost/pointer_to_other.hpp
* boost/boost/polymorphic_cast.hpp
* boost/boost/polymorphic_pointer_cast.hpp
* boost/boost/predef.h
* boost/boost/preprocessor.hpp
* boost/boost/preprocessor/arithmetic.hpp
* boost/boost/preprocessor/arithmetic/
* boost/boost/preprocessor/array.hpp
* boost/boost/preprocessor/array/
* boost/boost/preprocessor/assert_msg.hpp
* boost/boost/preprocessor/cat.hpp
* boost/boost/preprocessor/comma.hpp
* boost/boost/preprocessor/comma_if.hpp
* boost/boost/preprocessor/comparison.hpp
* boost/boost/preprocessor/control.hpp
* boost/boost/preprocessor/control/deduce_d.hpp
* boost/boost/preprocessor/control/detail/
* boost/boost/preprocessor/control/expr_if.hpp
* boost/boost/preprocessor/control/expr_iif.hpp
* boost/boost/preprocessor/control/if.hpp
* boost/boost/preprocessor/control/iif.hpp
* boost/boost/preprocessor/control/while.hpp
* boost/boost/preprocessor/debug.hpp
* boost/boost/preprocessor/dec.hpp
* boost/boost/preprocessor/detail/
* boost/boost/preprocessor/empty.hpp
* boost/boost/preprocessor/enum.hpp
* boost/boost/preprocessor/enum_params.hpp
* boost/boost/preprocessor/enum_params_with_a_default.hpp
* boost/boost/preprocessor/enum_params_with_defaults.hpp
* boost/boost/preprocessor/enum_shifted.hpp
* boost/boost/preprocessor/enum_shifted_params.hpp
* boost/boost/preprocessor/expand.hpp
* boost/boost/preprocessor/expr_if.hpp
* boost/boost/preprocessor/facilities.hpp
* boost/boost/preprocessor/facilities/
* boost/boost/preprocessor/for.hpp
* boost/boost/preprocessor/identity.hpp
* boost/boost/preprocessor/if.hpp
* boost/boost/preprocessor/inc.hpp
* boost/boost/preprocessor/iterate.hpp
* boost/boost/preprocessor/iteration.hpp
* boost/boost/preprocessor/iteration/detail/
* boost/boost/preprocessor/iteration/iterate.hpp
* boost/boost/preprocessor/iteration/local.hpp
* boost/boost/preprocessor/iteration/self.hpp
* boost/boost/preprocessor/library.hpp
* boost/boost/preprocessor/limits.hpp
* boost/boost/preprocessor/list.hpp
* boost/boost/preprocessor/list/adt.hpp
* boost/boost/preprocessor/list/append.hpp
* boost/boost/preprocessor/list/at.hpp
* boost/boost/preprocessor/list/cat.hpp
* boost/boost/preprocessor/list/detail/
* boost/boost/preprocessor/list/enum.hpp
* boost/boost/preprocessor/list/filter.hpp
* boost/boost/preprocessor/list/first_n.hpp
* boost/boost/preprocessor/list/fold_left.hpp
* boost/boost/preprocessor/list/fold_right.hpp
* boost/boost/preprocessor/list/for_each.hpp
* boost/boost/preprocessor/list/for_each_i.hpp
* boost/boost/preprocessor/list/for_each_product.hpp
* boost/boost/preprocessor/list/rest_n.hpp
* boost/boost/preprocessor/list/reverse.hpp
* boost/boost/preprocessor/list/size.hpp
* boost/boost/preprocessor/list/to_array.hpp
* boost/boost/preprocessor/list/to_seq.hpp
* boost/boost/preprocessor/list/to_tuple.hpp
* boost/boost/preprocessor/list/transform.hpp
* boost/boost/preprocessor/logical.hpp
* boost/boost/preprocessor/max.hpp
* boost/boost/preprocessor/min.hpp
* boost/boost/preprocessor/punctuation.hpp
* boost/boost/preprocessor/punctuation/
* boost/boost/preprocessor/repeat.hpp
* boost/boost/preprocessor/repeat_2nd.hpp
* boost/boost/preprocessor/repeat_3rd.hpp
* boost/boost/preprocessor/repeat_from_to.hpp
* boost/boost/preprocessor/repeat_from_to_2nd.hpp
* boost/boost/preprocessor/repeat_from_to_3rd.hpp
* boost/boost/preprocessor/repetition.hpp
* boost/boost/preprocessor/repetition/deduce_r.hpp
* boost/boost/preprocessor/repetition/deduce_z.hpp
* boost/boost/preprocessor/repetition/detail/
* boost/boost/preprocessor/repetition/enum.hpp
* boost/boost/preprocessor/repetition/enum_binary_params.hpp
* boost/boost/preprocessor/repetition/enum_params.hpp
* boost/boost/preprocessor/repetition/enum_params_with_a_default.hpp
* boost/boost/preprocessor/repetition/enum_params_with_defaults.hpp
* boost/boost/preprocessor/repetition/enum_shifted.hpp
* boost/boost/preprocessor/repetition/enum_shifted_binary_params.hpp
* boost/boost/preprocessor/repetition/enum_shifted_params.hpp
* boost/boost/preprocessor/repetition/enum_trailing.hpp
* boost/boost/preprocessor/repetition/enum_trailing_binary_params.hpp
* boost/boost/preprocessor/repetition/enum_trailing_params.hpp
* boost/boost/preprocessor/repetition/for.hpp
* boost/boost/preprocessor/repetition/repeat.hpp
* boost/boost/preprocessor/repetition/repeat_from_to.hpp
* boost/boost/preprocessor/selection.hpp
* boost/boost/preprocessor/seq.hpp
* boost/boost/preprocessor/seq/
* boost/boost/preprocessor/slot.hpp
* boost/boost/preprocessor/slot/
* boost/boost/preprocessor/stringize.hpp
* boost/boost/preprocessor/tuple.hpp
* boost/boost/preprocessor/tuple/
* boost/boost/preprocessor/variadic.hpp
* boost/boost/preprocessor/variadic/
* boost/boost/preprocessor/while.hpp
* boost/boost/preprocessor/wstringize.hpp
* boost/boost/process.hpp
* boost/boost/program_options.hpp
* boost/boost/progress.hpp
* boost/boost/python.hpp
* boost/boost/random.hpp
* boost/boost/range.hpp
* boost/boost/ratio.hpp
* boost/boost/rational.hpp
* boost/boost/ref.hpp
* boost/boost/regex.h
* boost/boost/regex.hpp
* boost/boost/regex_fwd.hpp
* boost/boost/scope_exit.hpp
* boost/boost/scoped_array.hpp
* boost/boost/scoped_ptr.hpp
* boost/boost/shared_array.hpp
* boost/boost/shared_container_iterator.hpp
* boost/boost/shared_ptr.hpp
* boost/boost/signals2.hpp
* boost/boost/smart_ptr.hpp
* boost/boost/spirit.hpp
* boost/boost/stacktrace.hpp
* boost/boost/static_string.hpp
* boost/boost/swap.hpp
* boost/boost/thread.hpp
* boost/boost/throw_exception.hpp
* boost/boost/timer.hpp
* boost/boost/token_functions.hpp
* boost/boost/token_iterator.hpp
* boost/boost/tokenizer.hpp
* boost/boost/type.hpp
* boost/boost/type_index.hpp
* boost/boost/type_traits.hpp
* boost/boost/type_traits/
* boost/boost/unordered_map.hpp
* boost/boost/unordered_set.hpp
* boost/boost/utility.hpp
* boost/boost/variant.hpp
* boost/boost/version.hpp
* boost/boost/visit_each.hpp
* boost/boost/wave.hpp
* boost/boost/weak_ptr.hpp
* boost/doc/
* boost/libs/*/bench/
* boost/libs/*/checks/
* boost/libs/*/doc/
* boost/libs/*/example/
* boost/libs/*/proj/
* boost/libs/*/test/
* boost/more/
* boost/status/
* boost/tools/

Copyright: various
Download URL: https://dl.bintray.com/boostorg/release/1.74.0/source/boost_1_74_0.tar.bz2
Download date: August 25, 2020
Git commit: a7090e8ce184501cfc9e80afa6cafb5bfd3b371c (et al)
License file: boost/LICENSE_1_0.txt
License type: BSL-1.0
Location: boost
Project URL: https://www.boost.org/
Release URL: https://www.boost.org/users/history/version_1_74_0.html
Release date: August 14th, 2020
Version: 1.74.0

## gnulib

The gnulib directory contains Gnulib, the GNU portability library.

The following Gnulib modules have been imported:

* getopt-gnu (and transitive dependencies)

Run the following commands to import:

    $ gnulib-tool --lgpl --import getopt-gnu

For details on importing or upgrading Gnulib, see Gnulib's documentation:
https://www.gnu.org/software/gnulib/manual/html_node/Invoking-gnulib_002dtool.html

Copyright: Copyright (C) 2002-2020 Free Software Foundation, Inc.
Download URL: git://git.savannah.gnu.org/gnulib.git
Download date: July 28, 2020
Git commit: 5e50baa16ef90204d9048a9e2f23c5a538955121
License file: N/A
License type: FSFULLR, GPL-3.0-or-later, LGPL-2.0-or-later, LGPL-2.1-or-later
Location: gnulib
Project URL: https://www.gnu.org/software/gnulib/
Release URL: N/A
Release date: N/A
Version: N/A

## googletest

The googletest directory contains Google Test, a testing framework for C++
applications, and Google Mock, its companion mocking framework.

The following patches have been manually applied:

* googletest-result-of.patch

Copyright: Copyright 2008, Google Inc.
Download URL: https://github.com/google/googletest/archive/release-1.10.0.tar.gz
Download date: July 27, 2020
Git commit: 703bd9caab50b139428cea1aaff9974ebee5742e + patches
License file: googletest/LICENSE, googletest/googlemock/LICENSE, googletest/googlemock/scripts/generator/LICENSE, googletest/googletest/LICENSE
License type: Apache-2.0, BSD-3-Clause
Location: googletest
Project URL: https://github.com/google/googletest
Release URL: https://github.com/google/googletest/releases/tag/release-1.10.0
Release date: October 3, 2019
Version: 1.10.0

## jsoncpp

The jsoncpp directory contains JsonCpp, a C++ library that allows manipulating
JSON values.

The following directories have been deleted to reduce storage consumption:

* jsoncpp/src/test_lib_json/
* jsoncpp/test/

Copyright: Public Domain; Copyright (c) 2007-2010 Baptiste Lepilleur and The JsonCpp Authors
Download URL: https://github.com/open-source-parsers/jsoncpp/archive/1.9.3.tar.gz
Download date: August 13, 2020
Git commit: 6aba23f4a8628d599a9ef7fa4811c4ff6e4070e2
License file: jsoncpp/LICENSE
License type: Public Domain; MIT
Location: jsoncpp
Project URL: https://github.com/open-source-parsers/jsoncpp
Release URL: https://github.com/open-source-parsers/jsoncpp/releases/tag/1.9.3
Release date: May 29, 2020
Version: 1.9.3
