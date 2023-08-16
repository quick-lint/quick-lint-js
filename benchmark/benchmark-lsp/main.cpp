// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <map>
#include <memory>
#include <quick-lint-js/benchmark-config.h>
#include <quick-lint-js/cli/arg-parser.h>
#include <quick-lint-js/container/result.h>
#include <quick-lint-js/container/string-view.h>
#include <quick-lint-js/io/file.h>
#include <quick-lint-js/io/output-stream.h>
#include <quick-lint-js/lsp-benchmarks.h>
#include <quick-lint-js/lsp-logging.h>
#include <quick-lint-js/lsp-server-process.h>
#include <unistd.h>

using namespace quick_lint_js;
using namespace std::literals::string_view_literals;

namespace quick_lint_js {
FILE* log_file = nullptr;
bool log_colors = false;
}

namespace {
struct Benchmark_Run_Config {
  int warmup_iterations;
  int measurement_iterations;

  int samples;
};

struct parsed_args {
  std::string_view benchmark_filter = std::string_view();
  const char* output_json_path = nullptr;
  bool list_benchmarks = false;
  Benchmark_Run_Config run_config = {
      .warmup_iterations = 1,
      .measurement_iterations = 10,
      .samples = 1,
  };
};

parsed_args parse_arguments(int argc, char** argv);

// {
//   "data": [
//     {
//       "benchmarkName": "quick-lint-js/open-wait-close/tiny.js",
//       "warmupIterations": 10,
//       "measurementIterations": 100,
//       "samples": {
//         "durationPerIteration": [10, 20, 15]
//       }
//     }
//   ]
// }
class Benchmark_Results_Writer {
 public:
  // json_output is optional.
  // verbose_output is optional.
  explicit Benchmark_Results_Writer(FILE* json_output, FILE* verbose_output)
      : json_output_(json_output), verbose_output_(verbose_output) {}

  void add_metadata(const Benchmark_Config_Program& program_config) {
    if (this->json_output_) {
      this->metadatas_[program_config.name] = program_config.get_metadata();
    }
  }

  void begin_benchmark(const char* name,
                       const Benchmark_Run_Config& run_config) {
    QLJS_ASSERT(this->current_benchmark_.samples.empty());

    this->current_benchmark_.benchmark_name = name;
    this->current_benchmark_.warmup_iterations = run_config.warmup_iterations;
    this->current_benchmark_.measurement_iterations =
        run_config.measurement_iterations;

    this->current_benchmark_.samples.reserve(
        narrow_cast<std::size_t>(run_config.samples));

    if (this->verbose_output_) {
      std::fprintf(this->verbose_output_, "=== %s ===\n", name);
      std::fflush(this->verbose_output_);
    }
  }

  void end_benchmark() {
    this->benchmark_results_.emplace_back(std::move(this->current_benchmark_));
    this->current_benchmark_ = Benchmark_Result();
  }

  void write_sample(double duration_per_iteration) {
    this->current_benchmark_.samples.emplace_back(
        sample{.duration_per_iteration = duration_per_iteration});

    if (this->verbose_output_) {
      std::fprintf(this->verbose_output_, "%.2f ms per iteration\n",
                   duration_per_iteration * 1000);
      std::fflush(this->verbose_output_);
    }
  }

  void done() {
    if (this->json_output_) {
      Memory_Output_Stream json;

      json.append_copy(u8"{\n  \"data\": [\n"_sv);
      bool need_data_comma = false;
      for (const Benchmark_Result& result : this->benchmark_results_) {
        if (need_data_comma) {
          json.append_copy(u8",\n"_sv);
        }
        json.append_copy(u8"    {\n      \"benchmarkName\": \""_sv);
        write_json_escaped_string(json, to_string8_view(result.benchmark_name));
        json.append_copy(u8"\",\n      \"warmupIterations\": "_sv);
        json.append_decimal_integer(result.warmup_iterations);
        json.append_copy(u8",\n      \"measurementIterations\": "_sv);
        json.append_decimal_integer(result.measurement_iterations);
        json.append_copy(
            u8",\n      \"samples\": {\n      \"durationPerIteration\": ["_sv);
        bool need_sample_comma = false;
        for (const sample& s : result.samples) {
          if (need_sample_comma) {
            json.append_copy(u8", "_sv);
          }
          json.append_decimal_float_slow(s.duration_per_iteration);
          need_sample_comma = true;
        }
        json.append_copy(u8"]\n      }\n    }"_sv);
        need_data_comma = true;
      }

      json.append_copy(u8"\n  ],\n  \"metadata\": {\n"_sv);
      bool need_metadata_comma = false;
      for (const auto& [program_name, program_metadata] : this->metadatas_) {
        if (need_metadata_comma) {
          json.append_copy(u8",\n"_sv);
        }
        json.append_copy(u8"    \""_sv);
        write_json_escaped_string(json, to_string8_view(program_name));
        json.append_copy(u8"\": {\n"_sv);
        bool need_metadata_entry_comma = false;
        for (const auto& [key, value] : program_metadata) {
          if (need_metadata_entry_comma) {
            json.append_copy(u8",\n"_sv);
          }
          json.append_copy(u8"      \""_sv);
          write_json_escaped_string(json, to_string8_view(key));
          json.append_copy(u8"\": \""_sv);
          write_json_escaped_string(json, to_string8_view(value));
          json.append_copy(u8"\""_sv);
          need_metadata_entry_comma = true;
        }
        json.append_copy(u8"\n    }\n"_sv);
        need_metadata_comma = true;
      }
      json.append_copy(u8"  }\n"_sv);

      json.append_copy(u8"}\n"_sv);
      json.flush();
      String8 json_string = json.get_flushed_string8();

      std::size_t written = std::fwrite(json_string.data(), 1,
                                        json_string.size(), this->json_output_);
      if (written != json_string.size()) {
        std::fprintf(stderr, "error: failed to write JSON\n");
        std::exit(1);
      }
    }
  }

 private:
  struct sample {
    double duration_per_iteration;
  };

  struct Benchmark_Result {
    std::string benchmark_name;
    int warmup_iterations;
    int measurement_iterations;
    std::vector<sample> samples;
  };

  FILE* json_output_;
  FILE* verbose_output_;
  Benchmark_Result current_benchmark_;
  std::vector<Benchmark_Result> benchmark_results_;
  std::map<std::string, std::map<std::string, std::string>> metadatas_;
};

void run_benchmark(Benchmark_Factory&, const Benchmark_Config_Server&,
                   const Benchmark_Run_Config&,
                   Benchmark_Results_Writer& results);
void run_benchmark_once(Benchmark*, const Benchmark_Config_Server&,
                        const Benchmark_Run_Config&,
                        Benchmark_Results_Writer& results);
}

int main(int argc, char** argv) {
  parsed_args args = parse_arguments(argc, argv);

  FILE* output_json_file = nullptr;
  if (args.output_json_path) {
    output_json_file = std::fopen(args.output_json_path, "w");
    if (!output_json_file) {
      std::fprintf(stderr, "error: failed to open %s: %s\n",
                   args.output_json_path, std::strerror(errno));
      std::exit(1);
    }
  }
  Benchmark_Results_Writer results(/*json_output=*/output_json_file,
                                   /*verbose_output=*/stdout);

  Benchmark_Config config = Benchmark_Config::load();
  std::vector<Benchmark_Factory> benchmark_factories =
      get_benchmark_factories();

  for (Benchmark_Config_Server& server_config : config.servers) {
    for (Benchmark_Factory& factory : benchmark_factories) {
      std::unique_ptr<Benchmark> b = factory();
      if (!b->is_supported(server_config)) {
        continue;
      }
      std::string benchmark_name = server_config.name + "/" + b->name();
      if (!contains(benchmark_name, args.benchmark_filter)) {
        continue;
      }

      if (args.list_benchmarks) {
        std::puts(benchmark_name.c_str());
      } else {
        results.begin_benchmark(benchmark_name.c_str(), args.run_config);
        run_benchmark(factory, server_config, args.run_config, results);
        results.end_benchmark();

        auto program_it = std::find_if(
            config.programs.begin(), config.programs.end(),
            [&](const Benchmark_Config_Program& program_config) {
              return program_config.name == server_config.program_name;
            });
        if (program_it != config.programs.end() &&
            !program_it->dumped_metadata) {
          results.add_metadata(*program_it);
          program_it->dumped_metadata = true;
        }
      }
    }
  }

  results.done();
  return 0;
}

namespace {
parsed_args parse_arguments(int argc, char** argv) {
  auto read_number = [](const char* arg_value) {
    int output_number;
    if (parse_integer_exact(std::string_view(arg_value), output_number) !=
        Parse_Integer_Exact_Error::ok) {
      std::fprintf(stderr, "error: failed to parse number: %s\n", arg_value);
      std::exit(2);
    }
    return output_number;
  };

  parsed_args args;

  Arg_Parser parser(argc, argv);
  while (!parser.done()) {
    if (const char* argument = parser.match_argument()) {
      if (args.benchmark_filter.empty()) {
        args.benchmark_filter = argument;
      } else {
        std::fprintf(stderr, "error: unexpected argument: %s\n", argument);
        std::exit(2);
      }
    } else if (parser.match_flag_option("--list"sv, "--list"sv)) {
      args.list_benchmarks = true;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--output-json"sv)) {
      args.output_json_path = arg_value;
    } else if (const char* arg_value =
                   parser.match_option_with_value("--samples"sv)) {
      args.run_config.samples = read_number(arg_value);
    } else if (const char* arg_value =
                   parser.match_option_with_value("--iterations"sv)) {
      args.run_config.measurement_iterations = read_number(arg_value);
    } else if (const char* arg_value =
                   parser.match_option_with_value("--warmup-iterations"sv)) {
      args.run_config.warmup_iterations = read_number(arg_value);
    } else {
      const char* unrecognized = parser.match_anything();
      std::fprintf(stderr, "error: invalid option: %s\n", unrecognized);
      std::exit(2);
    }
  }
  if (args.list_benchmarks) {
    args.output_json_path = nullptr;
  }

  const char* debug_value = std::getenv("QLJS_BENCHMARK_LSP_DEBUG");
  if (debug_value && debug_value[0] != '\0' &&
      std::strcmp(debug_value, "0") != 0) {
    log_file = stderr;
    log_colors = ::isatty(STDERR_FILENO);
  }

  return args;
}

void run_benchmark(Benchmark_Factory& factory,
                   const Benchmark_Config_Server& server_config,
                   const Benchmark_Run_Config& run_config,
                   Benchmark_Results_Writer& results) {
  for (int i = 0; i < run_config.samples; ++i) {
    std::unique_ptr<Benchmark> b = factory();
    run_benchmark_once(b.get(), server_config, run_config, results);
  }
}

void run_benchmark_once(Benchmark* b,
                        const Benchmark_Config_Server& server_config,
                        const Benchmark_Run_Config& run_config,
                        Benchmark_Results_Writer& results) {
  LSP_Server_Process server = LSP_Server_Process::spawn(server_config);
  server.run_and_kill([&]() -> LSP_Task<void> {
    co_await server.initialize_lsp_async();

    int total_iteration_count =
        run_config.warmup_iterations + run_config.measurement_iterations;
    co_await b->set_up_async(server, server_config, total_iteration_count);
    co_await b->run_iterations_async(server, 0, run_config.warmup_iterations);
    auto begin = std::chrono::steady_clock::now();
    co_await b->run_iterations_async(server, run_config.warmup_iterations,
                                     total_iteration_count);
    auto end = std::chrono::steady_clock::now();

    results.write_sample(/*duration_per_iteration=*/std::chrono::duration_cast<
                             std::chrono::duration<double>>(end - begin)
                             .count() /
                         run_config.measurement_iterations);

    co_await server.shut_down_lsp();
  });
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
