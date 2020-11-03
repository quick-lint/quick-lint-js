#include <memory>
#include <sajson.h>
#include <vector>

const char* default_files[] = {
    "testdata/apache_builds.json", "testdata/github_events.json",
    "testdata/instruments.json",   "testdata/mesh.json",
    "testdata/mesh.pretty.json",   "testdata/nested.json",
    "testdata/svg_menu.json",      "testdata/truenull.json",
    "testdata/twitter.json",       "testdata/update-center.json",
    "testdata/whitespace.json",
};
const size_t default_files_count
    = sizeof(default_files) / sizeof(*default_files);

template <typename AllocationStrategy>
void run_benchmark(size_t max_string_length, const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("fopen failed");
        return;
    }

    std::unique_ptr<FILE, int (*)(FILE*)> deleter(file, fclose);

    if (fseek(file, 0, SEEK_END)) {
        perror("fseek failed");
        return;
    }
    size_t length = ftell(file);
    if (fseek(file, 0, SEEK_SET)) {
        perror("fseek failed");
        return;
    }

    std::vector<char> buffer(length);
    if (fread(buffer.data(), length, 1, file) != 1) {
        perror("fread failed");
        return;
    }

    deleter.reset();

    clock_t minimum_each = std::numeric_limits<clock_t>::max();

    clock_t start = clock();

    const size_t N = 1000;
    for (size_t i = 0; i < N; ++i) {
        clock_t before_each = clock();
        sajson::parse(
            AllocationStrategy(), sajson::string(buffer.data(), buffer.size()));
        clock_t elapsed_each = clock() - before_each;
        minimum_each = std::min(minimum_each, elapsed_each);
    }

    clock_t elapsed = clock() - start;

    double average_elapsed_ms = 1000.0 * elapsed / CLOCKS_PER_SEC / N;
    double minimum_elapsed_ms = 1000.0 * minimum_each / CLOCKS_PER_SEC;
    printf(
        "%*s - %0.3f ms - %0.3f ms\n",
        static_cast<int>(max_string_length),
        filename,
        average_elapsed_ms,
        minimum_elapsed_ms);
}

template <typename AllocationStrategy>
void run_all(size_t files_count, const char** files) {
    size_t max_string_length = 0;
    for (size_t i = 0; i < files_count; ++i) {
        max_string_length = std::max(max_string_length, strlen(files[i]));
    }
    printf(
        "%*s - %8s - %8s\n",
        static_cast<int>(max_string_length),
        "file",
        "avg",
        "min");
    printf(
        "%*s - %8s - %8s\n",
        static_cast<int>(max_string_length),
        "----",
        "---",
        "---");
    for (size_t i = 0; i < files_count; ++i) {
        run_benchmark<AllocationStrategy>(max_string_length, files[i]);
    }
}

int main(int argc, const char** argv) {
    if (argc > 1) {
        // printf("\n=== SINGLE ALLOCATION ===\n\n");
        run_all<sajson::single_allocation>(argc - 1, argv + 1);
        // printf("\n=== DYNAMIC ALLOCATION ===\n\n");
        // run_all<sajson::dynamic_allocation>(argc - 1, argv + 1);
    } else {
        // printf("\n=== SINGLE ALLOCATION ===\n\n");
        run_all<sajson::single_allocation>(default_files_count, default_files);
        // printf("\n=== DYNAMIC ALLOCATION ===\n\n");
        // run_all<sajson::dynamic_allocation>(default_files_count,
        // default_files);
    }
}
