echo "this is a template - TODO: allow master/slave name as argument"
exit 1

AFL_SKIP_CPUFREQ=1 ~/src/afl-2.41b/afl-fuzz -i testcase_dir/ -o findings_dir/ -M fuzzer01 -- ./sajson-fuzz @@
