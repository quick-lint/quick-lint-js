#!/bin/bash

AFL_SKIP_CPUFREQ=1 ~/src/afl-2.41b/afl-fuzz -i testcase_dir -o findings_dir -x json.dict -- ./sajson-fuzz @@
