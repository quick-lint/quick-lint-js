#!/bin/bash

export AFL_USE_ASAN=1
~/src/afl-2.41b/afl-g++ -m32 -std=c++11 -I../include -O2 -g -o sajson-fuzz ../example/main.cpp
