import subprocess
import os
import sys
import json
from time import gmtime, strftime
import platform
import git


if(len(sys.argv) != 2):
    print("Usage: ./precise_benchmarks.py <ABS PATH TO PROCESS>")
    exit(1);

def run(cmd):
    proc = subprocess.Popen(cmd,
        stdout = subprocess.PIPE,
        stderr = subprocess.STDOUT,
    )
    stdout, stderr = proc.communicate()
    return proc.returncode, stdout, stderr
 
def get_colon_index(string):
    index = 0
    found = False
    for char in string:
        if char == ':':
            Found = True
            return index+1
        index = index + 1
    if not found:
        return -1

def preprocess_brackets(string):
    index = 0
    string_has_bracket = False
    for char in string:
        if char == '(':
            string_has_bracket = True
            break
        index = index + 1
    if string_has_bracket:
        return string[:index]
    else:
        return string

def cachegrind_processor(code, out):
    cachegrind_linesplit = []
    for line in out.splitlines():
        cachegrind_linesplit.append(line)
    cachegrind_split = cachegrind_linesplit[-15:] 
    cachegrind_results = []
    for index, values in enumerate(cachegrind_split):
        token_index = get_colon_index(cachegrind_split[index])
        if token_index > 0:
            token_value = cachegrind_split[index][token_index:].strip()
            preprocessed_token_value = preprocess_brackets(token_value)
            cachegrind_results.append(preprocessed_token_value)
    return cachegrind_results

def create_json_from_dict(result_dict, file_name):
    with open(file_name, "w") as json_dump_file:
        json.dump(result_dict, json_dump_file, sort_keys=False, indent=4)

def memcheck_processor(code, out):
    memcheck_linesplit = []
    for line in out.splitlines():
        memcheck_linesplit.append(line)
    memcheck_split = memcheck_linesplit[-15:] 
    memcheck_results = []
    for index, values in enumerate(memcheck_split):
        token_index = get_colon_index(memcheck_split[index])
        if token_index > 0:
            token_value = memcheck_split[index][token_index:].strip()
            preprocessed_token_value = preprocess_brackets(token_value)
            memcheck_results.append(preprocessed_token_value)
    return memcheck_results


def main():
    code_cache, out_cache, err = run(["valgrind", "--tool=cachegrind", sys.argv[1]])
    cachegrind_results = cachegrind_processor(code_cache, out_cache.decode('utf-8'))
    code_mem, out_mem, err = run(["valgrind", "--tool=memcheck", sys.argv[1]])
    memcheck_results = memcheck_processor(code_mem, out_mem.decode('utf-8'))
    repo = git.Repo(search_parent_directories=True)
    useful_bench_info = {
            "Operating System:": platform.platform(),
            "Platform Uname:": platform.uname(),
            "Platform Processor:": platform.processor(),
            "System Type:": platform.system(),
            "Binary Size:": f"{os.path.getsize(sys.argv[1])} bytes",
            "Git Commit:": repo.head.object.hexsha,
    }
    cachegrind_result_dict = {
            "I_ref": cachegrind_results[0],
            "I1_misses": cachegrind_results[1],
            "LLi_misses": cachegrind_results[2],
            "I1_miss_rate": cachegrind_results[3],
            "LLi_miss_rate": cachegrind_results[4],
            "D_ref": cachegrind_results[5],
            "D1_misses": cachegrind_results[6],
            "LLd_misses": cachegrind_results[7],
            "D1_miss_rate": cachegrind_results[8],
            "LLd_miss_rate": cachegrind_results[9],
            "LL_ref": cachegrind_results[10],
            "LL_misses": cachegrind_results[11],
            "LL_miss_rate": cachegrind_results[12],
    }
    memcheck_result_dict = {
            "HEAP total usage at exit:": memcheck_results[1],
            "HEAP total usage:": memcheck_results[2]
    }
    final_dict_dump = useful_bench_info
    final_dict_dump.update(cachegrind_result_dict)
    final_dict_dump.update(memcheck_result_dict)
    output_file_name = strftime("%Y-%m-%d %H:%M:%S", gmtime())
    create_json_from_dict(final_dict_dump, output_file_name)
    
if __name__ == "__main__":
    main()
    os.system("rm -f cachegrind.out*")
