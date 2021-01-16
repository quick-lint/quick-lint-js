import subprocess
import os
import sys
import json
from time import gmtime, strftime
import platform

if(len(sys.argv) != 2):
    print("Usage: ./precise_benchmarks.py <PROCESS NAME>")
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
        json.dump(result_dict, json_dump_file, sort_keys=True, indent=4)

def main():
    code, out, err = run(["valgrind", "--tool=cachegrind", sys.argv[1]])
    cachegrind_results = cachegrind_processor(code, out.decode('utf-8'))
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
    output_file_name = strftime("%Y-%m-%d %H:%M:%S", gmtime())
    create_json_from_dict(cachegrind_result_dict, output_file_name)
    
if __name__ == "__main__":
    main()
