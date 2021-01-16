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

def generic_processor(code, out):
    generic_linesplit = []
    for line in out.splitlines():
     generic_linesplit.append(line)
    generic_split = generic_linesplit[-15:] 
    generic_results = []
    for index, values in enumerate(generic_split):
     token_index = get_colon_index(generic_split[index])
     if token_index > 0:
         token_value = generic_split[index][token_index:].strip()
         preprocessed_token_value = preprocess_brackets(token_value)
         generic_results.append(preprocessed_token_value)
    return generic_results

def create_json_from_dict(result_dict, file_name):
    with open(file_name, "w") as json_dump_file:
     json.dump(result_dict, json_dump_file, sort_keys=False, indent=4)

def remove_comma(string):
    returner = ""
    for char in string:
     if char != ',':
         returner += str(char)
     else:
         continue
    return returner

def remove_percent(string):
    returner = ""
    for char in string:
     if char != '%':
         returner += str(char)
     else:
         continue
    return returner

def get_space_index(string):
    index = 0
    has_space = False
    for char in string:
     if char == ' ':
         has_space = True 
         index+1
         break;
     index = index + 1
    if has_space:
     return index
    else:
     return -1

def percent_to_float(percent):
    return float(percent/100)

def split_commas_into_token(string):
    return [str_.strip() for str_ in string.split(',')]
def split_space_into_token(string):
    return [str_.strip() for str_ in string.split(' ')]
 
def main():
    code_cache, out_cache, err = run(["valgrind", "--tool=cachegrind", sys.argv[1]])
    cachegrind_results = generic_processor(code_cache, out_cache.decode('utf-8'))
    code_mem, out_mem, err = run(["valgrind", "--tool=memcheck", sys.argv[1]])
    memcheck_results = generic_processor(code_mem, out_mem.decode('utf-8'))
    code_instr, out_instr, err = run(["valgrind", "--tool=callgrind", sys.argv[1]])
    instr_results = generic_processor(code_instr, out_instr.decode('utf-8'))
    repo = git.Repo(search_parent_directories=True)
    useful_bench_info = {
            "Operating System": platform.platform(),
            "Platform Uname": platform.uname(),
            "Binary Size": int(os.path.getsize(sys.argv[1])),
            "Git Commit": repo.head.object.hexsha,
    }
    cachegrind_result_dict = {
            "I_ref": int(remove_comma(cachegrind_results[0].strip())),
            "I1_misses": int(remove_comma(cachegrind_results[1].strip())),
            "LLi_misses": int(remove_comma(cachegrind_results[2].strip())),
            "I1_miss_rate": percent_to_float(float(remove_percent(cachegrind_results[3].strip()))),
            "LLi_miss_rate": percent_to_float(float(remove_percent(remove_percent(cachegrind_results[4].strip())))),
            "D_ref": int(remove_comma(cachegrind_results[5].strip())),
            "D1_misses": int(remove_comma(cachegrind_results[6].strip())),
            "LLd_misses": int(remove_comma(cachegrind_results[7].strip())),
            "D1_miss_rate": percent_to_float(float(remove_percent(cachegrind_results[8]))),
            "LLd_miss_rate": percent_to_float(float(remove_percent(cachegrind_results[9].strip()))),
            "LL_ref": int(remove_comma(cachegrind_results[10].strip())),
            "LL_misses": int(remove_comma(cachegrind_results[11].strip())),
            "LL_miss_rate": percent_to_float(float(remove_percent(cachegrind_results[12].strip()))),
    }
    memcheck_result_dict = {
            "HEAP total usage at exit": int(split_space_into_token(remove_comma(memcheck_results[1]))[0]),
            "HEAP total usage": int(split_space_into_token(remove_comma(memcheck_results[2]))[4])
    }
    instruction_result_dict = {
            "Instruction reads": int(remove_comma(instr_results[2]))
    }
    metrics_dict = cachegrind_result_dict;
    metrics_dict.update(memcheck_result_dict)
    metrics_dict.update(instruction_result_dict)
    final_dict = {
            "System Information": useful_bench_info,
            "Metrics": metrics_dict
    }
    output_file_name = strftime("%Y-%m-%d %H:%M:%S", gmtime())
    create_json_from_dict(final_dict, output_file_name)
    
if __name__ == "__main__":
    main()
    os.system("rm -f cachegrind.out* callgrind.out*")
