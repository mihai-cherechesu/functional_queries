import os
from collections import defaultdict
import subprocess


values = {
            "T1": 30,
            "T2": 15,
            "T3": 5,
            "T4": 20,
            "T5": 20,
            "T6": 10,
            "T7": 20
        }

test_meaning = {
            "T1": "Table Show",
            "T2": "Select",
            "T3": "SelectLimit",
            "T4": "Filter",
            "T5": "Tabel OR",
            "T6": "Queries",
            "T7": "Cosine"
        }

def test() -> dict:
    results = defaultdict(bool)
    binary_file = "./tests/main" 
    if not os.path.exists(binary_file):
        print("Compiled binary not found! Please run: make run_test and be sure there were no errors!")
        return results

    ref_files_path = "./tests/ref"
    output_files_path = "./tests/output"
    binary_file = "./tests/main"

    for _, _, files in os.walk(ref_files_path):
        for f in files:
            test_name = f.split(".")[0]
            ref_file = open(ref_files_path + "/" + f, "rb")
            ref_content = ref_file.read().decode("utf-8")
            ref_file.close()

            test_number = test_name.split("-")[0][-1]
            subtest_number = test_name.split("-")[1]
            cmd = binary_file + " " + str(test_number) + " " + str(subtest_number)
            out = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            out_content = out.stdout.read()
            out_file = open(output_files_path + "/" + test_name + ".out", "wb")
            out_file.write(out_content)
            out_file.close()
            results[test_name] = ref_content == out_content.decode("utf-8")

    return results

def summarize_results(results: dict, values: dict, test_meaning: dict) -> None:
    test_results = list(results.items())
    test_results.sort(key=lambda x: (int(x[0].split("-")[0][1:]), int(x[0].split("-")[1])))
    print("="*25)
    print("Haskell Homework Checker")
    print("="*25)
    test_sets = list({x.split("-")[0] for x, y in test_results})
    test_sets.sort()

    test_clusters = defaultdict(list)
    for test, rezult in test_results:
        test_clusters[test.split("-")[0]].append((test, rezult))


    final_score = 0
    for test_set in test_sets:
        total_score = values[test_set]
        obtained_score = 0
        print(f"{test_set} - {test_meaning[test_set]} \t {total_score}")
        subtests = test_clusters[test_set]
        score_subtest = total_score / len(subtests)
        for subtest, result in subtests:
            print(f"{subtest} \t\t {score_subtest if result else 0:.2f}")
            obtained_score += score_subtest if result else 0
        print(f"{test_set} \t\t Total: {obtained_score}")
        print("-"*25)
        final_score += obtained_score

    print(f"Final grade: {final_score}" + "\n")
    pass

if __name__ == "__main__":
    results = test()
    summarize_results(results, values, test_meaning)
