import subprocess
import platform
import sys
import random
import argparse
import os 
import shutil
from fuzzingbook.Grammars import is_valid_grammar, convert_ebnf_grammar
from fuzzingbook.GeneratorGrammarFuzzer import ProbabilisticGeneratorGrammarCoverageFuzzer

from grammar.grammar import GRAMMAR
from config import *


################ Constants ################

TMP_TEST_PATH = "utils/tmp/tmp-test.hirl"
TMP_LOG_PATH = "utils/tmp/tmp.log"

EXTENSION = ".exe" if platform.system() == "Windows" else ""
LOG_TO_FILE = False
## Grammar and Fuzzer init
assert is_valid_grammar(GRAMMAR)
GRAMMAR = convert_ebnf_grammar(GRAMMAR)
###########################################


def run_test(test_path = TMP_TEST_PATH):
	path = BINARY_PATH + EXTENSION + " -m pretty-print "+ test_path
	#print(path)
	c4 = subprocess.Popen(path,	stdout=subprocess.PIPE,	stderr=subprocess.STDOUT)
	output = ""
	try:
		output = c4.communicate(timeout=5)[0]
		output = output.decode(errors="replace")
		failed = c4.returncode != 0
		timeout = False
	except subprocess.TimeoutExpired as timeErr:
		output = timeErr.stdout
		c4.kill()
		timeout = failed = True

	return output, failed, timeout


# Log generated input, parser output and fuzzer log on unexpected test outcome
def log_unexpected(index:str):
		
	unexp_or_timeout = ""
	if timeout:
		unexp_or_timeout = "Timeout"
	else:
		unexp_or_timeout = "Unexpected"
	
	# copy to failed-test dir with name <id>-unexpected.c or <id>-timeout.c
	with open(TMP_TEST_PATH, "r") as fd_test:
		f_name = index
		f_name += f"-{unexp_or_timeout.lower()}.hirl"
		
		test_content = unexp_or_timeout + " on following input:\n"
		test_content += fd_test.read()

		test_content += "\n\nWith following parser output:\n"
		test_content += output

	if LOG_TO_FILE:
		if not os.path.exists("utils/fuzzer/fuzzer-failed-tests"):
			os.makedirs("utils/fuzzer/fuzzer-failed-tests")
		with open(f"utils/fuzzer/fuzzer-failed-tests/{f_name}", "w") as f:
			f.write(test_content)
	else:
		sys.stderr.write(f"Unexpected fail on test {f_name}:\n{test_content}\n")

def log_expected(index:str):
		
	with open(TMP_TEST_PATH, "r") as fd_test: 
		f_name = index
		f_name += "-succesful.hirl"
			
		test_content = "Success on following input:\n"
		test_content += fd_test.read()

		test_content += "\n\nWith following parser output:\n"
		test_content += output

	if LOG_TO_FILE:
		if not os.path.exists("utils/fuzzer/fuzzer-succeded-tests"):
			os.makedirs("utils/fuzzer/fuzzer-succeded-tests")
		with open(f"utils/fuzzer/fuzzer-succeded-tests/{f_name}", "w") as f:
			f.write(test_content)
	else:
		sys.stdout.write(f"Succesful test {f_name}:\n{test_content}")


if __name__ == "__main__":
	parser = argparse.ArgumentParser(
                    prog='ProgramName',
                    description='What the program does',
                    epilog='Text at the bottom of help')
	parser.add_argument('--seed', type=int, default=0, help='Seed for random generator',required=False)
	parser.add_argument('--random-seed', action='store_true', help='Use random seed',required=False)
	parser.add_argument('--log-to-file',action='store_true', help='Log to file',required=False)	
 
	args = parser.parse_args(sys.argv[1:])
	if not (args.random_seed or args.seed):
		print("Specify either --random-seed or --seed <seed>")
		sys.exit(1)
	if args.seed:
		random.seed(args.seed)
		print(f"Starting fuzzing with seed = {args.seed}")
	else:
		random.seed()
		print(f"Starting fuzzing with random seed = {random.getstate()[1][0]}")
	LOG_TO_FILE = args.log_to_file
	if not os.path.exists("utils/tmp"):
		os.makedirs("utils/tmp")
	fuzzer = ProbabilisticGeneratorGrammarCoverageFuzzer(
									GRAMMAR, 
									min_nonterminals = MIN_NON_TERMINALS,
									max_nonterminals = MAX_NON_TERMINALS,
									log = False
									)
	
	expected_total = 0
	unexpected_total = 0
	timeout_total = 0

	i = 0
	initilal_coverage = len(fuzzer.missing_expansion_coverage())
	while len(fuzzer.missing_expansion_coverage()) >  0.05 * initilal_coverage:
		i += 1

		if unexpected_total >= MAX_FAILED_TESTS:
			break

		if VERBOSE:
			print("Run:", i)
			if i % 10 == 0:
				print("Missing coverages:", fuzzer.missing_expansion_coverage())
		elif i % 100 == 0:
			print("Run:", i)
			print("Amount of missing coverages:", len(fuzzer.missing_expansion_coverage()))

		
		# Close file before running
		with open(TMP_TEST_PATH, "w") as fd_test: 
		# Redirect stdout to file for log
			with open(TMP_LOG_PATH, "w") as sys.stdout:
				fd_test.write(fuzzer.fuzz())
		
		# Restore stdout
		sys.stdout = sys.__stdout__

		output, failed, timeout = run_test()
		if not (failed or timeout):
			expected_total += 1
			log_expected(str(i))
		else:
			unexpected_total += 1
			timeout_total += int(timeout)
			log_unexpected(str(i))
			print(f"Unexpected test: expected {expected_total}, unexpected {unexpected_total}, timeout {timeout_total}")

	print(f"Done: expected {expected_total}, unexpected {unexpected_total}, timeout {timeout_total}")
	print(f"Amount of missing coverages: {len(fuzzer.missing_expansion_coverage())} / {initilal_coverage}")
	print(f"Missing coverages: {fuzzer.missing_expansion_coverage()}")
	shutil.rmtree("utils/tmp")
