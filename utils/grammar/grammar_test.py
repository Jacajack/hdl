#!/bin/env python3
from fuzzingbook.Grammars import is_valid_grammar, convert_ebnf_grammar
from fuzzingbook.GeneratorGrammarFuzzer import ProbabilisticGeneratorGrammarCoverageFuzzer
from grammar import GRAMMAR

assert is_valid_grammar(GRAMMAR)
grammar = convert_ebnf_grammar(GRAMMAR)

MIN_NON_TERMINALS = 0
MAX_NON_TERMINALS = 15

fuzzer = ProbabilisticGeneratorGrammarCoverageFuzzer(
	grammar,
	min_nonterminals = MIN_NON_TERMINALS,
	max_nonterminals = MAX_NON_TERMINALS,
	log = False
)
i = 0 
initilal_coverage = len(fuzzer.missing_expansion_coverage())
while len(fuzzer.missing_expansion_coverage()) >  0.05 * initilal_coverage:
	print(f"{i=}, result: {fuzzer.fuzz()}")
	i+=1
 
print(f"Final coverage: {len(fuzzer.missing_expansion_coverage())} / {initilal_coverage}")
