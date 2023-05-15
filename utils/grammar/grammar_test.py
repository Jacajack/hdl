
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

while len(fuzzer.missing_expansion_coverage()) != 0:
	print(fuzzer.fuzz())