#!/bin/env python3
from grammar import GRAMMAR

for nonterminal, rules in GRAMMAR.items():
	if len(rules) == 1:
		print(f"{nonterminal} ::= {rules[0]}")
	else:
		rules_str = "\n\t| ".join(rules)
		print(f"{nonterminal} ::=\n\t{rules_str}")