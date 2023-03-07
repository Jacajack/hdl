import string
from fuzzingbook.Grammars import opts

LETTERS = string.ascii_uppercase + string.ascii_lowercase
DIGITS = string.digits

KEYWORDS = [
	"module", "if", "for", "register", "input", "output", "wire", "sync", "clock",
	"conditional", "match", "comb", "tristate", "int", "signed", "unsigned", "auto"
	"unused", "const", "ff_sync", "clock_gate", "tristate_buffer", "enum"
]

GRAMMAR = {
	"<start>": ["<top_defs>"],
	
	"<id>": ["FOO", "BAR"],
	"<number>": ["123", "77"],
	
	"<top_defs>": ["<top_def>", "<top_defs><top_def>"], 
	"<top_def>": ["<module_def>", "<module_impl>"],

	"<module_def>": ["module <id> { <module_def_stmt>* }"],
	"<module_def_stmt>": ["<variable_decl>", "<variable_block>"],
	
	"<module_impl>": ["impl <id> { <module_impl_stmt>* }"],
	"<module_impl_stmt>": [
		"<variable_decl>",
		"<variable_block>", # TODO blocks with initializers should be allowed here
		"<variable_def>",
		"<assignment_stmt>",
		"<if_stmt>",
		"<for_stmt>",
		"<instantiation>",
	],
	
	# Variable declarations
	"<variable_decl>": ["<variable_declarator>;"],
	"<variable_declarator>": ["<type_name> <id><array_declarator>?"],
	"<type_name>": ["<variable_type_specifiers>? <variable_type><vector_declarator>?"],
	"<array_declarator>": ["[<expression>]"],
	"<vector_declarator>": ["<<expression>>"],
	"<variable_type>": ["auto", "int", "wire", "bus"],
	"<variable_type_specifier>": [
		"signed",
		"unsigned",
		"tristate",
		"const",
		"comb",
		"comb(<expression>)",
		"sync(<expression>)"
	],

	"<variable_type_specifiers>": ["<variable_type_specifier>", "<variable_type_specifiers> <variable_type_specifier>"],
	
	# Variable blocks
	# TODO maybe allow definitions in blocks here and exclude them on semantic level in port lists
	"<variable_block>": ["<variable_type_specifiers> { <variable_block_stmts> };"],
	"<variable_block_stmts>": ["<variable_block_stmt>", "<variable_block_stmts> <variable_block_stmt>"],
	"<variable_block_stmt>": ["<variable_decl>", "<variable_block>"],
	
	# Variable definitions
	"<variable_def>": ["<variable_declarator> = <expression>;"],
	
	# Standalone assignment
	"<assignment_stmt>": ["<id> = <expression>;"],
	
	# For statement
	"<for_stmt>": ["for (<expression>) { <module_impl_stmt>* }"], # TODO

	# If statement
	"<if_stmt>": ["if (<expression>) { <module_impl_stmt>* }"],

	# Module instantiation
	# TODO fix commas
	"<instantiation>": ["<id> <id> { <instantiation_stmts> };"],
	"<instantiation_stmts>": [
		"<id>: <expression>",
		"<id>: <variable_declarator>",
		"<id>: auto",
		"<id>",
	],

	# Expressions
	"<primary_expression>": [
		"<number>",
		"<id>",
		"(<expression>)",
		"<match_expression>",
		"<conditional_expression>",
		"<compound_expression>",
	],
	"<expression>": [
		"<ternary_expression>"
	],
	
	# Match expressions
	# TODO fix commas
	"<match_expression>": ["match(<expression>) {<match_expression_stmt>+}"],
	"<match_expression_stmt>": [
		"<expression>: <expression>",
		"default: <expression>",
	],
	
	# Conditional expressions
	# TODO fix commas
	"<conditional_expression>": ["conditional(<expression>) {<conditional_expression_stmt>+}"],
	"<conditional_expression_stmt>": [
		"<expression>: <expression>",
		"default: <expression>",
	],
	
	# Compound expressions
	"<compound_expression>": ["{<compound_expression_part>}"],
	"<compound_expression_part>": ["<expression>", "<compound_expression_part>, <expression>"],

	# Precedence: 0
	# TODO add function call (for future)
	"<postfix_expression>": [
		"<primary_expression>",
		"<primary_expression>[<expression>]"
	],
	
	# Precedence: 1
	"<unary_operator>": ["~", "!", "-", "+"],
	"<unary_expression>": [
		"<postfix_expression>",
		"<unary_operator><postfix_expression>",
	],
	
	# Precedence: 2
	# TODO do we really want this style of casts?
	# TODO maybe cast<T>()
	"<cast_expression>": ["<unary_expression>", "(<type_name>) <unary_expression>"],
	
	# Precedence: 3
	"<multiplicative_expression>": [
		"<cast_expression>",
		"<multiplicative_expression> * <cast_expression>",
		"<multiplicative_expression> / <cast_expression>",
		"<multiplicative_expression> % <cast_expression>",
	],

	# Precedence: 4
	"<additive_expression>": [
		"<multiplicative_expression>",
		"<additive_expression> + <multiplicative_expression>",
		"<additive_expression> - <multiplicative_expression>",
	],
	
	# Precedence: 5
	"<shift_expression>": [
		"<additive_expression>",
		"<shift_expression> << <additive_expression>",
		"<shift_expression> >> <additive_expression>",
	],
	
	# Precedence: 6
	"<relational_expression>": [
		"<shift_expression>",
		"<relational_expression> < <shift_expression>",
		"<relational_expression> > <shift_expression>",
		"<relational_expression> <= <shift_expression>",
		"<relational_expression> >= <shift_expression>",
	],
	
	# Precedence: 7
	"<equality_expression>": [
		"<relational_expression>",
		"<equality_expression> == <relational_expression>",
		"<equality_expression> != <relational_expression>",
	],
	
	# Precedence: 8
	# TODO increase precedence
	"<bitwise_and_expression>": [
		"<equality_expression>",
		"<bitwise_and_expression> & <equality_expression>",
	],
	
	# Precedence: 9
	# TODO increase precedence (fix C's mistake)
	"<bitwise_xor_expression>": [
		"<bitwise_and_expression>",
		"<bitwise_xor_expression> & <bitwise_and_expression>",
	],

	# Precedence: 10
	# TODO increase precedence (fix C's mistake)
	"<bitwise_or_expression>": [
		"<bitwise_xor_expression>",
		"<bitwise_or_expression> & <bitwise_xor_expression>",
	],
	
	# Precedence: 11
	"<and_expression>": [
		"<bitwise_or_expression>",
		"<and_expression> & <bitwise_or_expression>",
	],
	
	# Precedence: 12
	"<or_expression>": [
		"<and_expression>",
		"<or_expression> & <and_expression>",
	],
	
	# Precedence: 13
	# TODO - not sure about this one (Perl-like). ".." might be useful for ranges
	# TODO - maybe cat{<exprs>} is better?
	# TODO - maybe <expr> @ <expr> ?
	"<concat_expression>": [
		"<or_expression>",
		"<concat_expression> .. <or_expression>",
	],

	# Precedence: 14
	"<ternary_expression>": [
		"<concat_expression>",
		"<concat_expression> ? <expression> : <ternary_expression>",
	],
}

