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
		"<variable_block>",
		"<variable_def>",
		"<assignment_stmt>",
		"<if_stmt>",
		"<for_stmt>",
		"<instantiation>",
	],
	
	# Variable declarations
	# TODO cleanup this declaration stuff
	# TODO tuples
	# TODO enums
	"<variable_decl>": ["<variable_declarator>;"],
	"<variable_declarator>": ["<type_name> <id><array_declarator>*"],
	"<type_name>": ["<variable_type_specifiers>? <variable_type><vector_declarator>?"], # TODO what about wire<4>[16]
	"<array_declarator>": ["<index_expression>"],
	"<vector_declarator>": ["<<expression>>"],
	"<variable_type>": [
    	"auto",
        "int",
        "wire",
        "bus",
        "node",
    ],
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
	"<variable_block>": ["<variable_type_specifiers> { <variable_block_stmt>* };"],
	"<variable_block_stmt>": [
    	"<variable_decl>",
    	"<variable_def>",
        "<variable_block>"
    ],
	
	# Variable definitions
	"<variable_def>": ["<variable_declarator> = <expression>;"],
	
	# Standalone assignment
	"<assignment_op>": ["=", "+=", "&=", "^=", "|="],
	"<assignment_stmt>": ["<expression> <assignment_op> <expression>;"],
	
	# For statement
	"<for_stmt>": ["for (<id> = <range_expression>) { <module_impl_stmt>* }"],

	# If statement
	"<if_stmt>": [
    	"if (<expression>) { <module_impl_stmt>* }",
    	"if (<expression>) { <module_impl_stmt>* } else { <module_impl_stmt>* }",
    ],

	# Module instantiation
	"<instantiation>": [
    	"<id> <id> { <instantiation_stmt_list> };",
    	"<id> <id> { <instantiation_stmt_list>, };",
    ],
	"<instantiation_stmt_list>": [
		"<instantiation_stmt>",
        "<instantiation_stmt_list>, <instantiation_stmt>",
	],
	"<instantiation_stmt>": [
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
        "true",
        "false",
	],
	"<expression>": [
		"<ternary_expression>"
	],
    
	"<range_expression>": [
		"<index_expression>",
        "[<expression> : <expression>]",
        "[<expression> :+ <expression>]",
	],
    
	"<index_expression>": [
		"[<expression>]",
	],
	
	# Match expressions
	"<match_expression>": [
    	"match(<expression>) {<match_expression_stmt_list>+}"
    	"match(<expression>) {<match_expression_stmt_list>+,}"
    ],
    "<match_expression_stmt_list>":[
		"<match_expression_stmt>",
		"<match_expression_stmt_list>, <match_expression_stmt>",
	],
	"<match_expression_stmt>": [
		"<match_expression_antecendent>: <expression>",
	],
    "<match_expression_antecendent>": [
		"<expression>", "default"
	],
    
	# Conditional expressions
	"<conditional_expression>": [
    	"conditional(<expression>) {<match_expression_stmt_list>+}"
    	"conditional(<expression>) {<match_expression_stmt_list>+,}"
    ],
	
	# Compound expressions
	"<compound_expression>": [
		"{<expression_list>}",
		"{<expression_list>,}",
	],
    
	# Comma separated expressions
	"<expression_list>": [
		"<expression>",
        "<expression_list>, <expression>",
	],

	# Precedence: 0 - postfix ops
	"<postfix_expression>": [
		"<primary_expression>",
		"<postfix_expression><range_expression>",
        "<postfix_expression>(<argument_list>?)",
        "<postfix_expression>.<id>",
	],
	"<argument_list>": [
		"<expression>",
        "<argument_list>, <expression>",
	],
	
	# Precedence: 1 - prefix ops
	"<unary_operator>": ["~", "!", "-", "+"],
	"<unary_expression>": [
		"<postfix_expression>",
		"<unary_operator><unary_expression>",
	],
	
	# Precedence: 2 - C-style casts
	"<cast_expression>": ["<unary_expression>", "(<type_name>) <cast_expression>"],
	
	# Precedence: 3 - multiplication/division
	"<multiplicative_expression>": [
		"<cast_expression>",
		"<multiplicative_expression> * <cast_expression>",
		"<multiplicative_expression> / <cast_expression>",
		"<multiplicative_expression> % <cast_expression>",
	],

	# Precedence: 4 - addition/subtraction
	"<additive_expression>": [
		"<multiplicative_expression>",
		"<additive_expression> + <multiplicative_expression>",
		"<additive_expression> - <multiplicative_expression>",
	],
	
	# Precedence: 5 - bit shifts
	"<shift_expression>": [
		"<additive_expression>",
		"<shift_expression> << <additive_expression>",
		"<shift_expression> >> <additive_expression>",
	],

	# Precedence: 6 - bitwise AND
	"<bitwise_and_expression>": [
		"<shift_expression>",
		"<bitwise_and_expression> & <shift_expression>",
	],

	# Precedence: 7 - bitwise XOR
	"<bitwise_xor_expression>": [
		"<bitwise_and_expression>",
		"<bitwise_xor_expression> & <bitwise_and_expression>",
	],

	# Precedence: 8 - bitwise OR
	"<bitwise_or_expression>": [
		"<bitwise_xor_expression>",
		"<bitwise_or_expression> ^ <bitwise_xor_expression>",
	],
	
	# Precedence: 9 - comparison operators
	"<relational_expression>": [
		"<bitwise_or_expression>",
		"<relational_expression> < <bitwise_or_expression>",
		"<relational_expression> > <bitwise_or_expression>",
		"<relational_expression> <= <bitwise_or_expression>",
		"<relational_expression> >= <bitwise_or_expression>",
	],
	
	# Precedence: 10 - equality operators
	"<equality_expression>": [
		"<relational_expression>",
		"<equality_expression> == <relational_expression>",
		"<equality_expression> != <relational_expression>",
	],
	
	# Precedence: 11 - logical AND
	"<and_expression>": [
		"<equality_expression>",
		"<and_expression> && <equality_expression>",
	],
	
	# Precedence: 12 - logical OR
	"<or_expression>": [
		"<and_expression>",
		"<or_expression> || <and_expression>",
	],
	
	# Precedence: 13 - ternary conditional operator
	"<ternary_expression>": [
		"<or_expression>",
		"<or_expression> ? <expression> : <ternary_expression>",
	],
    
	
}

