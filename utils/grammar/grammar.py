import string
from fuzzingbook.Grammars import opts

LETTERS = string.ascii_uppercase + string.ascii_lowercase
DIGITS = string.digits

GRAMMAR = {
	"<start>": ["<top_defs>"],
	
	"<id>": ["FOO", "BAR"],
	"<number>": ["123", "77"],
    
	"<metadata_comment>": [
        "<metadata_comment>+",
    	"/// METADATA\n",
    ],
	
	"<top_defs>": ["<top_def>", "<top_defs><top_def>"], 
	"<top_def>": ["<module_def>", "<module_impl>"],

	"<module_def>": ["<metadata_comment>? module <id> { <module_def_stmt>* }"],
	"<module_def_stmt>": ["<variable_decl_stmt>", "<variable_block>"],
	
	"<module_impl>": ["<metadata_comment>? impl <id> { <module_impl_stmt>* }"],
	"<module_impl_stmt>": [
		"<variable_decl_stmt>",
		"<variable_block>",
		"<variable_def_stmt>",
		"<assignment_stmt>",
		"<if_stmt>",
		"<for_stmt>",
		"<instantiation_stmt>",
	],

	# TODO enum definition

	# Variable declarations
	"<variable_decl_stmt>": ["<variable_decl>;"],
	"<variable_decl>": ["<metadata_comment>? <type_declarator> <id><array_declarator>*"],
	"<type_name>": ["<type_declarator><array_declarator>*"],
	"<type_declarator>": ["<variable_type_specifiers>? <variable_type><vector_declarator>?"],
	"<array_declarator>": ["<index_expression>"],
	"<vector_declarator>": ["<<expression>>"],
	"<variable_type>": [
    	"auto",
        "int",
        "wire",
        "bus",
    ],
	"<variable_type_specifier>": [
		"signed",
		"unsigned",
		"tristate",
		"const",
		"comb(<expression>)",
		"sync(<expression>)",
        "input",
        "output",
        "async",
	],
	"<variable_type_specifiers>": [
		"<variable_type_specifier>",
        "<variable_type_specifier> <variable_type_specifiers>",
	],
	
	# Variable blocks
	"<variable_block>": ["<metadata_comment>? <variable_type_specifiers> { <variable_block_stmt>* };"],
	"<variable_block_stmt>": [
    	"<variable_decl_stmt>",
    	"<variable_def_stmt>",
        "<variable_block>",
    ],
	
	# Variable definitions
	"<variable_def>": ["<variable_decl> = <expression>"],
    "<variable_def_stmt>": ["<variable_def>;"],
	
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

	# Module instantiation statement
	"<instantiation_stmt>": [
    	"<metadata_comment>? <id> <id> { <port_bind_stmt_list> };",
    	"<metadata_comment>? <id> <id> { <port_bind_stmt_list>, };",
    ],
	"<port_bind_stmt_list>": [
		"<port_bind_stmt>",
        "<port_bind_stmt_list>, <port_bind_stmt>",
	],
	"<port_bind_stmt>": [
		"<id>: <expression>",
		"<id>: <variable_decl>",
		"<id>",
	],

	# Expressions
	"<primary_expression>": [
		"<number>",
		"<id>",
		"(<expression>)",
		"<match_expression>",
		"<conditional_expression>",
		"<tuple>",
        "true",
        "false",
	],
	"<expression>": [
		"<primary_expression>",
		"<ternary_expression>",
        "<tuple>",
        "<postfix_expression>",
        "<range_expression>",
        "<unary_expression>",
        "<postfix_expression>",
        "<multiplicative_expression>",
		"<additive_expression>",
		"<shift_expression>",
		"<bitwise_and_expression>",
		"<bitwise_xor_expression>",
		"<bitwise_or_expression>",
		"<relational_expression>",
		"<equality_expression>",
		"<and_expression>",
		"<or_expression>",
		"<ternary_expression>",
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
    	"match(<expression>) {<match_expression_stmt_list>}",
    	"match(<expression>) {<match_expression_stmt_list>,}",
    ],
    "<match_expression_stmt_list>": [
		"<match_expression_stmt>",
		"<match_expression_stmt>, <match_expression_stmt_list>",
	],
	"<match_expression_stmt>": [
		"<match_expression_antecendent> => <expression>",
	],
    "<match_expression_antecendent>": [
		"<expression>", "default"
	],
    
	# Conditional expressions
	"<conditional_expression>": [
    	"conditional(<expression>) {<match_expression_stmt_list>}",
    	"conditional(<expression>) {<match_expression_stmt_list>,}",
    ],
	
	# Tuples
	"<tuple>": [
		"{<expression_list>}",
		"{<expression_list>,}",
	],
	"<expression_list>": [
		"<expression>",
        "<expression_list>, <expression>",
	],

	# Precedence: 0 - postfix ops
	"<postfix_expression>": [
		"<expression><range_expression>",
        "<expression>(<argument_list>?)",
        "<expression>.<id>",
	],
	"<argument_list>": [
        "<argument_list>, <expression>",
	],
	
	# Precedence: 1 - prefix ops + C-style cast
	"<unary_operator>": ["~", "!", "-", "+"],
	"<unary_expression>": [
		"<unary_operator><expression>",
        "(<type_name>) <expression>",
	],
	
	# Precedence: 2 - multiplication/division
	"<multiplicative_expression>": [
		"<expression> * <expression>",
		"<expression> / <expression>",
		"<expression> % <expression>",
	],

	# Precedence: 3 - addition/subtraction
	"<additive_expression>": [
		"<expression> + <expression>",
		"<expression> - <expression>",
	],
	
	# Precedence: 4 - bit shifts
	"<shift_expression>": [
		"<expression> << <expression>",
		"<expression> >> <expression>",
	],

	# Precedence: 5 - bitwise AND
	"<bitwise_and_expression>": [
		"<expression> & <expression>",
	],

	# Precedence: 6 - bitwise XOR
	"<bitwise_xor_expression>": [
		"<expression> ^ <expression>",
	],

	# Precedence: 7 - bitwise OR
	"<bitwise_or_expression>": [
		"<expression> | <expression>",
	],
	
	# Precedence: 8 - comparison operators
	"<relational_expression>": [
		"<expression> < <expression>",
		"<expression> > <expression>",
		"<expression> <= <expression>",
		"<expression> >= <expression>",
	],
	
	# Precedence: 9 - equality operators
	"<equality_expression>": [
		"<expression> == <expression>",
		"<expression> != <expression>",
	],
	
	# Precedence: 10 - logical AND
	"<and_expression>": [
		"<expression> && <expression>",
	],
	
	# Precedence: 11 - logical OR
	"<or_expression>": [
		"<expression> || <expression>",
	],
	
	# Precedence: 12 - ternary conditional operator
	"<ternary_expression>": [
		"<expression> ? <expression> : <expression>",
	],	
}

