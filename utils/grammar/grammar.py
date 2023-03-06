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
    
    "<id>": ["IDENT"],
    "<number>": ["77"],
    
    "<top_defs>": ["<top_def>", "<top_defs><top_def>"], 
    "<top_def>": ["<module_def>"],

	"<module_def>": ["module <id> { <module_def_stmts> }"],
	"<module_def_stmts>": ["<module_def_stmt>", "<module_def_stmts><module_def_stmt>"],
	"<module_def_stmt>": ["<variable_decl>", "<variable_block>"],
    
	# Variable declarations
	"<variable_decl>": ["<variable_type> <id>", "<variable_decl><array_declarator>"],
	"<array_declarator>": ["[<number>]"],
    "<variable_type>": ["auto", "int", "wire", "wire<<number>>"],
	"<variable_type_specifier>": ["signed", "unsigned", "tristate", "const", "comb", "sync(...)"], # TODO - comb clock domain, TODO clocking_expression
	"<variable_type_specifiers>": ["<variable_type_specifier>", "<variable_type_specifiers> <variable_type_specifier>"],
	
	# Variable blocks
	"<variable_block>": ["<variable_type_specifiers> { <variable_block_stmts> }"],
	"<variable_block_stmts>": ["<variable_block_stmt>", "<variable_block_stmts> <variable_block_stmt>"],
    "<variable_block_stmt>": ["<variable_decl>", "<variable_block>"],
}
