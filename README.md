# HDL

An _actual_ hardware description language.

# Syntax

## Keywords (so far)

```
module, if, for, register, input, output, wire, sync, clock,
conditional, match, comb, tristate, int, signed, unsigned, auto
unused, const, ff_sync, clock_gate, tristate_buffer, enum, bus
```
## Type system
TODO

### Compile-time 

`int` is the only non-synthesizable type which can be used for generic code and expressions evaluated at compile time. `int` is represented by a signed 64-bit integer value.

### Synthesizable types

`wire` is the basic synthesizable type. `wire<N>` represents a `N`-bit bus (unsigned by default).
 
Type specifiers are:
 - `unsigned` - (default)
 - `signed`
 - `const`
 - `comb`
 - `sync(<clocking_expression>)`
 - `tristate`

### Arrays
TODO

### Type conversions
TODO

#### Synchronous and asynchronous signals
TODO

#### Tristate signals 
TODO

#### Clock domain crossing
TODO


## Language grammar

```perl
S ::= <top_defs>
<top_defs> ::= <top_def> | <top_defs> <top_def>
<top_def> ::= <module_def> | <module_impl>

# Identifiers
<id_letter> ::= [a-zA-Z] | [0-9] | "_"
<id> ::= <id_letter> | <id> <id_letter>

# Numeric constants
<number> ::= TODO
<constant> ::= TODO

# Module definition
<module_def> ::= "module" <id> "{" <module_def_stmts> "}"
<module_def_stmts> ::= <module_def-stmt> | <module_def_stmts> <module_def_stmt>
<module_def_stmt> ::= <variable_decl> | <variable_block>

# Module implementation
<module_impl> ::= "impl" <id> "{" <module_impl_stmts> "}"
<module_impl_stmts> ::= <module_impl_stmt> | <module_impl_stmts> <module_impl_stmt>
<module_impl_stmt> ::= <variable_decl> | <variable_def>

# Variable declarations
<variable_decl> ::= <variable_type> <id> | <variable_decl> <array_declarator>
<array_declarator> ::= "[" <number> "]"
<variable_type> ::= "auto" | "int" | "wire" | "wire" "<" <number> ">" # TODO - should these be LT and GT?
<variable_type_specifier> ::= "signed" | "unsigned" | "tristate" | "const" | "comb" | "sync" "(" <clocking_expression> ")" # TODO - comb clock domain
<variable_type_specifiers> ::= <variable_type_specifier> | <variable_type_specifiers> <variable_type_specifier>

# Variable blocks
<variable_block> ::= <variable_type_specifiers> "{" <variable_block_stmts> "}"
<variable_block_stmts> ::= <variable_block_stmt> | <variable_block_stmts> <variable_block_stmt>
<variable_block_stmt> ::= <variable_decl> | <variable_block>

# Variable definitions (declaration + assignment)
<variable_def> ::= <variable_decl> "=" <expression>


# Clocking expressions
<clocking_expression> ::= # TODO

# TODO
# Expressions
<primary_expression> ::= "auto" | <id> | <constant> | "(" <expression> ")"
<expression> ::= 
	<and_expression>
	| <or_expression>
	| <xor_expression>
	| <multiplicative_expression>
	| <additive_expression>
	| <shift_expression>
	| <concat_expression>
	| <ternary_expression>
	| <conditional_expression>
	| <match_expression>
	| <cast_expression>
	| <relational_expression>
	| <compound_expression>
	| <unary_expression>

```