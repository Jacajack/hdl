# HDL

An _actual_ hardware description language.

# Syntax

## Keywords (so far)

```
module, if, for, register, input, output, wire, sync, clock,
conditional, match, comb, tristate, int, signed, unsigned, auto
unused, const, ff_sync, clock_gate, tristate_buffer, enum
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

# Module definition
<module_def> ::= "module" <id> "{" <module_def_stmts> "}"
<module_def_stmts> ::= <module_def-stmt> | <module_def_stmts> <module_def_stmt>
<module_def_stmt> ::= TODO

# Module implementation
<module_impl> ::= "impl" <id> "{" <module_impl_stmts> "}"
<module_impl_stmts> ::= <module_impl_stmt> | <module_impl_stmts> <module_impl_stmt>
<module_impl_stmt> ::= TODO

```