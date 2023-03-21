---
title: "HDL language definition"
# subtitle: "Projekt programistyczny"
author: [Jacek Wieczorek, Wojciech Ptaś]
geometry: a4paper, margin=2.5cm
date: 15.03.2023r.
colorlinks: true
codeBlockCaptions: true
indent: false
numbersections: true
--- 

# Introduction

This document describes syntax and semantics of the proposed hardware description language. 

Requirement key words such as ,,must'' or ,,should'' in this document should be interpreted with accordance to RFC 2119.

# Type system

## Basic types


\np `int` represents a signed 64-bit integer. int is a non-synthesizable type and can only be used in expressions evaluated at compile time.

\np `wire` represents a synthesizable 1-bit logical signal.

\np `bus<n>` represents a synthesizable $n$-bit data bus. $n$ must be a positive integer and can be an expression evaluated at compile time. $n$ is referred to as _signal width_.



\np `auto` is not a real type, but can be used in definitions in place of type name. The underyling type will then be deduced based on the right hand side of the expression.

\np Since `wire`, `bus`  are types that represent actual hardware logic, they will be referred to as _synthesizable types_. There is no guarantee that expressions containing these types can be evaluated at compile time.

## Type qualifiers

Type qualifiers are keywords that can be added in declarations and definitions along with the base type name.



### Signedness

\np Signedness of a `bus` signals can be specified by using `signed` or `unsigned` qualifiers. Note that signedness of `wire` and `int` cannot be specified.

\np Unsigned $n$-bit signals can represent integers in range $[0; 2^n − 1]$.

\np Signed $n$-bit signals use two’s complement and can represent integers in range $[−2^{n−1}; 2^{n−1} − 1]$.

\np If signedness is not specified in a declaration, `unsigned` is preferred over `signed` by default.

### Sensitivity lists

\np _Sensitivity lists_ express sensitivity of a signal to different clock signals.

\np A _sensitivity list_ is a comma-separated list of `clock` signal identifiers. Occurence of an identifier indicates that the signal is subject to change on rising edges of certain clock signal.

\np Each identifier in the list can be optionally followed by the _logical NOT operator_ ,,`!`'', indicating that the signal is sensitive to falling edges of certain clock signal.

\np 

### Signal sensitivity

\np `int` is a compile-time-only type. It must be converted to a synthesizable type in order to be used in a synthesizable expression.

\np `const` is a qualifier applicable to synthesizable types indicating that the signal’s value doesn’t change over time.

\np `clock` is a qualifier indicating a clock signal. This qualifier can only be used with the `wire` base type.

\np `sync(E)` indicates that the signal is purely synchronous --- i.e. is an output of a synchronous register. `E` indicates a _sensitivity list_ associating the signal with certain clock signals.

\np `comb(E)` indicates that the signal is an output of combinational logic driven by signals sensitive to clocks indicated in the _sensitivity list_ `E`.

<!-- TODO maybe this should be called async? -->
\np `comb` indicates that the signal is a combinational signal not associated with any clock domain.


### Signal direction

\np Signals declared in a module port definitions are referred to as _module interface_.

\np `input` and `output` are additional qualifiers which can only be used in _module port definitions_. They are applicable to all synthesizable types.

\np `tristate` is a qualifier used to indicate that the signal can assume additional high-impedance (Z) state. It is applicable to all synthesizable types. It can be used in a module port defition instead of input or output. Unlike `input` and `output`, the `tristate` qualifier can be used in a declaration outside the module interface.

\np `tristate`, `output` and `input` qualifiers are all mutually exclusive.

\np Output signals must be driven by the module implementation. Value of an output signal can be read by the module implementation.

\np Output signals must not be driven by any external logic when a module is instantiated.

\np Input signals must be driven by some external logic when a module is instantiated.

\np Input signals must not be driven by the module implementation.

\np Tristate signals must not be driven by any non-tristate signals.

\np Tristate signals’ values cannot be evaluated to be used in any expression unlessthe tristate qualifier is removed by one of the tristate casts.

## Conversions
There are two types of conversions — implicit conversions and explicit conversions. Implicit conversions are performed by the compiler without any explicit input from the programmer. Explicit conversions must be prompted by the programmer by using the _cast expression_.

### Implicit conversions

\np Signal type $A$ is said to be _implicitly convertible to_ with signal type $B$ if a implicit conversion chain can be performed from $A$ to $B$.

\np Implicit conversions **must not** affect signal width.

\np Implicit conversions **must not** affect signal signedness.

\np Mutual implicit conversions between `unsigned bus<1>` and `wire` are allowed.

\np Implicit conversions can affect signal sensitivity, but only if the resulting signal type is _more sensitive_. 

\np Implicit conversions of signal sensitivity are allowed if the 

### Explicit conversions

\np The _cast expression_ can be used to forcibly coerce signal types. 

\np Builtin functions `pulldown()` and `pullup()` can be used to cast away the `tristate` qualifier. 


### Signal purity




## Type deduction

\np _Fully constrained_ signal must be fully qualified in terms of: base type (including width), signedness and sensitivity to clock events.

\np If any of the above is not explicitly qualified, the signal is _partially constraied_.



\np Type deduction takes place in assignments (standalone or as a part of definitions) and in function calls.

\np In assignment statements, type of the l-value in the assignment is referred to as _expected type_. Type of the r-value in the assignment is known as _assigned type_.

\np In function calls, type of the parameter accepted by the function is the _expected type_. Type of the argument passed at the call site is known as _assigned type_.

\np Type deduction takes place whenever _expected type_ or _assigned type_ are not fully constrained. The compiler must then find a suitable _deduced type_ `T` such that _assigned type_ can be implcitly converted into `T` and that `T` is not in conflict with the _expected type_ in terms of the constraints.



### Type deduction algorithm

```
auto x = y ? 4 : 5u7 // error - different types in branches
auto x = y ? 4 : 5 // error - cannot init int with non-compile-time value
auto x = y ? 4' : 5' // error - cannot deduce width
bus<4> x = 4' // ok - deduced const unsigned bus<4>
signed bus<4> x = 5' // ok - deduced const signed bus<4>
auto x = 45s11; // ok - deduced const signed bus<11>
comb(clk) signed wire = 1u1; // unsigned wire<1> should be implicitly convertible to wire

```


\np `auto` is considered to be fully unconstrained type.

\np The _assigned type_ must not be `auto`.

\np If the _expected type_ is `auto` and the _assigned type_ is fully constrained, _deduced type_ is equal to _assigned type_. 

\np If the _expected type_ is `auto` and the _assigned type_ is not fully constrained, _deduced type_ is equal to _assigned type_ with additional default constraints (`unsigned`, `comb`).



### Expected type propagation

\np In assignment statements (standalone, in definitions and module instantiation) the _expected type_ propagates from the left side of the assignment to the right.

\np In ternary operator conditional expressions such as `A ? B : C`, the _expected type_ is propagated to the expressions serving as branches of the conditional expreesion (here `B` and `C`).

\np In `conditional` block operator expressions, the _expected type_ is propagated to the right side of each branch.

\np In `match` block operator expressions, the _expected type_ is propagated to the right side of each branch.

\np In builtin function calls, propagation of the _expected type_ is implementation defined.

\np _Expected type_ does not propagate through expressions other than mentioned in this section. Instead, `auto` is assumed to be the expected type.

# Lexical elements

## Comments and whitespace

\np Comments and redundant whitespace are ignored by the lexer

\np _Single line comment_ starts with `//` and lasts until the end of the line 

\np _Block comment_ starts with `/*` and lasts until the first occurence of `*/`. 

\np Unterminated block comment must cause a compiler error.

## Keywords

## Numeric constants

\np Every token starting with a digit is treated as a numeric constant.

\np Numeric constants starting with `0b` are treated as binary constants. Numeric constants starting with `0x` are treated as hexadecimal constants. Otherwise, decimal base is assumed.

\np Constant value can be followed by either `u`, `s` or `'`. `s` indicates a signed constant, `u` indicates an unsigned constant, `'` leaves the signedness unconstrained. Each of these indicate that the token is a _fixed width_ numeric constant.

\np The signedness specifier can be followed by another number (base 10) which specifies the width of the constant. If not used, the width is assumed to be unconstrained.

\np If a numeric constant is not fully constrained, its type can be deduced from the context using _type deduction algorithm_. Naturally, if type deduction algorihtm fails, an error must be reported by the compiler.

<!--
\np In expressions, numeric constants have type `const signed wire<N>` or `const unsigned wire<N>` where `N` is the number of bits defined within the constant itself.
-->

## Logical constants

\np Logical constant `true` represents a constant logical truth value. Its type in expressions is `const wire`.

\np Logical constant `false` represents a constant logical false value. Its type in expressions is `const wire`.

## Identifiers

## Expressions

## Statements

## Signal declarations

## Module definitions

Module definition consists of two blocks — module interface definition and _module interface defintion_. Both of these are top-level syntax constructs.

### Module interface definition

\np Module interface is defined using the syntax shown below:

~~~{.v}
module <id> {
	// module port-list declarations
}
~~~

\np Module interface does not need to have a corresponding implementation block in the same translation unit.

### Module implementation definition

\np Module implementation is defined using the syntax shown below:
```
impl <id> {
	// module implementation statements
}
```

\np The implementation block identifier must correspond to a module interface block within the same translation unit.

# Builtin functions

`pullup()`, `pulldown()`, `fold_sum()`, `fold_or()`, `fold_xor()`, `fold_and()`, `zext()`, `sext()`, `ext()`

# Builtin modules