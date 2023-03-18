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

# Type system

## Basic types


\np `int` represents a signed 64-bit integer. int is a non-synthesizable type and can only be used in expressions evaluated at compile time.

\np `wire` represents a synthesizable 1-bit logical signal.

\np `bus<n>` represents a synthesizable $n$-bit data bus. $n$ must be a positive integer and can be an expression evaluated at compile time. $n$ is referred to as signal width.

\np `node<n>` represents a synthesizable $n$-bit aggregation node. $n$ must be a positive integer and can be an expression evaluated at compile time. $n$ is referred to as _signal width_.

\np `auto` is not a real type, but can be used in definitions in place of type name. The underyling type will then be deduced based on the right hand side of the expression.

\np Since `wire`, `bus` and `node` are types that represent actual hardware logic, they will be referred to as _synthesizable types_. There is no guarantee that expressions containing these types can be evaluated at compile time.

## Type qualifiers

Type qualifiers are keywords that can be added in declarations and definitions along with the base type name.

### Signedness

\np Signedness of a bus and node nets can be specified by using signed or unsigned qualifiers. If not specified, signals are assumed to be unsigned. Note that signedness of `wire` and `int` cannot be specified.

\np Unsigned $n$-bit signals can represent integers in range $[0; 2^n − 1]$.

\np Signed $n$-bit signals use two’s complement and can represent integers in range $[−2^{n−1}; 2^{n−1} − 1]$.

### Signal sensitivity

\np `int` is a compile-time-only type. It must be converted to a synthesizable type in order to be used in a synthesizable expression.

\np `const` is a qualifier applicable to synthesizable types indicating that the signal’s value doesn’t change over time.

\np `clock` is a qualifier indicating a clock signal. This qualifier can only be used with the `wire` base type.

\np `sync(E)` indicates that the signal is purely synchronous --- i.e. is an output of a synchronous register. `E` indicates a clocking expression associating the signal with certain clock signals.

\np `comb` indicates that the signal is a combinational signal.

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
There are two types of conversions — implicit conversions and explicit conversions. Implicit conversions are performed by the compiler without any explicit input from the programmer. Explicit conversions must be prompted by the programmer.

Signal type $A$ is said to be compatible with signal type $B$ if a implicit conversion chain can be performed from $A$ to $B$.

### Width conversions

\np There can be no implicit conversions altering signal width.


### Signal purity

\np Constant (const) signals can be implicitly converted to any synchronous (sync)
clocked by any expression.

### Signedness conversions

\np There can be no implicit conversions altering singal signedness.

### Trisatate conversions
\np Builtin functions `z_to_0()` and `z_to_1()` can be used to cast away the `tristate` qualifier. 

# Lexical elements

## Keywords

## Numeric constants
\np In expressions, numeric constants have type `const signed wire<N>` or `const unsigned wire<N>` where `N` is the number of bits defined within the constant itself.

## Identifiers

## Expressions

## Statements

## Signal declarations

## Module definitions

Module definition consists of two blocks — module interface definition and _module interface defintion_. Both of these are top-level syntax constructs.

### Module interface definition

\np Module interface is defined using the syntax shown below:
```
module <id> {
// module port-list declarations
}
```

\np Module interface does not need to have a corresponding implementation block in the same translation unit.

### Module implementation definition

\np Module implementation is defined using the syntax shown below:
```
impl <id> {
// module implementation statements
}
```

\np The implementation block identifier must correspond to a module interface block in the same translation unit.
