# HIRL Hardware Description Language

[![Main CI](https://github.com/Jacajack/hdl/actions/workflows/ci.yml/badge.svg)](https://github.com/Jacajack/hdl/actions/workflows/ci.yml)  [![codecov](https://codecov.io/gh/Jacajack/hdl/graph/badge.svg?token=NCOY8ZS9BQ)](https://codecov.io/gh/Jacajack/hdl)

HIRL (working name: _Hardware Intermediate Representation Language_) is a proof-of-concept, declarative hardware description language optimized for RTL coding, aiming to provide modern Rust-like syntax and more restrictive semantics protecting the user from typical errors.

Notable features:
 - **Syntax inspired by Rust and C++**
 - **Sensitivity semantics in the type system provide basic CDC validation**
 - **Restrictive signal width and signedness semantics**
 - **Bi-directional type deduction** - not all types need to be explicitly specified in assignments
 - **No implicit conversions** - potentially harmful implicit conversions are not allowed
 - **Generic constructs** compiled to equivalent generic System Verilog code
 - **Builtin elab phase** ensuring proper use of signals (i.e. no double drivers, no dangling inputs etc.)
 - Interoperability with System Verilog

## Examples

This section presents some basic code examples. More code samples can be found in the [tests](/tests/) directory.

### Combinational full adder

```sv
// Interface definition
module full_adder {
	// All inputs and outputs are asynchronous
	input async a;
	input async b;
	input async cin;
	output async cout;
	output async q;
}

// Module logic
impl full_adder {
	cout = (a & b) | (cin & (a ^ b));
	q = a ^ b ^ cin;
}
```

### Simple counter

```sv
module simple_counter{
	// Clock input
	input clock clk;

	// Asynchronous reset signal (active low)
	input async wire nreset;

	// Combinational enable signal (clock domain is clk)
	input comb(clk) wire enable;

	// Synchronous 16-bit output bus
	output unsigned sync(clk) bus<16> data;
}

impl simple_counter {
	// data is 16 bits wide, 1u1 is 1 bit wide.
	// Addition result is 17-bit and therefore needs 
	// to be explicitly truncated with trunc() builtin function
	bus<16> counter_next = trunc(data + 1u1);

	// Register storing counter's value
	reg counter{
		clk,    // Auto-connected to `clk`
		nreset, // Auto-connected to `nreset`
		en: enable, // Enable the register with `enable` signal
		next: counter_next, // Next value for the register
		data, // Output is auto-connected to `data`
	};
}
```

## Building

Use Cargo for building. The compiler has builtin command line help. For practical example of use please also see [this Makefile](/tests/input_sim/Makefile).

## Coverage graphs

<table>
<tr><td>
<img src="https://codecov.io/gh/Jacajack/hdl/graphs/sunburst.svg?token=NCOY8ZS9BQ"/>
</td><td>
<img src="https://codecov.io/gh/Jacajack/hdl/graphs/tree.svg?token=NCOY8ZS9BQ"/>
</td><td>
<img src="https://codecov.io/gh/Jacajack/hdl/graphs/icicle.svg?token=NCOY8ZS9BQ"/>
</td></tr>
</table>
