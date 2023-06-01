pub mod expression_ops;

pub struct HirnDesign {
	pub root_namespace: Namespace,
}

pub struct Namespace {
	pub name: String,
	pub namespaces: Vec<Namespace>,
	pub modules: Vec<Module>,
}

pub struct NumericConstant {
	pub width: u32,
	pub class: SignalClass,
	pub value: Vec<u8>,
}

pub struct Module {
	pub name: String,
	pub interface: Scope,
	pub main_scope: Scope,
	pub parameters: Vec<GenericParam>,
}

impl Module {
	pub fn new(name: String) -> Self {
		Self {
			name,
			interface: Scope::new(),
			main_scope: Scope::new(), // TODO link scopes
			parameters: Vec::new(),
		}
	}

	pub fn add_generic_parameter(&mut self, name: &String) {
		todo!();
	}

	pub fn add_interface_signal(&mut self, name: &String, signal_type: SignalType) {
		todo!();
	}

	pub fn get_scope(&mut self) -> &mut Scope {
		todo!();
	}
}


pub enum ArraySlice {
	Range(usize, usize),
	Index(usize),
}

pub struct SignalRef {
	pub signal_id: usize, // TODO ???
	pub slices: Vec<ArraySlice>,
	pub bit_range: (usize, usize),
}

pub struct GenerateIf {

}

pub struct GenerateFor {
	pub scope: Scope,
	pub iterator_var: GenericRef,
	pub iterator_begin: Expression,
	pub iterator_end: Expression,
}

pub struct Assignment {
	pub lhs: SignalRef,
	pub rhs: Expression,
}

pub struct ModuleInstance {
	pub name: String,
	// TODO bindings
}

pub struct Register {
	pub width: u32,
	pub class: SignalClass,
	pub input_nreset: Option<SignalRef>,
	pub input_clk: SignalRef,
	pub input_next: SignalRef,
	pub output_data: SignalRef,

}

pub struct Scope {
	// signals
	// genvars
	// assignments
	pub assignments: Vec<Assignment>,

}

impl Scope {
	fn new() -> Self {
		Self {
			assignments: vec![],
		}
	}

	fn add_signal() {

	}

	fn add_assignment() {

	}

	fn add_conditional_block() {

	}

	fn add_loop_block() {

	}
}

// TODO do we want generic array parameters
pub struct GenericRef {

}


pub struct GenericParam {
	pub name: String,
}

pub struct SignalType {
	pub width: u32,
	pub class: SignalClass,
	pub senitivity: SignalSensitivity,
}

pub enum SignalClass {
	Logic,
	Signed,
	Unsigned,
}

pub struct EdgeSensitivity {
	pub clock_signal: SignalRef,
	pub on_rising: bool,
}

pub struct SensitivityList {
	pub edges: Vec<EdgeSensitivity>,
}

pub enum SignalSensitivity {
	Asynchronous,
	Combinational(SensitivityList),
	Sequential(SensitivityList),
	Clock,
	Const,
}

// TODO check if we have all 
pub enum BinaryOp {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	ShiftLeft,
	ShiftRight,
	LogicalAnd,
	LogicalOr,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	Join,
	Equal,
	NotEqual,
	Less,
	LessEqual,
	Greater,
	GreaterEqual,
}

// TODO check if we have all
pub enum UnaryOp {
	Negate,
	LogicalNot,
	BitwiseNot,
	ZeroExtend{width: u32},
	SignExtend{width: u32},
	BitSelect(u32, u32),
	ReductionAnd,
	ReductionOr,
	ReductionXor,
}

pub struct ConditionalExpressionBranch {
	pub condition: Expression,
	pub value: Expression,
}

pub struct ConditionalExpression {
	pub branches: Vec<ConditionalExpressionBranch>,
	pub default: Box<Expression>,
}

// TODO implement Rust operator overloads
pub enum Expression {
	Conditional(ConditionalExpression),
	Constant(NumericConstant),
	Generic(GenericRef),
	Signal(SignalRef),
	Binary{op: BinaryOp, lhs: Box<Expression>, rhs: Box<Expression>},
	Unary{op: UnaryOp, operand: Box<Expression>},
	Cast{dest_type: SignalType, src: Box<Expression>},
}



pub trait IsCompileTimeConst {
	fn is_compile_time_const(&self) -> bool;
}

pub trait HasBitWidth {
	fn get_bit_width(&self) -> u32;
}

impl IsCompileTimeConst for ConditionalExpression {
	fn is_compile_time_const(&self) -> bool {
		self.default.is_compile_time_const() && self.branches.iter().all(|branch| {
			branch.condition.is_compile_time_const() && branch.value.is_compile_time_const()
		})
	}
}

impl IsCompileTimeConst for  Expression {
	fn is_compile_time_const(&self) -> bool {
		use Expression::*;
		match self {
			Conditional(cond) => cond.is_compile_time_const(),
			Constant{..} => true,
			Generic{..} => true,
			_ => false,
		}
	}
}
