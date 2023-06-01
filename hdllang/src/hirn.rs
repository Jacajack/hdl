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

pub struct ModuleInstance {
	pub name: String,
	pub bindings: Vec<(String, SignalRef)>,
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

pub struct ConditionalScope {
	pub condition: Expression,
	pub scope: Scope,
}

pub struct RangeScope {
	pub iterator_var: GenericRef,
	pub iterator_begin: Expression,
	pub iterator_end: Expression,
	pub scope: Scope,
}

pub struct Assignment {
	pub lhs: SignalRef,
	pub rhs: Expression,
}

pub struct Register {
	pub input_nreset: Option<SignalRef>,
	pub input_en: Option<SignalRef>,
	pub input_clk: SignalRef,
	pub input_next: SignalRef,
	pub output_data: SignalRef,
}

pub struct TristateBuffer {
	pub input_en: Option<SignalRef>,
	pub input_data: SignalRef,
	pub output_data: SignalRef,
}

pub struct ClockGate {
	pub input_en: SignalRef,
	pub input_clk: SignalRef,
	pub output_clk: SignalRef,
}

pub struct FfSync {
	pub input_nreset: Option<SignalRef>,
	pub input_en: Option<SignalRef>,
	pub input_clk1: SignalRef,
	pub input_clk2: SignalRef,
	pub input_next: SignalRef,
	pub output_data: SignalRef,
}

pub enum FunctionalBlock {
	Register(Register),
	TristateBuffer(TristateBuffer),
	ClockGate(ClockGate),
	FfSync(FfSync),
	Instance(ModuleInstance),
}

pub struct Signal {
	// TODO
}

pub struct Generic {
	// TODO
}

pub struct Scope {
	// signals
	// genvars
	// assignments

	signals: Vec<Signal>,
	generic: Vec<Generic>,
	assignments: Vec<Assignment>,
	loops: Vec<RangeScope>,
	conditionals: Vec<ConditionalScope>,
	blocks: Vec<FunctionalBlock>,
}

impl Scope {
	pub fn new() -> Self {
		Self {
			signals: vec![],
			generic: vec![],
			assignments: vec![],
			loops: vec![],
			conditionals: vec![],
			blocks: vec![],
		}
	}

	pub fn get_signal_ref(&mut self, name: &String) -> SignalRef {
		todo!();
	}

	pub fn add_signal(&mut self, signal_type: SignalType, name: String) {
		todo!();
	}

	pub fn assign(&mut self, lhs: SignalRef, rhs: Expression) {
		todo!();
	}

	pub fn conditional_block(&mut self, condition: &Expression) -> &mut Scope {
		todo!();
	}

	pub fn loop_block(&mut self, iterator: GenericRef, from: Expression, to: Expression) -> &mut Scope {
		todo!();
	}

	pub fn subscope(&mut self) -> &mut Scope {
		todo!();
	}
	
	// TODO add_register
	// TODO add_tristate_buffer
	// TODO add_clock_gate
	// TODO add_ff_sync
	// TODO add_instance

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
	Async,
	Comb(SensitivityList),
	Sync(SensitivityList),
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

impl Expression {

	pub fn cast(self, dest_type: SignalType) -> Self {
		Self::Cast{dest_type, src: Box::new(self)}
	}

	pub fn zero_extend(self, width: u32) -> Self {
		Self::Unary{op: UnaryOp::ZeroExtend{width}, operand: Box::new(self)}
	}

	pub fn sign_extend(self, width: u32) -> Self {
		Self::Unary{op: UnaryOp::SignExtend{width}, operand: Box::new(self)}
	}

	pub fn bit_select(self, msb: u32, lsb: u32) -> Self {
		assert!(msb >= lsb);
		Self::Unary{op: UnaryOp::BitSelect(msb, lsb), operand: Box::new(self)}
	}

	// TODO reduction AND/OR/XOR
	// TODO bitwise not
	// TODO from i32
	// TODO from u32
	// TODO from bool
	// TODO from signal ref
	// TODO from generic ref
	// TODO remaining binary ops
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
