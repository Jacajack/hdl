use super::{Codegen, CodegenError};
use crate::{
	design::InterfaceSignal,
	design::SignalSignedness,
	design::{functional_blocks::BlockInstance, ScopeHandle},
	design::{functional_blocks::ModuleInstance, SignalDirection, SignalSensitivity},
	BinaryOp, Design, DesignError, Expression, ModuleHandle, ModuleId, ScopeId, SignalId, UnaryOp,
};
use std::collections::HashSet;
use std::fmt;
use log::debug;

#[derive(Clone)]
pub struct SVCodegen<'a> {
	design: &'a Design,
	indent_level: u32,
}

macro_rules! emitln {
	($self:ident, $w:expr, $($arg:tt)*) => {
		writeln!($w, "{}{}", "\t".repeat($self.indent_level as usize), format!($($arg)*))
	}
}

macro_rules! emit {
	($self:ident, $w:expr, $($arg:tt)*) => {
		write!($w, "{}{}", "\t".repeat($self.indent_level as usize), format!($($arg)*))
	}
}

impl<'a> SVCodegen<'a> {
	pub fn new(design: &'a Design) -> Self {
		Self {
			design,
			indent_level: 0,
		}
	}

	fn begin_indent(&mut self) {
		self.indent_level += 1;
	}

	fn end_indent(&mut self) {
		assert!(self.indent_level > 0);
		self.indent_level -= 1;
	}

	fn translate_signal_id(&self, id: SignalId) -> String {
		let sig = self.design.get_signal(id).unwrap();
		sig.name().into()
	}

	fn translate_expression(&self, expr: &Expression) -> String {
		use Expression::*;
		match expr {
			Conditional(e) => {
				assert!(e.branches().len() > 0);
				let mut str: String = "".into();
				for br in e.branches() {
					str = format!(
						"{} ({}) ? ({}) : ",
						str,
						self.translate_expression(&br.condition),
						self.translate_expression(&br.value)
					);
				}
				format!("{} ({})", str, self.translate_expression(e.default()))
			},
			Constant(c) => {
				format!("{}'h{}", c.width(), c.to_hex_str())
			},
			Signal(s) => {
				let mut str: String = self.translate_signal_id(s.signal);
				for slice in &s.indices {
					str = format!("{}[{}]", str, self.translate_expression(&slice));
				}
				str
			},
			Binary(e) => {
				let lhs_str = self.translate_expression(&e.lhs);
				let rhs_str = self.translate_expression(&e.rhs);
				// TODO proper width rules
				use BinaryOp::*;
				match e.op {
					Add => format!("{} + {}", lhs_str, rhs_str),
					Subtract => format!("{} - {}", lhs_str, rhs_str),
					Multiply => format!("{} * {}", lhs_str, rhs_str),
					Divide => format!("{} * {}", lhs_str, rhs_str),
					_ => todo!(), // TODO
				}
			},
			Unary(u) => {
				let operand_str = self.translate_expression(&u.operand);

				use UnaryOp::*;
				match u.op {
					Negate => format!("-{}", operand_str),
					LogicalNot => format!("!{}", operand_str),
					BitwiseNot => format!("~{}", operand_str),
					ReductionAnd => format!("&{}", operand_str),
					ReductionOr => format!("|{}", operand_str),
					ReductionXor => format!("^{}", operand_str),
				}
			},
			Builtin(b) => todo!(),
			Cast(c) => self.translate_expression(&c.src),
		}
	}

	fn mangle_module_name(&self, name: String, namespaces: Vec<String>) -> String {
		let mut segments = namespaces;
		segments.push(name);
		segments.join("_")
	}

	fn format_signal_declaration(&self, sig_id: SignalId) -> String {
		let sig = self.design.get_signal(sig_id).unwrap();

		use SignalSignedness::*;
		let sign_str = match (sig.class.is_wire(), sig.class.signedness()) {
			(true, _) => "",
			(false, Signed) => " signed",
			(false, Unsigned) => " unsigned",
		};

		let bus_width_str =	match sig.class.is_wire() {
			false => format!("[({}) - 1 : 0]", self.translate_expression(&sig.class.width())),
			true => "".into(),
		};

		let mut array_size_str = String::new();
		for dim in &sig.dimensions {
			array_size_str = format!("{}[{}]", array_size_str, self.translate_expression(&dim));
		}

		format!("wire{}{} {}{}", sign_str, bus_width_str, sig.name(), array_size_str)
	}

	fn module_interface_definition(&self, m: ModuleHandle, s: InterfaceSignal) -> String {
		let sig = self.design.get_signal(s.signal).unwrap();

		use SignalDirection::*;
		let direction_str = match s.direction {
			Input => "input",
			Output => "output",
			_ => unimplemented!(),
		};

		format!("{} {}", direction_str, self.format_signal_declaration(s.signal))
	}

	fn module_parameter_definition(&self, s: InterfaceSignal) -> String {
		format!("parameter {} = 'x", self.translate_expression(&s.signal.into()))
	}

	fn emit_assignment(
		&mut self,
		w: &mut dyn fmt::Write,
		lhs: &Expression,
		rhs: &Expression,
	) -> Result<(), CodegenError> {
		emitln!(
			self,
			w,
			"assign {} = {};",
			self.translate_expression(&lhs),
			self.translate_expression(&rhs)
		)?;
		Ok(())
	}

	fn emit_module_instance(&mut self, w: &mut dyn fmt::Write, instance: &ModuleInstance) -> Result<(), CodegenError> {
		emitln!(self, w, "{} #(", instance.module.name())?;
		self.begin_indent();
		emitln!(self, w, "/* TODO parameters */")?;
		self.end_indent();
		emitln!(self, w, ") {} (", "TODO_INSTANCE_NAME")?;
		self.begin_indent();

		for binding in instance.get_bindings() {
			emitln!(
				self,
				w,
				".{}({}),",
				binding.0,
				self.translate_expression(&binding.1.into())
			)?;
		}

		self.end_indent();
		emitln!(self, w, ")")?;
		Ok(())
	}

	fn emit_scope(
		&mut self,
		w: &mut dyn fmt::Write,
		scope_id: ScopeId,
		naked: bool,
		in_generate: bool,
		skip_signals: HashSet<SignalId>,
	) -> Result<(), CodegenError> {
		debug!("Scope codegen ({:?})", scope_id);
		let scope = self.design.get_scope_handle(scope_id).unwrap();

		if !naked {
			if in_generate {
				emitln!(self, w, "begin")?;
			} 
			else {
				emitln!(self, w, "generate if ('1) begin")?;
			}
			self.begin_indent();
		}

		emitln!(self, w, "/* signals */")?;
		for sig_id in scope.signals() {
			if !skip_signals.contains(&sig_id) {
				emitln!(self, w, "{};", self.format_signal_declaration(sig_id))?;
			}
		}

		emitln!(self, w, "")?;
		emitln!(self, w, "/* assignments */")?;
		for asmt in scope.assignments() {
			self.emit_assignment(w, &asmt.lhs, &asmt.rhs)?;
		}

		let mut processed_subscopes = HashSet::new();

		emitln!(self, w, "")?;
		emitln!(self, w, "/* if-subscopes */")?;
		for conditional_scope in scope.conditional_subscopes() {
			emitln!(
				self,
				w,
				"generate if ({}) begin",
				self.translate_expression(&conditional_scope.condition)
			)?;
			self.begin_indent();
			self.emit_scope(w, conditional_scope.scope, true, true, HashSet::new())?;
			self.end_indent();
			emitln!(self, w, "end endgenerate")?;
			processed_subscopes.insert(conditional_scope.scope);
		}

		emitln!(self, w, "")?;
		emitln!(self, w, "/* loop subscopes */")?;
		for loop_scope in scope.loop_subscopes() {
			// TODO gb name
			emitln!(
				self,
				w,
				"generate for (genvar {} = ({}); ({}) <= ({}); ({})++) begin",
				self.translate_expression(&loop_scope.iterator_var.into()),
				self.translate_expression(&loop_scope.iterator_begin),
				self.translate_expression(&loop_scope.iterator_var.into()),
				self.translate_expression(&loop_scope.iterator_end),
				self.translate_expression(&loop_scope.iterator_var.into())
			)?;
			self.begin_indent();
			self.emit_scope(w, loop_scope.scope, true, true, HashSet::from([loop_scope.iterator_var]))?;
			self.end_indent();
			emitln!(self, w, "end endgenerate")?;
			processed_subscopes.insert(loop_scope.scope);
		}

		emitln!(self, w, "")?;
		emitln!(self, w, "/* unconditional subscopes */")?;
		let subscope_ids = scope.subscopes();
		for subscope_id in subscope_ids {
			if !processed_subscopes.contains(&subscope_id) {
				self.emit_scope(w, subscope_id, false, in_generate, HashSet::new())?;
			}
			processed_subscopes.insert(subscope_id);
		}

		// TODO registers

		emitln!(self, w, "")?;
		emitln!(self, w, "/* module instances */")?;
		for block in scope.blocks() {
			match block {
				BlockInstance::Module(instance) => self.emit_module_instance(w, &instance)?,
				_ => todo!(),
			}
		}

		if !naked {
			self.end_indent();
			if in_generate {
				emitln!(self, w, "end")?;
			}
			else {
				emitln!(self, w, "end endgenerate")?;
			}
		}
		Ok(())
	}
}

impl<'a> Codegen for SVCodegen<'a> {
	fn emit_module(&mut self, w: &mut dyn fmt::Write, module: ModuleId) -> Result<(), CodegenError> {
		let m = self
			.design
			.get_module_handle(module)
			.ok_or(CodegenError::InvalidModuleId(module))?;

		let mut interface_signal_ids = HashSet::new();
		for sig in m.interface() {
			interface_signal_ids.insert(sig.signal);
		}

		let mut last_param_id = None;
		let mut last_interface_id = None;
		for sig in m.interface() {
			if matches!(
				self.design.get_signal(sig.signal).unwrap().sensitivity,
				SignalSensitivity::Generic
			) {
				last_param_id = Some(sig.signal);
			}
			else {
				last_interface_id = Some(sig.signal);
			}
		}

		emit!(self, w, "module {}", self.mangle_module_name(m.name(), m.namespace_path()))?;
		if last_param_id.is_some() {
			emitln!(self, w, " #(")?;
			self.begin_indent();
			emitln!(self, w, "/* parameters */")?;

			for sig in m.interface() {
				if matches!(
					self.design.get_signal(sig.signal).unwrap().sensitivity,
					SignalSensitivity::Generic
				) {
					emitln!(
						self, w,
						"{}{}",
						self.module_parameter_definition(sig),
						if last_param_id == Some(sig.signal) {""} else {","}
					)?; 
				}
			}

			self.end_indent();
			emit!(self, w, ")")?;
		}

		if last_interface_id.is_some() {
			emitln!(self, w, "(")?;	
			self.begin_indent();
			emitln!(self, w, "/* interface */")?;

			for sig in m.interface(){
				if !matches!(
					self.design.get_signal(sig.signal).unwrap().sensitivity,
					SignalSensitivity::Generic
				) {
					emitln!(
						self, w,
						"{}{}",
						self.module_interface_definition(m.clone(), sig),
						if last_interface_id == Some(sig.signal) {""} else {","}
					)?; 
				}
			}

			self.end_indent();
			emit!(self, w, ")")?;
		}
		emitln!(self, w, ";")?;

		self.begin_indent();
		self.emit_scope(w, m.scope().id(), true, false, interface_signal_ids)?;
		self.end_indent();

		writeln!(w, "endmodule;")?;
		Ok(())
	}
}
