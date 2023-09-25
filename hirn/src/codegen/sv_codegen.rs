use std::fmt;
use crate::{Design, ModuleId, DesignError, ModuleHandle, SignalId, design::{SignalDirection, functional_blocks::ModuleInstance}, design::SignalSignedness, design::InterfaceSignal, Expression, design::EvaluatesDimensions, BinaryOp, UnaryOp, design::{ScopeHandle, functional_blocks::BlockInstance}, ScopeId};
use super::{Codegen, CodegenError};
use std::collections::HashSet;

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
				assert!(e.branches.len() > 0);
				let mut str : String = "".into();
				for br in &e.branches {
					str = format!("{} ({}) ? ({}) : ", str, self.translate_expression(&br.condition), self.translate_expression(&br.value));
				}
				format!("{} ({})", str, self.translate_expression(&e.default))
			},
			Constant(c) => {
				format!("{}'h{}", c.width(), c.to_hex_str())
			},
			Signal(s) => {
				let mut str : String = self.translate_signal_id(s.signal);
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
			}
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
			}
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
		let sign_str = match sig.class.signedness {
			Signed => "signed",
			Unsigned => "unsigned",
		};

		let bus_width_str = format!("[({}) - 1 : 0]", self.translate_expression(&sig.class.width));
		
		let mut array_size_str = String::new();
		for dim in &sig.dimensions {
			array_size_str = format!("{}[{}]", array_size_str, self.translate_expression(&dim));
		}

		format!("{} wire{} {}{}", sign_str, bus_width_str, sig.name(), array_size_str)
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

	fn emit_assignment(&mut self, w: &mut dyn fmt::Write, lhs: &Expression, rhs: &Expression) -> Result<(), CodegenError> {
		emitln!(self, w, "assign {} = {};", self.translate_expression(&lhs), self.translate_expression(&rhs))?;
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
			emitln!(self, w, ".{}({}),", binding.0, self.translate_expression(&binding.1))?;
		}

		self.end_indent();
		emitln!(self, w, ")")?;
		Ok(())
	}

	fn emit_scope(&mut self, w: &mut dyn fmt::Write, scope_id: ScopeId, naked: bool, skip_signals: HashSet<SignalId>) -> Result<(), CodegenError> {
		let scope = self.design.get_scope_handle(scope_id).unwrap();
		
		if !naked {
			emitln!(self, w, "begin")?;
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
			emitln!(self, w, "generate if ({}) begin", self.translate_expression(&conditional_scope.condition))?;
			self.begin_indent();
			self.emit_scope(w, conditional_scope.scope, true, HashSet::new())?;
			self.end_indent();
			emitln!(self, w, "end endgenerate")?;
			processed_subscopes.insert(conditional_scope.scope);
		}

		emitln!(self, w, "")?;
		emitln!(self, w, "/* loop subscopes */")?;
		for loop_scope in scope.loop_subscopes() {
			// TODO gb name
			emitln!(self, w, "generate for (genvar {} = ({}); ({}) <= ({}); ({})++) begin",
				self.translate_expression(&loop_scope.iterator_var.into()),
				self.translate_expression(&loop_scope.iterator_begin),
				self.translate_expression(&loop_scope.iterator_var.into()),
				self.translate_expression(&loop_scope.iterator_end),
				self.translate_expression(&loop_scope.iterator_var.into()))?;
			self.begin_indent();
			self.emit_scope(w, loop_scope.scope, true, HashSet::from([loop_scope.iterator_var]))?;
			self.end_indent();
			emitln!(self, w, "end endgenerate")?;
			processed_subscopes.insert(loop_scope.scope);
		}

		emitln!(self, w, "")?;
		emitln!(self, w, "/* unconditional subscopes */")?;
		let subscope_ids = scope.subscopes();
		for subscope_id in subscope_ids {
			if !processed_subscopes.contains(&subscope_id) {
				self.emit_scope(w, subscope_id, false, HashSet::new())?;
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
			emitln!(self, w, "end")?;
		}
		Ok(())
	}
}

impl<'a> Codegen for SVCodegen<'a> {

	fn emit_module(&mut self, w: &mut dyn fmt::Write, module: ModuleId) -> Result<(), CodegenError> {
		let m = self.design.get_module_handle(module).ok_or(CodegenError::InvalidModuleId(module))?;
		
		// TODO skip param list if empty
		// TODO param list
		// TODO how to check if module is generic


		emitln!(self, w, "module {} #(", self.mangle_module_name(m.name(), m.namespace_path()))?;
		self.begin_indent();
		emitln!(self, w, "/* parameters */")?;
		self.end_indent();
		emitln!(self, w, ")(")?;
		self.begin_indent();
		emitln!(self, w, "/* interface */")?;
		
		let mut interface_signal_ids = HashSet::new();
		for sig in m.interface() {
			emitln!(self, w, "{},", self.module_interface_definition(m.clone(), sig))?; // FIXME trailing comma
			interface_signal_ids.insert(sig.signal);
		}
		
		self.end_indent();
		emitln!(self, w, ");")?;

		self.begin_indent();
		self.emit_scope(w, m.scope().id(), true, interface_signal_ids)?;
		self.end_indent();

		writeln!(w, "endmodule;")?;
		Ok(())
	}
}
