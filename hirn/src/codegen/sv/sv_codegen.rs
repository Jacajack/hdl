use crate::codegen::{Codegen, CodegenError};
use crate::{
	design::BlockInstance,
	design::InterfaceSignal,
	design::SignalSignedness,
	design::{
		BinaryOp, BuiltinOp, Design, Expression, ModuleHandle, ModuleId, ModuleInstance, ScopeId, SignalDirection,
		SignalId, SignalSensitivity, UnaryOp, WidthExpression,
	},
};
use log::debug;
use std::collections::HashSet;
use std::fmt;

#[derive(Clone)]
pub struct SVCodegen<'a> {
	pub(super) design: &'a Design,
	pub(super) indent_level: u32,
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

	fn mangle_module_name(&self, name: String, namespaces: Vec<String>) -> String {
		let mut segments = namespaces;
		segments.push(name);
		segments.join("_")
	}

	fn format_signal_declaration(&self, sig_id: SignalId) -> Result<String, CodegenError> {
		let sig = self.design.get_signal(sig_id).unwrap();

		use SignalSignedness::*;
		let sign_str = match (sig.class.is_wire(), sig.class.signedness()) {
			(true, _) => "",
			(false, Signed) => " signed",
			(false, Unsigned) => " unsigned",
		};

		let bus_width_str = match sig.class.is_wire() {
			false => format!(
				"[({}): 0]",
				self.translate_expression_try_eval(&(sig.class.width().clone() - 1.into()), false)?
			),
			true => "".into(),
		};

		let mut array_size_str = String::new();
		for dim in &sig.dimensions {
			array_size_str = format!(
				"{}[{}]",
				array_size_str,
				self.translate_expression_try_eval(&dim, false)?
			);
		}

		Ok(format!(
			"wire{}{} {}{}",
			sign_str,
			bus_width_str,
			sig.name(),
			array_size_str
		))
	}

	fn module_interface_definition(&self, m: ModuleHandle, s: InterfaceSignal) -> Result<String, CodegenError> {
		let sig = self.design.get_signal(s.signal).unwrap();

		use SignalDirection::*;
		let direction_str = match s.direction {
			Input => "input",
			Output => "output",
			_ => unimplemented!(),
		};

		Ok(format!(
			"{} {}",
			direction_str,
			self.format_signal_declaration(s.signal)?
		))
	}

	fn module_parameter_definition(&self, s: InterfaceSignal) -> Result<String, CodegenError> {
		Ok(format!(
			"parameter {} = 'x",
			self.translate_expression(&s.signal.into(), false)?
		))
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
			self.translate_expression(&lhs, true)?,
			self.translate_expression(&rhs, true)?
		)?;
		Ok(())
	}

	// FIXME implement io::Write?
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
				self.translate_expression(&binding.1.into(), true)?
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
		within_generate: bool,
		skip_signals: HashSet<SignalId>,
	) -> Result<(), CodegenError> {
		debug!("Scope codegen ({:?})", scope_id);
		let scope = self.design.get_scope_handle(scope_id).unwrap();

		let in_generate = within_generate || (!naked && !within_generate); // FIXME not a big fan of that
		if !naked {
			if within_generate {
				emitln!(self, w, "if ('1) begin")?;
			}
			else {
				emitln!(self, w, "generate if ('1) begin")?;
			}
			self.begin_indent();
		}

		emitln!(self, w, "/* signals */")?;
		for sig_id in scope.signals() {
			if !skip_signals.contains(&sig_id) {
				emitln!(self, w, "{};", self.format_signal_declaration(sig_id)?)?;
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
				"{}if ({}) begin",
				if in_generate { "" } else { "generate " },
				self.translate_expression(&conditional_scope.condition, true)?
			)?;
			self.begin_indent();
			self.emit_scope(w, conditional_scope.scope, true, true, HashSet::new())?;
			processed_subscopes.insert(conditional_scope.scope);
			self.end_indent();

			if let Some(else_scope) = conditional_scope.else_scope {
				emitln!(self, w, "end else begin")?;
				self.begin_indent();
				self.emit_scope(w, else_scope, true, true, HashSet::new())?;
				processed_subscopes.insert(else_scope);
				self.end_indent();
			}

			emitln!(self, w, "end{}", if in_generate { "" } else { " endgenerate" })?;
		}

		emitln!(self, w, "")?;
		emitln!(self, w, "/* loop subscopes */")?;
		for loop_scope in scope.loop_subscopes() {
			// TODO gb name
			emitln!(
				self,
				w,
				"{}for (genvar {} = ({}); ({}) <= ({}); ({})++) begin",
				if in_generate { "" } else { "generate " },
				self.translate_expression(&loop_scope.iterator_var.into(), true)?,
				self.translate_expression(&loop_scope.iterator_begin, true)?,
				self.translate_expression(&loop_scope.iterator_var.into(), true)?,
				self.translate_expression(&loop_scope.iterator_end, true)?,
				self.translate_expression(&loop_scope.iterator_var.into(), true)?
			)?;
			self.begin_indent();
			self.emit_scope(
				w,
				loop_scope.scope,
				true,
				true,
				HashSet::from([loop_scope.iterator_var]),
			)?;
			self.end_indent();
			emitln!(self, w, "end{}", if in_generate { "" } else { " endgenerate" })?;
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
			if within_generate {
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

		emit!(
			self,
			w,
			"module {}",
			self.mangle_module_name(m.name(), m.namespace_path())
		)?;
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
						self,
						w,
						"{}{}",
						self.module_parameter_definition(sig)?,
						if last_param_id == Some(sig.signal) { "" } else { "," }
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

			for sig in m.interface() {
				if !matches!(
					self.design.get_signal(sig.signal).unwrap().sensitivity,
					SignalSensitivity::Generic
				) {
					emitln!(
						self,
						w,
						"{}{}",
						self.module_interface_definition(m.clone(), sig)?,
						if last_interface_id == Some(sig.signal) { "" } else { "," }
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

		writeln!(w, "endmodule")?;
		Ok(())
	}
}
