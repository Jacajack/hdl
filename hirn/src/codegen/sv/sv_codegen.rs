use crate::codegen::{Codegen, CodegenError};
use crate::design::{Register, ScopeHandle};
use crate::{
	design::BlockInstance,
	design::InterfaceSignal,
	design::SignalSignedness,
	design::{
		Design, Expression, HasComment, HasInstanceName, ModuleHandle, ModuleId, ModuleInstance, ScopeId,
		SignalDirection, SignalId, SignalSensitivity,
	},
};
use log::debug;
use std::collections::{HashMap, HashSet};
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
				"[({}):0]",
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

	fn format_localparam_declaration(
		&self,
		sig_id: SignalId,
		value: &Option<Expression>,
	) -> Result<String, CodegenError> {
		let sig = self.design.get_signal(sig_id).unwrap();
		Ok(format!(
			"localparam {} = {}",
			sig.name(),
			match value {
				Some(expr) => self.translate_expression(expr, false)?,
				None => "'x".into(), // This is evil
			}
		))
	}

	fn module_interface_definition(&self, _m: ModuleHandle, s: InterfaceSignal) -> Result<String, CodegenError> {
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
		// Get instantionated module interface
		let m = &instance.module;
		let interface = m.interface();
		let bindings = instance.get_bindings();

		let mut generic_bindings = HashMap::new();
		let mut electric_bindings = HashMap::new();

		for port in &interface {
			let internal_sig = self.design.get_signal(port.signal).expect("Design must be valid");
			for (binding_name, external_sig) in bindings {
				if binding_name == internal_sig.name() {
					if internal_sig.is_generic() {
						generic_bindings.insert(binding_name, external_sig.clone());
					}
					else {
						electric_bindings.insert(binding_name, external_sig.clone());
					}
				}
			}
		}

		// Sanity check: the design must be valid
		assert!(generic_bindings.len() + electric_bindings.len() == interface.len());

		emit!(self, w, "{}", instance.module.name())?;
		if !generic_bindings.is_empty() {
			emit!(self, w, " #(\n")?;
			self.begin_indent();

			for (index, (name, signal_id)) in generic_bindings.iter().enumerate() {
				emitln!(
					self,
					w,
					".{}({}){}",
					name,
					self.translate_expression(&Expression::from(*signal_id), true)?,
					if index == generic_bindings.len() - 1 { "" } else { "," }
				)?;
			}

			self.end_indent();
			emitln!(self, w, ")")?;
		}

		emit!(self, w, " {}", instance.instance_name())?;

		if !electric_bindings.is_empty() {
			emitln!(self, w, " (")?;
			self.begin_indent();

			for (index, (name, signal_id)) in electric_bindings.iter().enumerate() {
				emitln!(
					self,
					w,
					".{}({}){}",
					name,
					self.translate_expression(&Expression::from(*signal_id), true)?,
					if index == electric_bindings.len() - 1 { "" } else { "," }
				)?;
			}

			self.end_indent();
			emit!(self, w, ")")?;
		}

		emitln!(self, w, ";")?;
		emitln!(self, w, "")?;
		Ok(())
	}

	fn emit_register_instance(&mut self, w: &mut dyn fmt::Write, reg: &Register) -> Result<(), CodegenError> {
		let reg_name = format!("{}_r$", reg.instance_name());
		let input_signal = &self.design.get_signal(reg.input_next).unwrap();
		let nreset_expr = Expression::from(reg.input_nreset).logical_not();

		if input_signal.is_wire() {
			emitln!(self, w, "reg {};", reg_name)?;
		}
		else {
			emitln!(
				self,
				w,
				"reg {}[{}:0] {};",
				if input_signal.is_unsigned() {
					"unsigned"
				}
				else {
					"signed"
				},
				self.translate_expression_try_eval(&(input_signal.width().clone() - 1u32.into()), false)?,
				reg_name
			)?;
		}
		emitln!(
			self,
			w,
			"assign {} = {};",
			self.translate_expression(&reg.output_data.into(), true)?,
			reg_name
		)?;
		emitln!(
			self,
			w,
			"always @(posedge {})",
			self.translate_expression(&reg.input_clk.into(), true)?
		)?;
		self.begin_indent();
		emitln!(self, w, "if ({})", self.translate_expression(&nreset_expr, true)?)?;
		self.begin_indent();
		emitln!(self, w, "{} <= '0;", reg_name)?;
		self.end_indent();
		emitln!(
			self,
			w,
			"else if ({})",
			self.translate_expression(&reg.input_en.into(), true)?
		)?;
		self.begin_indent();
		emitln!(
			self,
			w,
			"{} <= {};",
			reg_name,
			self.translate_expression(&reg.input_next.into(), true)?
		)?;
		self.end_indent();
		emitln!(self, w, "else")?;
		self.begin_indent();
		emitln!(self, w, "{} <= {};", reg_name, reg_name)?;
		self.end_indent();
		self.end_indent();
		emitln!(self, w, "")?;
		Ok(())
	}

	fn emit_scope(
		&mut self,
		w: &mut dyn fmt::Write,
		scope_id: ScopeId,
		naked: bool,
		within_generate: bool,
		mut skip_signals: HashSet<SignalId>,
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

		// I very much don't like this code.
		let mut emitted_any_localparam = false;
		for sig_id in scope.signals() {
			if !skip_signals.contains(&sig_id) {
				if self.design.get_signal(sig_id).unwrap().is_generic() {
					fn search_scope(design: &Design, scope: ScopeHandle, sig_id: SignalId) -> Option<Expression> {
						for asmt in scope.assignments() {
							match asmt.lhs.try_drive() {
								Some(lhs_slice) => {
									if lhs_slice.signal == sig_id {
										return Some(asmt.rhs);
									}
								},
								None => {},
							}
						}

						for subscope_id in scope.subscopes() {
							let subscope = design.get_scope_handle(subscope_id).unwrap();
							if let Some(expr) = search_scope(design, subscope, sig_id) {
								return Some(expr);
							}
						}

						return None;
					}

					let rhs_expr = search_scope(self.design, scope.clone(), sig_id);
					self.emit_metadata_comment(w, &self.design.get_signal(sig_id).unwrap())?;
					emitln!(self, w, "{};", self.format_localparam_declaration(sig_id, &rhs_expr)?)?;
					skip_signals.insert(sig_id);
					emitted_any_localparam = true;
				}
			}
		}

		if emitted_any_localparam {
			emitln!(self, w, "")?;
		}

		let mut emitted_any_signals = false;
		for sig_id in scope.signals() {
			if !skip_signals.contains(&sig_id) {
				self.emit_metadata_comment(w, &self.design.get_signal(sig_id).unwrap())?;
				emitln!(self, w, "{};", self.format_signal_declaration(sig_id)?)?;
				skip_signals.insert(sig_id);
				emitted_any_signals = true;
			}
		}

		if emitted_any_signals {
			emitln!(self, w, "")?;
		}

		let mut emitted_any_assignemnts = false;
		for asmt in scope.assignments() {
			match asmt.lhs.try_drive() {
				Some(slice) => {
					if !self.design.get_signal(slice.signal).unwrap().is_generic() {
						self.emit_assignment(w, &asmt.lhs, &asmt.rhs)?;
						emitted_any_assignemnts = true;
					}
				},
				None => panic!("Cannot assign to this LHS expression"),
			}
		}

		if emitted_any_assignemnts {
			emitln!(self, w, "")?;
		}

		let mut processed_subscopes = HashSet::new();

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
			emitln!(self, w, "")?;
		}

		for loop_scope in scope.loop_subscopes() {
			// TODO gb name
			emitln!(
				self,
				w,
				"{}for (genvar {} = ({}); ({}) <= ({}); {}++) begin",
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
			emitln!(self, w, "")?;
			processed_subscopes.insert(loop_scope.scope);
		}

		let subscope_ids = scope.subscopes();
		for subscope_id in subscope_ids {
			if !processed_subscopes.contains(&subscope_id) {
				self.emit_scope(w, subscope_id, false, in_generate, HashSet::new())?;
			}
			processed_subscopes.insert(subscope_id);
		}

		for block in scope.blocks() {
			if let BlockInstance::Register(reg) = block {
				self.emit_register_instance(w, &reg)?;
			}
		}

		for block in scope.blocks() {
			if let BlockInstance::Module(instance) = block {
				self.emit_module_instance(w, &instance)?;
			}
		}

		if !naked {
			self.end_indent();
			if within_generate {
				emitln!(self, w, "end\n")?;
			}
			else {
				emitln!(self, w, "end endgenerate\n")?;
			}
		}
		else {
			emitln!(self, w, "")?;
		}
		Ok(())
	}

	fn emit_multiline_comment(&mut self, w: &mut dyn fmt::Write, comment: &str) -> Result<(), CodegenError> {
		for line in comment.lines() {
			emitln!(self, w, "/// {}", line)?;
		}
		Ok(())
	}

	fn emit_metadata_comment(&mut self, w: &mut dyn fmt::Write, object: &dyn HasComment) -> Result<(), CodegenError> {
		if let Some(comment) = object.get_comment() {
			self.emit_multiline_comment(w, &comment)?;
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

		self.emit_metadata_comment(w, &m)?;
		emit!(
			self,
			w,
			"module {}",
			self.mangle_module_name(m.name(), m.namespace_path())
		)?;
		if last_param_id.is_some() {
			emitln!(self, w, " #(")?;
			self.begin_indent();
			// emitln!(self, w, "/* parameters */")?;

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
			// emitln!(self, w, "/* interface */")?;

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
