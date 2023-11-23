use crate::codegen::{Codegen, CodegenError};
use crate::design::{DesignHandle, EvalContext, EvaluatesType, Register, ScopeHandle, SignalClass};
use crate::{
	design::BlockInstance,
	design::InterfaceSignal,
	design::SignalSignedness,
	design::{
		Expression, HasComment, HasInstanceName, HasSensitivity, HasSignedness, ModuleHandle, ModuleId, ModuleInstance,
		ScopeId, SignalDirection, SignalId, SignalSensitivity,
	},
};
use log::{debug, warn};
use std::collections::{HashMap, HashSet};
use std::fmt;

use super::sv_expr::{IntermediateSignal, SVExpressionCodegen};

#[derive()]
pub struct SVCodegen<'a> {
	pub(super) design: DesignHandle,
	pub(super) indent_level: u32,
	tmp_counter: u32, // FIXME
	signal_subs: HashMap<SignalId, String>,
	output_stream: &'a mut dyn fmt::Write,
}

macro_rules! emitln {
	($self:ident, $($arg:tt)*) => {
		writeln!($self.output_stream, "{}{}", "\t".repeat($self.indent_level as usize), format!($($arg)*))
	}
}

macro_rules! emit {
	($self:ident, $($arg:tt)*) => {
		write!($self.output_stream, "{}{}", "\t".repeat($self.indent_level as usize), format!($($arg)*))
	}
}

fn expr_sub_one(design: DesignHandle, expr: &Expression) -> Expression {
	let expr_type = expr
		.eval_type(&EvalContext::without_assumptions(design))
		.expect("All variables must be in design");
	if expr_type.is_signed() {
		(expr.clone() - 1i64.into()).into()
	}
	else {
		(expr.clone() - 1u64.into()).into()
	}
}

impl<'a> SVCodegen<'a> {
	pub fn new(design: DesignHandle, w: &'a mut dyn fmt::Write) -> Self {
		Self {
			design,
			indent_level: 0,
			tmp_counter: 0,
			signal_subs: HashMap::new(),
			output_stream: w,
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

	fn format_signal_declaration_impl(
		&mut self,
		name: &str,
		class: &SignalClass,
		dimensions: &[Expression],
	) -> Result<String, CodegenError> {
		use SignalSignedness::*;
		let sign_str = match (class.is_wire(), class.signedness()) {
			(true, _) => "",
			(false, Signed) => " signed",
			(false, Unsigned) => " unsigned",
		};

		let bus_msb_str =
			self.translate_expression_try_eval(&expr_sub_one(self.design.clone(), class.width()), false)?;
		let bus_width_str = match class.is_wire() {
			false => format!("[{}:0]", bus_msb_str),
			true => "".into(),
		};

		let mut array_size_str = String::new();
		for dim in dimensions {
			array_size_str = format!(
				"{}[{}]",
				array_size_str,
				self.translate_expression_try_eval(&dim, false)?
			);
		}

		Ok(format!("wire{}{} {}{}", sign_str, bus_width_str, name, array_size_str))
	}

	fn format_signal_declaration(&mut self, sig_id: SignalId) -> Result<String, CodegenError> {
		let sig = self.design.get_signal(sig_id).expect("signal must be in design");
		self.format_signal_declaration_impl(sig.name(), &sig.class, &sig.dimensions)
	}

	fn format_intermediate_signal_declaration(&mut self, sig: &IntermediateSignal) -> Result<String, CodegenError> {
		use SignalSensitivity::*;
		match sig.sensitivity() {
			Generic => Ok(format!(
				"{}",
				self.format_param_declaration_impl(
					sig.name().into(),
					&sig.class(),
					&[],
					&Some(sig.value().clone()),
					true
				)?,
			)),
			_ => Ok(format!(
				"{} = {}",
				self.format_signal_declaration_impl(sig.name(), &sig.class(), &vec![])?,
				self.translate_expression(&sig.value().clone(), false)?,
			)),
		}
	}

	fn format_param_declaration_impl(
		&mut self,
		name: &str,
		class: &SignalClass,
		dimensions: &[Expression],
		value: &Option<Expression>,
		is_local: bool,
	) -> Result<String, CodegenError> {
		let bus_msb_str =
			self.translate_expression_try_eval(&expr_sub_one(self.design.clone(), class.width()), false)?;
		let bus_width_str = match class.is_wire() {
			false => format!("[{}:0]", bus_msb_str),
			true => "".into(),
		};

		let mut array_size_str = String::new();
		for dim in dimensions {
			array_size_str = format!(
				"{}[{}]",
				array_size_str,
				self.translate_expression_try_eval(&dim, false)?
			);
		}

		use SignalSignedness::*;
		let sign_str = match (class.is_wire(), class.signedness()) {
			(true, _) => "",
			(false, Signed) => " signed",
			(false, Unsigned) => " unsigned",
		};

		if is_local {
			Ok(format!(
				"localparam logic{}{} {}{} = {}",
				sign_str,
				bus_width_str,
				name,
				array_size_str,
				match value {
					Some(expr) => self.translate_expression(expr, true)?,
					None => {
						warn!("No value assigned for localparam definition!");
						"'x".into() // This is evil
					},
				}
			))
		}
		else {
			Ok(format!(
				"parameter logic{}{} {}{}",
				sign_str, bus_width_str, name, array_size_str,
			))
		}
	}

	fn format_localparam_declaration(
		&mut self,
		sig_id: SignalId,
		value: &Option<Expression>,
	) -> Result<String, CodegenError> {
		let sig = self.design.get_signal(sig_id).unwrap();
		self.format_param_declaration_impl(sig.name().into(), &sig.class, &sig.dimensions, value, true)
	}

	fn module_interface_definition(&mut self, _m: ModuleHandle, s: InterfaceSignal) -> Result<String, CodegenError> {
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

	fn format_module_parameter_declaration(&mut self, s: InterfaceSignal) -> Result<String, CodegenError> {
		// TODO Note: we could fall back to plain 'parameter' here for max compatibility
		let sig = self.design.get_signal(s.signal).unwrap();
		self.format_param_declaration_impl(sig.name(), &sig.class, &sig.dimensions, &None, false)
	}

	fn emit_assignment(&mut self, lhs: &Expression, rhs: &Expression) -> Result<(), CodegenError> {
		let lhs_str = self.translate_expression(&lhs, true)?;
		let rhs_str = self.translate_expression(&rhs, true)?;
		emitln!(self, "assign {} = {};", lhs_str, rhs_str)?;
		Ok(())
	}

	// FIXME implement io::Write?
	fn emit_module_instance(&mut self, instance: &ModuleInstance) -> Result<(), CodegenError> {
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

		emit!(self, "{}", instance.module.name())?;
		if !generic_bindings.is_empty() {
			emit!(self, " #(\n")?;
			self.begin_indent();

			for (index, (name, signal_id)) in generic_bindings.iter().enumerate() {
				// FIXME this is a hack - if something goes wrong and we emit and intermediate here, we die
				let expr_str = self.translate_expression(&Expression::from(*signal_id), true)?;
				emitln!(
					self,
					".{}({}){}",
					name,
					expr_str,
					if index == generic_bindings.len() - 1 { "" } else { "," }
				)?;
			}

			self.end_indent();
			emitln!(self, ")")?;
		}

		emit!(self, " {}", instance.instance_name())?;

		emitln!(self, " (")?;
		if !electric_bindings.is_empty() {
			self.begin_indent();

			for (index, (name, signal_id)) in electric_bindings.iter().enumerate() {
				let expr_str = self.translate_expression(&Expression::from(*signal_id), true)?;
				emitln!(
					self,
					".{}({}){}",
					name,
					expr_str,
					if index == electric_bindings.len() - 1 { "" } else { "," }
				)?;
			}

			self.end_indent();
		}
		emit!(self, ")")?;

		emitln!(self, ";")?;
		emitln!(self, "")?;
		Ok(())
	}

	fn emit_register_instance(&mut self, reg: &Register) -> Result<(), CodegenError> {
		let reg_name = format!("{}_r$", reg.instance_name());
		let input_signal = &self.design.get_signal(reg.input_next).unwrap();
		let reset_cond = Expression::from(reg.input_nreset).logical_not();

		let msb_str =
			self.translate_expression_try_eval(&expr_sub_one(self.design.clone(), &input_signal.width()), false)?;
		let output_str = self.translate_expression(&reg.output_data.into(), true)?;
		let clk_str = self.translate_expression(&reg.input_clk.into(), true)?;
		let en_str = self.translate_expression(&reg.input_en.into(), true)?;
		let next_str = self.translate_expression(&reg.input_next.into(), true)?;
		let nreset_str = self.translate_expression(&reg.input_nreset.into(), true)?;
		let reset_cond_str = self.translate_expression(&reset_cond, true)?;

		if input_signal.is_wire() {
			emitln!(self, "reg {} = '0;", reg_name)?;
		}
		else {
			emitln!(
				self,
				"reg {}[{}:0] {} = '0;",
				if input_signal.is_unsigned() {
					"unsigned"
				}
				else {
					"signed"
				},
				msb_str,
				reg_name
			)?;
		}
		emitln!(self, "assign {} = {};", output_str, reg_name)?;
		emitln!(self, "always @(posedge {} or negedge {})", clk_str, nreset_str)?;
		self.begin_indent();
		emitln!(self, "if ({})", reset_cond_str)?;
		self.begin_indent();
		emitln!(self, "{} <= '0;", reg_name)?;
		self.end_indent();
		emitln!(self, "else if ({})", en_str)?;
		self.begin_indent();
		emitln!(self, "{} <= {};", reg_name, next_str,)?;
		self.end_indent();
		self.end_indent();
		emitln!(self, "")?;
		Ok(())
	}

	fn emit_scope(
		&mut self,
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
				emitln!(self, "if ('1) begin")?;
			}
			else {
				emitln!(self, "generate if ('1) begin")?;
			}
			self.begin_indent();
		}

		// I very much don't like this code.
		let mut emitted_any_localparam = false;
		for sig_id in scope.signals() {
			if !skip_signals.contains(&sig_id) {
				if self.design.get_signal(sig_id).unwrap().is_generic() {
					fn search_scope(design: DesignHandle, scope: ScopeHandle, sig_id: SignalId) -> Option<Expression> {
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
							if let Some(expr) = search_scope(design.clone(), subscope, sig_id) {
								return Some(expr);
							}
						}

						return None;
					}

					let rhs_expr = search_scope(self.design.clone(), scope.clone(), sig_id);
					self.emit_metadata_comment(&self.design.get_signal(sig_id).unwrap())?;
					let localparam_str = self.format_localparam_declaration(sig_id, &rhs_expr)?;
					emitln!(self, "{};", localparam_str)?;
					skip_signals.insert(sig_id);
					emitted_any_localparam = true;
				}
			}
		}

		if emitted_any_localparam {
			emitln!(self, "")?;
		}

		let mut emitted_any_signals = false;
		for sig_id in scope.signals() {
			if !skip_signals.contains(&sig_id) {
				self.emit_metadata_comment(&self.design.get_signal(sig_id).unwrap())?;
				let sig_str = self.format_signal_declaration(sig_id)?;
				emitln!(self, "{};", sig_str)?;
				skip_signals.insert(sig_id);
				emitted_any_signals = true;
			}
		}

		if emitted_any_signals {
			emitln!(self, "")?;
		}

		let mut emitted_any_assignemnts = false;
		for asmt in scope.assignments() {
			match asmt.lhs.try_drive() {
				Some(slice) => {
					if !self.design.get_signal(slice.signal).unwrap().is_generic() {
						self.emit_assignment(&asmt.lhs, &asmt.rhs)?;
						emitted_any_assignemnts = true;
					}
				},
				None => panic!("Cannot assign to this LHS expression"),
			}
		}

		if emitted_any_assignemnts {
			emitln!(self, "")?;
		}

		let mut processed_subscopes = HashSet::new();

		for conditional_scope in scope.conditional_subscopes() {
			let condition_str = self.translate_expression(&conditional_scope.condition, true)?;
			emitln!(
				self,
				"{}if ({}) begin",
				if in_generate { "" } else { "generate " },
				condition_str,
			)?;
			self.begin_indent();
			self.emit_scope(conditional_scope.scope, true, true, HashSet::new())?;
			processed_subscopes.insert(conditional_scope.scope);
			self.end_indent();

			if let Some(else_scope) = conditional_scope.else_scope {
				emitln!(self, "end else begin")?;
				self.begin_indent();
				self.emit_scope(else_scope, true, true, HashSet::new())?;
				processed_subscopes.insert(else_scope);
				self.end_indent();
			}

			emitln!(self, "end{}", if in_generate { "" } else { " endgenerate" })?;
			emitln!(self, "")?;
		}

		for loop_scope in scope.loop_subscopes() {
			let iterator_str = self.translate_expression(&loop_scope.iterator_var.into(), true)?;
			let begin_str = self.translate_expression(&loop_scope.iterator_begin, true)?;
			let end_str = self.translate_expression(&loop_scope.iterator_end, true)?;
			// TODO gb name
			emitln!(
				self,
				"{}for (genvar {} = ({}); ({}) <= ({}); {}++) begin",
				if in_generate { "" } else { "generate " },
				iterator_str,
				begin_str,
				iterator_str,
				end_str,
				iterator_str
			)?;
			self.begin_indent();
			self.emit_scope(loop_scope.scope, true, true, HashSet::from([loop_scope.iterator_var]))?;
			self.end_indent();
			emitln!(self, "end{}", if in_generate { "" } else { " endgenerate" })?;
			emitln!(self, "")?;
			processed_subscopes.insert(loop_scope.scope);
		}

		let subscope_ids = scope.subscopes();
		for subscope_id in subscope_ids {
			if !processed_subscopes.contains(&subscope_id) {
				self.emit_scope(subscope_id, false, in_generate, HashSet::new())?;
			}
			processed_subscopes.insert(subscope_id);
		}

		for block in scope.blocks() {
			if let BlockInstance::Register(reg) = block {
				self.emit_register_instance(&reg)?;
			}
		}

		for block in scope.blocks() {
			if let BlockInstance::Module(instance) = block {
				self.emit_module_instance(&instance)?;
			}
		}

		if !naked {
			self.end_indent();
			if within_generate {
				emitln!(self, "end\n")?;
			}
			else {
				emitln!(self, "end endgenerate\n")?;
			}
		}
		else {
			emitln!(self, "")?;
		}
		Ok(())
	}

	fn emit_multiline_comment(&mut self, comment: &str) -> Result<(), CodegenError> {
		for line in comment.lines() {
			emitln!(self, "/// {}", line)?;
		}
		Ok(())
	}

	fn emit_metadata_comment(&mut self, object: &dyn HasComment) -> Result<(), CodegenError> {
		if let Some(comment) = object.get_comment() {
			self.emit_multiline_comment(&comment)?;
		}
		Ok(())
	}

	/// Translates an expression and emits intermediate variables
	fn translate_expression_impl(
		&mut self,
		expr: &Expression,
		width_casts: bool,
		try_eval: bool,
	) -> Result<String, CodegenError> {
		// FIXME subs table is cloned here
		let mut cg = SVExpressionCodegen::new(
			self.design.clone(),
			width_casts,
			self.tmp_counter,
			Some(self.signal_subs.clone()),
		);
		let result = if try_eval {
			cg.translate_expression_try_eval(expr)?
		}
		else {
			cg.translate_expression(expr)?
		};

		// Note: tmp_counter needs to be updated here, because new intermediates can be generated
		// while translating the intermediates introduced by this expression
		assert!(self.tmp_counter <= cg.get_tmp_counter());
		self.tmp_counter = cg.get_tmp_counter();

		for intermediate in cg.intermediates() {
			let str = self.format_intermediate_signal_declaration(intermediate)?;
			emitln!(self, "{};", str)?;
		}
		Ok(result)
	}

	fn translate_expression(&mut self, expr: &Expression, width_casts: bool) -> Result<String, CodegenError> {
		self.translate_expression_impl(expr, width_casts, false)
	}

	fn translate_expression_try_eval(&mut self, expr: &Expression, width_casts: bool) -> Result<String, CodegenError> {
		self.translate_expression_impl(expr, width_casts, true)
	}
}

impl<'a> Codegen for SVCodegen<'a> {
	fn emit_module(&mut self, module: ModuleId) -> Result<(), CodegenError> {
		// i hate this
		self.tmp_counter = 0;
		self.signal_subs = HashMap::new();

		let m = self
			.design
			.get_module_handle(module)
			.ok_or(CodegenError::InvalidModuleId(module))?;

		let mut interface_signal_ids = HashSet::new();
		for sig in m.interface() {
			interface_signal_ids.insert(sig.signal);
		}

		let mut output_signal_ids = HashSet::new();
		let mut last_param_id = None;
		let mut last_interface_id = None;
		for intf_sig in m.interface() {
			let sig = self.design.get_signal(intf_sig.signal).unwrap();
			if sig.is_generic() {
				last_param_id = Some(intf_sig.signal);
			}
			else {
				last_interface_id = Some(intf_sig.signal);
			}

			if intf_sig.direction == SignalDirection::Output {
				output_signal_ids.insert(intf_sig.signal);
			}
		}

		self.emit_metadata_comment(&m)?;
		emit!(self, "module {}", self.mangle_module_name(m.name(), m.namespace_path()))?;
		if last_param_id.is_some() {
			emitln!(self, " #(")?;
			self.begin_indent();
			// emitln!(self, "/* parameters */")?;

			for sig in m.interface() {
				if matches!(
					self.design.get_signal(sig.signal).unwrap().sensitivity,
					SignalSensitivity::Generic
				) {
					let param_def_str = self.format_module_parameter_declaration(sig)?;
					emitln!(
						self,
						"{}{}",
						param_def_str,
						if last_param_id == Some(sig.signal) { "" } else { "," }
					)?;
				}
			}

			self.end_indent();
			emit!(self, ")")?;
		}

		if last_interface_id.is_some() {
			emitln!(self, "(")?;
			self.begin_indent();
			// emitln!(self, "/* interface */")?;

			for sig in m.interface() {
				if !matches!(
					self.design.get_signal(sig.signal).unwrap().sensitivity,
					SignalSensitivity::Generic
				) {
					let sig_def_str = self.module_interface_definition(m.clone(), sig)?;
					emitln!(
						self,
						"{}{}",
						sig_def_str,
						if last_interface_id == Some(sig.signal) { "" } else { "," }
					)?;
				}
			}

			self.end_indent();
			emit!(self, ")")?;
		}
		emitln!(self, ";")?;

		self.begin_indent();

		// Emit proxies for outputs
		for output_sig_id in output_signal_ids {
			let sig = self.design.get_signal(output_sig_id).unwrap();
			assert!(!sig.is_generic());
			let proxy_name = format!("{}_proxy$", sig.name());
			self.signal_subs.insert(output_sig_id, proxy_name.clone());
			let proxy_def_str = self.format_signal_declaration_impl(&proxy_name, &sig.class, &sig.dimensions)?;
			emitln!(self, "{};", proxy_def_str)?;
			emitln!(self, "assign {} = {};", sig.name(), proxy_name)?;
		}

		self.emit_scope(m.scope().id(), true, false, interface_signal_ids)?;
		self.end_indent();

		emitln!(self, "endmodule")?;
		Ok(())
	}
}
