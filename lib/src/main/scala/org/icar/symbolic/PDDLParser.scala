package org.icar.symbolic

import scala.util.parsing.combinator.JavaTokenParsers

class PDDLParser extends JavaTokenParsers {

	def domain = "("~"define"~domain_name~opt(types_def)~rep(structure_def)~")"

	def domain_name = "("~"define"~stringLiteral~")"

	def types_def = "("~":types"~typed_list~")"
	def typed_list = rep(type_entry)
	def type_entry = rep(ident)~opt("-"~ident)

	def variable_list = rep(variable_entry)
	def variable_entry = rep("?"~ident)~opt("-"~ident)

	def structure_def = action_def | axiom_def

	def action_def = "("~":action"~ident~":parameters"~"("~variable_list~")"~action_body
	def action_body = ":vars"~"("~variable_list~")" ~ precond_def ~ effect_def


	def precond_def = ident
	def effect_def = ident

	def axiom_def = ident

}
