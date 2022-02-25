package org.icar.rete

import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic.{Axiom, ConstantTerm, NegateCondition, Predicate, PredicateCondition, Rule, Term, VariableTerm}


object RETEBuilder {
	case class RETENodeDefinition(node:RETENode, terms: List[Term])
	case class BetaJoinDefinition(left:Predicate, left_pos:Int, right:Predicate, right_pos:Int)

	def factory(axioms : Array[Axiom], map:HL2Raw_Map, wi:RawState) : RETE = {
		val rete = new RETE(wi)
		var alpha_counter:Int=0
		var beta_counter:Int=0
		var p_counter:Int=0
		var global_alpha_list : List[AlphaNode] = List.empty
		var global_beta_list : List[BetaJoinNode] = List.empty
		var priority:Int = 0


		/* utility functions */
		def create_alpha(p: Predicate, negated: Boolean) : RETENodeDefinition = {
			/* init the alpha / alpha memory */
			val init_list = map.all_matching_vars(p)
			val alpha = new AlphaNode(rete,alpha_counter,init_list,map,negated)

			val alpha_memory = new AlphaMemory(Map.empty)
			for (v<-init_list)
				alpha_memory.setToken(v.index,wi.bit_descr(v.index))

			rete.memory.alpha_memory += (alpha_counter -> alpha_memory)
			rete.root.subnodes = alpha :: rete.root.subnodes

			global_alpha_list = alpha :: global_alpha_list

			alpha_counter += 1

			RETENodeDefinition(alpha,p.terms)
		}
		def create_betas(node_definitions: List[RETENodeDefinition]): RETENodeDefinition = {
			if (node_definitions.size==1)
				node_definitions.head
			else {
				/* create beta */
				val first : RETENodeDefinition = node_definitions.head
				val second : RETENodeDefinition = node_definitions.tail.head
				val join : RETENodeDefinition = merge_definitions(first,second)

				/* beta is child of its parent nodes */
				first.node.subnodes = join.node :: first.node.subnodes
				second.node.subnodes = join.node :: second.node.subnodes

				create_betas(join::node_definitions.tail.tail)
			}
		}
		def extract_joins(n1: RETENodeDefinition, n2: RETENodeDefinition) : List[TermJoin] = {
			var joins : List[TermJoin] = List.empty
			for (i1<-0 until n1.terms.length)
				for (i2<-0 until n2.terms.length)
					if (n1.terms(i1)==n2.terms(i2))
						if (n1.terms(i1).isInstanceOf[VariableTerm])
							joins = TermJoin(i1,i2) :: joins
			joins
		}
		def merge_definitions(a: RETENodeDefinition, b: RETENodeDefinition): RETENodeDefinition = {
			val join_couples = extract_joins(a,b)

			val beta = new BetaJoinNode(rete,beta_counter,a.node,b.node,join_couples)
			val beta_memory = new BetaMemory
			rete.memory.beta_memory += (beta_counter->beta_memory)
			beta_counter += 1

			global_beta_list = beta :: global_beta_list

			RETENodeDefinition(beta,a.terms:::b.terms)
		}
		/* end utility functions */


		/* function starts here */
		for (rule<-axioms) {
			rule match {
				case Rule(consequent,antecedent) =>
					var alpha_definitions : List[RETENodeDefinition] = List.empty

					for (ante_term <- antecedent.terms) {
						ante_term match {
							case NegateCondition(p) =>
								val alphanode = create_alpha(p,true)
								alpha_definitions = alphanode :: alpha_definitions

							case PredicateCondition(p) =>
								val alphanode = create_alpha(p,false)
								alpha_definitions = alphanode :: alpha_definitions

							case _ =>
						}
					}

					val last_node : RETENodeDefinition = create_betas(alpha_definitions)

					//generate the list of Match and Fix for the p-node
					val beta_terms = last_node.terms
					var pn_inf_terms : List[InferenceTerms] = List.empty
					for (cons_term <- consequent.terms)
						cons_term match {
							case term: ConstantTerm => pn_inf_terms = Fix(term) :: pn_inf_terms
							case VariableTerm(name) => pn_inf_terms = Match( search_first_occurrence(beta_terms,name)) :: pn_inf_terms
						}

					// create the pnode
					val rule = new PNode(rete,p_counter,priority,consequent.functional,pn_inf_terms.reverse,map)
					val pmemory = new PMemory()
					rete.memory.p_memory += (p_counter->pmemory)
					p_counter += 1

					last_node.node.subnodes = List(rule)

				case _ =>

			}
			priority += 1
		}

		rete.start(wi)
		rete
	}


	private def search_first_occurrence(terms: List[Term], var_name: String) : Int = {
		var occ = -1

		var index = 0
		for (t<-terms if occ == -1){
			t match {
				case term: ConstantTerm =>
				case VariableTerm(name) =>
					if (name == var_name) occ = index
			}
			index += 1
		}

		occ
	}

}
