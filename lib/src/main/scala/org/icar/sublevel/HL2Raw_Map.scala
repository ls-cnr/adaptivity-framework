package org.icar.sublevel

import org.icar.symbolic._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/* AnonymousTerm not yet supported */


/*
 * direct: predicate1 -> var_index (map)
 * inverse: var_index -> predicate1 (arraybuffer)
 */
class HL2Raw_Map(domain: Domain) {
	var direct : Map[GroundPredicate,Int] = Map.empty
	var inverse : ArrayBuffer[GroundPredicate] = ArrayBuffer()
	private var var_counter : Int = 0

	init

	def init = {
		for (p<-domain.space) {

			var args = p.args
			combine(p,args,Map.empty)

		}

		def combine(f:DomainPredicate, to_assign:List[DomainArgument], assigned:Map[DomainArgument,ConstantTerm]):Unit = {
			if (to_assign.isEmpty) {
				register(f,assigned)
			} else {
				val arg : DomainArgument = to_assign.head
				for (value <- arg.range(domain.types)){
					combine(f,to_assign.tail,assigned+(arg->value))
				}
			}
		}

		def register(f: DomainPredicate, assigned: Map[DomainArgument, ConstantTerm]): Unit = {
			var ground_args : List[ConstantTerm] = List.empty
			for (a <- f.args)
				ground_args = assigned(a) :: ground_args

			val p = GroundPredicate(f.functor,ground_args.reverse)

			direct += (p->var_counter)
			inverse += p


			var_counter += 1
		}

	}

	def state_of_world(statements : List[GroundPredicate]):Array[Boolean] = {
		val array : Array[Boolean] = Array.fill(var_counter){false}

		for (s<-statements) {
			val index = direct(s)
			array(index)=true
		}

		array
	}
	def inverse_state_of_world(state: Array[Boolean]) : List[GroundPredicate] = {
		(for (index<-0 until state.size if state(index)==true) yield inverse(index)) toList
	}

	def predicate_formula(f:HL_PredicateFormula) : RawPredicate = {
		def exist_quantifier(p : Predicate, pos : Int, assignments : Map[VariableTerm,ConstantTerm]) : RawPredicate = {
			if (pos == p.terms.size) {
				val pred = p.to_ground(assignments)
				RawVar(direct(pred))

			} else {
				var x_list : List[RawPredicate] = List.empty
				val arg = p.terms(pos)
				arg match {
					case a: VariableTerm =>
						val t : DomainArgument = domain.get_predicate_arg_type(p.functional,pos)
						if (assignments.contains(a)) {
							if (t.range(domain.types).contains(a))
								exist_quantifier(p, pos + 1, assignments)
							else
								RawFF()
						} else {

							for (value <- t.range(domain.types)){
								x_list = exist_quantifier(p,pos+1,assignments+(a->value)) :: x_list
							}
							combine_in_or(x_list.filter(_!=RawFF()))
						}
					case a: AtomTerm => exist_quantifier(p,pos+1,assignments)
					case a: NumeralTerm => exist_quantifier(p,pos+1,assignments)
					case a: StringTerm => exist_quantifier(p,pos+1,assignments)

					case _ => RawFF()
				}
			}
		}

		def foreach_quantifier(p : Predicate, pos : Int, assignments : Map[VariableTerm,ConstantTerm]) : RawPredicate = {
			if (pos == p.terms.size) {
				val pred = p.to_ground(assignments)
				RawVar(direct(pred))

			} else {
				var x_list : List[RawPredicate] = List.empty
				val arg = p.terms(pos)
				arg match {
					case a: VariableTerm =>
						val t : DomainArgument = domain.get_predicate_arg_type(p.functional,pos)
						if (assignments.contains(a)) {
							if (t.range(domain.types).contains(a))
								foreach_quantifier(p, pos + 1, assignments)
							else
								RawTT()
						} else {
							for (value <- t.range(domain.types)) {
								x_list = foreach_quantifier(p, pos + 1, assignments + (a -> value)) :: x_list
							}
							combine_in_and(x_list.filter(_ != RawTT()))
						}
					case a: AtomTerm => foreach_quantifier(p,pos+1,assignments)
					case a: NumeralTerm => foreach_quantifier(p,pos+1,assignments)
					case a: StringTerm => foreach_quantifier(p,pos+1,assignments)

					case _ => RawFF()
				}
			}
		}

		def combine_in_or(predicate_formulas: List[RawPredicate]) : RawPredicate = {
			if (predicate_formulas.size==0)
				RawFF()
			else if (predicate_formulas.size==1)
				predicate_formulas.head
			else
				RawDisj(predicate_formulas.head,combine_in_or(predicate_formulas.tail))
		}
		def combine_in_and(predicate_formulas: List[RawPredicate]) : RawPredicate = {
			if (predicate_formulas.size==0)
				RawFF()
			else if (predicate_formulas.size==1)
				predicate_formulas.head
			else
				RawConj(predicate_formulas.head,combine_in_and(predicate_formulas.tail))
		}

		/* CONVERTING PREDICATE FORMULA */
		f match {
			case p:GroundPredicate => RawVar(direct(p))
			case True() => RawTT()
			case False() => RawFF()
			case Negation(sf) => RawNeg[RawPredicate](predicate_formula(sf.asInstanceOf[HL_PredicateFormula]))
			case Conjunction(terms) =>
				val pf_terms = for (t<-terms) yield t.asInstanceOf[HL_PredicateFormula]
				if (pf_terms.size==2)
					RawConj(predicate_formula(pf_terms.head),predicate_formula(pf_terms.tail.head))
				else if (terms.size==1)
					predicate_formula(pf_terms.head)
				else
					RawConj(predicate_formula(pf_terms.head), predicate_formula(Conjunction(pf_terms.tail)))
			case Disjunction(terms) =>
				val pf_terms = for (t<-terms) yield t.asInstanceOf[HL_PredicateFormula]
				if (pf_terms.size==2)
					RawDisj(predicate_formula(pf_terms.head),predicate_formula(pf_terms.tail.head))
				else if (pf_terms.size==1)
					predicate_formula(pf_terms.head)
				else
					RawDisj(predicate_formula(pf_terms.head), predicate_formula(Disjunction(pf_terms.tail)))
			case ExistQuantifier(vars,literal) =>
				literal match {
					case p:Predicate=> exist_quantifier(p,0,Map.empty)
					case Conjunction(terms) =>
						val pf_terms = for (t<-terms) yield ExistQuantifier(vars,t.asInstanceOf[HL_PredicateFormula])
						predicate_formula(Conjunction(pf_terms))
					case Disjunction(terms) =>
						val pf_terms = for (t<-terms) yield ExistQuantifier(vars,t.asInstanceOf[HL_PredicateFormula])
						predicate_formula(Disjunction(pf_terms))
					case Negation(op) =>
						predicate_formula(Negation(ExistQuantifier(vars,op.asInstanceOf[HL_PredicateFormula])))
					case _ => RawFF()
				}

			case UnivQuantifier(vars,literal) =>
				literal match {
					case p:Predicate => foreach_quantifier(p, 0, Map.empty)
					case Conjunction(terms) =>
						val pf_terms = for (t <- terms) yield UnivQuantifier(vars, t.asInstanceOf[HL_PredicateFormula])
						predicate_formula(Conjunction(pf_terms))
					case Disjunction(terms) =>
						val pf_terms = for (t <- terms) yield UnivQuantifier(vars, t.asInstanceOf[HL_PredicateFormula])
						predicate_formula(Disjunction(pf_terms))
					case Negation(op) =>
						predicate_formula(Negation(UnivQuantifier(vars, op.asInstanceOf[HL_PredicateFormula])))
					case _ => RawFF()
				}
			case _ => RawFF()
		}

	}

	def all_matching_vars(p:Predicate) : List[RawVar] = {

		def predicate_quantifier(p : Predicate, pos : Int, assignments : Map[VariableTerm,ConstantTerm]) : List[RawVar] = {
			var x_list : List[RawVar] = List.empty

			if (pos == p.terms.size) {
				val pred = p.to_ground(assignments)
				x_list = List(RawVar(direct(pred)))

			} else {
				val arg = p.terms(pos)
				arg match {
					case a: VariableTerm =>
						val t : DomainArgument = domain.get_predicate_arg_type(p.functional,pos)
						if (assignments.contains(a)) {
							if (t.range(domain.types).contains(a))
								x_list = predicate_quantifier(p, pos + 1, assignments) ::: x_list
						} else {
							for (value <- t.range(domain.types)) {
								x_list = predicate_quantifier(p, pos + 1, assignments + (a -> value)) ::: x_list
							}
						}
					case a: AtomTerm => x_list = predicate_quantifier(p,pos+1,assignments) ::: x_list
					case a: IntegerTerm => x_list = predicate_quantifier(p,pos+1,assignments) ::: x_list
					case a: NumeralTerm => x_list = predicate_quantifier(p,pos+1,assignments) ::: x_list
					case a: StringTerm => x_list = predicate_quantifier(p,pos+1,assignments) ::: x_list

					case _ =>
				}

			}

			x_list
		}

		predicate_quantifier(p,0,Map.empty)
	}

	def goal_spec(g: GoalSPEC) : RawGoal = RawGoal(g.id,predicate_formula(g.pre),ltl_formula(g.post))

	def ltl_formula(f:HL_LTLFormula) : RawLTL = {
		f match {
			case p:GroundPredicate => RawVar(direct(p))
			case True() => RawTT()
			case False() => RawFF()
			case Implication(l,r) => RawImpl[RawLTL](ltl_formula(l.asInstanceOf[HL_LTLFormula]),ltl_formula(r.asInstanceOf[HL_LTLFormula]))
			case BiImplication(l,r) => RawIff[RawLTL](ltl_formula(l.asInstanceOf[HL_LTLFormula]),ltl_formula(r.asInstanceOf[HL_LTLFormula]))
			case Negation(op) => RawNeg[RawLTL](ltl_formula(op.asInstanceOf[HL_LTLFormula]))

			case Conjunction(sf) =>
				if (sf.length==1)
					ltl_formula(sf.head.asInstanceOf[HL_LTLFormula])
				else {
					val ltl_sf = for (s<-sf) yield s.asInstanceOf[HL_LTLFormula]
					RawConj[RawLTL](ltl_formula(ltl_sf.head), ltl_formula(Conjunction[HL_LTLFormula](ltl_sf.tail)) )
				}
			case Disjunction(sf) =>
				if (sf.length==1)
					ltl_formula(sf.head.asInstanceOf[HL_LTLFormula])
				else {
					val ltl_sf = for (s<-sf) yield s.asInstanceOf[HL_LTLFormula]
					RawDisj[RawLTL](ltl_formula(ltl_sf.head), ltl_formula(Disjunction[HL_LTLFormula](ltl_sf.tail)) )
				}

			case Globally(op) => RawGlobally(ltl_formula(op))
			case Finally(op) => RawFinally(ltl_formula(op))
			case Next(op) => RawNext(ltl_formula(op))
			case Until(l,r) => RawUntil(ltl_formula(l),ltl_formula(r))
			case Release(l,r) => RawRelease(ltl_formula(l),ltl_formula(r))

			case _ => RawFF()
		}
	}

	def grounding_scenario(name : String, probability : Float, evo : Array[EvoOperator], assigned:Map[String,ConstantTerm]=Map.empty) : RawEvolution = {
		var raw_op_list : List[RawEvoOperator] = List.empty
		for (op <- evo)
			op match {
				//case Deprec_AddEvoOperator(pred) => raw_op_list = RawAdd(RawVar(direct(pred))) :: raw_op_list
				//case Deprec_RemoveEvoOperator(pred) => raw_op_list = RawRem(RawVar(direct(pred))) :: raw_op_list
				//case Deprec_RemoveAllEvoOperator(pred_class) =>
					//for (i <- 0 until inverse.size if inverse(i).functional==pred_class)
						//raw_op_list = RawRem(RawVar(i)) :: raw_op_list
				case AddOperator(p) =>
					val p1 = HL_PredicateFormula.pred_substitution(p,assigned)
					val opt_p2 = p1.get_grounded
					if (opt_p2.isDefined) {
						val p2 = opt_p2.get
						raw_op_list = RawAdd(RawVar(direct(p2))) :: raw_op_list
					}

				case RmvOperator(p) =>
					val p1 = HL_PredicateFormula.pred_substitution(p,assigned)
					val opt_p2 = p1.get_grounded
					if (opt_p2.isDefined) {
						val p2 = opt_p2.get
						raw_op_list = RawRem(RawVar(direct(p2))) :: raw_op_list
					}

				case _ =>
			}
		RawEvolution(name,probability,raw_op_list.toArray)
	}


	def system_action(sys_action : AbstractCapability) : List[RawAction] = {

		def create_instances(to_assign:List[DomainArgument], assigned:Map[String,ConstantTerm]):List[RawAction] = {
			if (to_assign.isEmpty) {

				var unique_id : String = sys_action.id+"("
				var first = true
				for (v<-assigned.keys)
					if (first){
						unique_id+=v+"="+assigned(v)
						first = false
					} else {
						unique_id+=","+v+"="+assigned(v)
					}

				unique_id += ")"

				val real_pre = Conjunction(List(sys_action.pre,Negation(sys_action.post)))
				val raw_precond = predicate_formula(HL_PredicateFormula.substitution(real_pre,assigned))
				val raw_effects = for (e<-sys_action.effects) yield grounding_scenario(e.name,1,e.evo,assigned)
				val raw_invariants = for (i<-sys_action.future) yield predicate_formula(HL_PredicateFormula.substitution(i,assigned))

				List(RawAction(
					unique_id,
					raw_precond,
					raw_effects,
					raw_invariants,
					CapabilityGrounding(sys_action,assigned)
				))

			} else {
				var to_instantiate : List[RawAction] = List.empty
				val arg : DomainArgument = to_assign.head
				if (arg.isInstanceOf[DomainVariable]) {
					for (value <- arg.range(domain.types)) {
						val variable = arg.asInstanceOf[DomainVariable]
						to_instantiate = create_instances(to_assign.tail, assigned + (variable.name -> value)) ::: to_instantiate
					}
				} else {
						to_instantiate = create_instances(to_assign.tail,assigned) ::: to_instantiate
				}

				to_instantiate
			}
		}

		create_instances(sys_action.params,Map.empty)
	}

	def pretty_string(state:RawState):String = {
		var s = "["
		for (index<-0 until state.bit_descr.length)
			if (state.bit_descr(index))
				s+=inverse(index)
		s+"]"
	}

/*
	def environment_action(env_action : EnvironmentAction) : RawAction = {
		val raw_invariants = for (i<-env_action.future) yield predicate_formula(i)
		symbolic_level.RawAction(
			env_action.id,
			predicate_formula(env_action.pre),
			for (e<-env_action.effects) yield grounding_scenario(e.name,e.probability,e.evo),
			raw_invariants
		)
	}
*/

	//def axiom(ass:Assumption)
}
