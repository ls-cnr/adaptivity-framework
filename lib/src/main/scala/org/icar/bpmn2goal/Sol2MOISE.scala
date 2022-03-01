package org.icar.bpmn2goal

import org.icar.pmr_solver.best_first_planner._
import org.icar.rete.RETEBuilder
import org.icar.sublevel.{HL2Raw_Map, RawGoal, RawGoalModelSupervisor, RawState}
import org.icar.symbolic._

import java.io.FileInputStream
import scala.xml.Elem

//case class GoalSPEC(id:String, formula : HL_LTLFormula)
case class ServiceDescr(agent_id:String,abstractCapability: AbstractCapability)


class Sol2MOISE(ontology: Domain, goalmodel:GoalModel, yellowpage:List[ServiceDescr], initial : StateOfWorld) {
	var group_num = 1
	var scheme_num = 1
	var mission_num = 1
	var norm_num = 1
	var dummy_goal_num = 0
	def get_group_id = {
		val id = "group" + group_num
		group_num += 1
		id
	}

	def get_scheme_id = {
		val id = "scheme" + scheme_num
		scheme_num += 1
		id
	}

	def get_mission_id = {
		val id = "mission" + mission_num
		mission_num += 1
		id
	}
	def get_norm_id = {
		val id = "norm" + norm_num
		norm_num += 1
		id
	}

	def get_dummy_goal_id = {
		val id = if (dummy_goal_num==0) "root_goal" else "goal_" + dummy_goal_num
		dummy_goal_num += 1
		id
	}

	var management_mission : List[String] = List.empty
	var mission_map : Map[String,AbstractCapability] = Map.empty

	def moise_spec : Elem = {

		// result of PMR
		def qos(n: RawState): Float = 0

		val sys_action = (for (sd <- yellowpage) yield sd.abstractCapability).toArray
		val available = AvailableActions(sys_action, Array.empty)

		/* the solver */
		val the_problem = Problem(initial, goalmodel, available)

		val map = new HL2Raw_Map(ontology)

		val I = RawState.factory(map.state_of_world(the_problem.I.statements), ontology.axioms, map)
		val rete = RETEBuilder.factory(ontology.axioms, map, I)
		rete.execute

		val specifications: Array[RawGoal] = for (g<-the_problem.goal_model.goals) yield map.goal_spec(g)
		val init_supervisor = RawGoalModelSupervisor.factory(rete.state,specifications)

		//val specifications: Array[RawLTL] = for (g <- the_problem.goal_model.goals) yield map.ltl_formula(g)
		//val init_supervisor = RawGoalModelSupervisor.factory(rete.state, specifications)

		val available_actions = (for (a <- the_problem.actions.sys_action) yield map.system_action(a)).flatten
		val available_perturb = (for (a <- the_problem.actions.env_action) yield map.system_action(a)).flatten

		val solver = new Solver(rete, init_supervisor, available_actions, available_perturb, qos)


		//val solver = Solver(the_problem,ontology,qos)
		val its = solver.iterate_until_termination(SolverConfiguration(IterationTermination(20), SolutionConfiguration(allow_self_loop = false, allow_cap_multiple_instance = true, allow_loop = true, allow_parallel_action = true)))

		if (solver.opt_solution_set.isDefined) {
			println("**Planning**")
			println("Number of iterations: " + its)

			println("**WTS**")
			println("Number of generated WTS: " + solver.opt_solution_set.get.wts_list.size)
			println("Number of full WTS: " + solver.opt_solution_set.get.full_wts.size)
			println("Number of partial WTS: " + solver.opt_solution_set.get.partial_wts.size)
			//println( solver.opt_solution_set.get.all_solutions_to_graphviz( _.toString ) )
			//println( solver.opt_solution_set.get.all_solutions_to_graphviz( map.pretty_string ) )

			val wts_list = solver.opt_solution_set.get.wts_list


			println("**Solutions**")
			val solutions: List[Solution] = for (g <- wts_list) yield WTSGraph.WTStoSolution(g, initial)
			//val first_sol: Solution = solutions.head

			//println(first_sol.to_graphviz())

			//val sol_trees = for (s<-solutions) yield new SolutionPattern(s)
			//val trees = for (s<-sol_trees) yield s.root_pattern._1

			//println("**Organization**")
			// MOISE TEMPLATE

			apply_rule__main_template(solutions)

		} else {
			<organisational-specification></organisational-specification>
		}
	}

	def apply_rule__main_template(solutions: List[Solution]): Elem = {
		management_mission = List.empty
		mission_map = Map.empty

		<organisational-specification>
			<structural-specification>
				<role-definitions>
					<role id="manager"/>
					<role id="worker"/>{yellowpage.map(i => <role id={i.abstractCapability.id + "_role"}>
					<extends role="worker"/>
				</role>)}
				</role-definitions>
				<group-specification>
					<roles>
						<role id="manager" min="1" max="1"></role>
					</roles>
					<subgroups>
						{solutions.map(sol => apply_rule__group_specification(sol))}
					</subgroups>
					<formation-constraints>
						<cardinality object="role" id="manager"/>
						<compatibility from="worker" to="worker" extends-subgroups="false"></compatibility>
					</formation-constraints>
				</group-specification>
			</structural-specification>

			<functional-specification>
				{solutions.map(s => apply_rule__scheme(s))}
			</functional-specification>

			<normative-specification>
				{management_mission.map( m => apply_rule__management_norm(m))++mission_map.map(m => apply_rule__mission_norm(m)) }
			</normative-specification>
		</organisational-specification>

	}

	def apply_rule__group_specification(sol: Solution): Elem = {
		<group-specification id={get_group_id} min="0">
			{apply_rule__group_roles(sol) ++ apply_rule__group_links(sol)}
		</group-specification>
	}
	def apply_rule__group_roles(s: Solution): Elem = {
		<roles>
			<role id="manager" min="1" max="1"/>{s.wfitems.filter(i => i.isInstanceOf[SolutionTask]).map(t =>
				<role id={t.asInstanceOf[SolutionTask].grounding.capability.id + "_role"} min="1" max="1"/>
		)}
		</roles>
	}
	def apply_rule__group_links(s: Solution): Elem = {
		<links>
			<link from="manager" to="worker" type="authority" extends-subgroups="false" bi-dir="false" scope="intra-group"/>
			<link from="worker" to="manager" type="acquaintance" extends-subgroups="false" bi-dir="false" scope="intra-group"/>
		</links>
	}

	def apply_rule__scheme(s: Solution): Elem = {
		dummy_goal_num = 0
		val sol_tree = new SolutionPattern(s)
		val opt_tree: Option[WorkflowPattern] = sol_tree.root_pattern._1

		if (opt_tree.isDefined) {
			val tree : WorkflowPattern = opt_tree.get

			val capabilities = for (i<-s.wfitems if i.isInstanceOf[SolutionTask]) yield i.asInstanceOf[SolutionTask].grounding.capability

			val scheme_id = get_scheme_id
			<scheme id={scheme_id}>
				{apply_rule__plan(tree)++apply_rule__management_mission(scheme_id)++capabilities.map(c=>apply_rule__mission(c))}
			</scheme>
		} else {
			<scheme id={get_scheme_id}>
			</scheme>
		}
	}

	def apply_rule__plan(pattern: WorkflowPattern) : Elem = {
		pattern match {
			case SequencePattern(children) =>
				<goal id={get_dummy_goal_id}>
					<plan operator="sequence">
						{children.map(c => apply_rule__plan(c))}
					</plan>
				</goal>

			case ChoicePattern(children) =>
				<goal id={get_dummy_goal_id}>
					<plan operator="choice">
						{children.map(c => apply_rule__plan(c))}
					</plan>
				</goal>

			case LoopPattern(before_check, after_check, exit) =>
				<goal id={get_dummy_goal_id}>
					<plan operator="sequence" type="maintain">
						{before_check.map(c => apply_rule__plan(c))}
						<goal id={"check_exit_"+get_dummy_goal_id}></goal>
						{after_check.map(c => apply_rule__plan(c))}
					</plan>
				</goal>


			case ActivityPattern(task) =>
				val id = task.grounding.capability.id
				<goal id={id}></goal>

			case _ =>
				<goal id={"unsup_"+get_dummy_goal_id}></goal>
		}
	}

	def apply_rule__mission(cap: AbstractCapability) : Elem = {
		val miss_id = get_mission_id
		mission_map += (miss_id -> cap)
		<mission id={miss_id} min="1" max="1">
			<goal id={cap.id}></goal>
		</mission>
	}

	def apply_rule__management_mission(scheme_id:String) : Elem = {
		val mng_id = "management_"+scheme_id
		management_mission = mng_id :: management_mission
		<mission id={"management_"+scheme_id} min="1" max="1">
			<goal id="root_goal"></goal>
		</mission>
	}

	def apply_rule__management_norm(m: String): Elem = {
		<norm id={get_norm_id} type="permission" role="manager" mission={m}/>
	}

	def apply_rule__mission_norm(m: (String, AbstractCapability)): Elem = {
		<norm id={get_norm_id} type="obligation" role={"role_"+m._2.id} mission={m._1}/>
	}



}




object Sol2MOISETest extends App {

	//val file = "data/process/peer_review_process.bpmn"
	val file = "data/process/simplified_email_voting.bpmn"
	val is = new FileInputStream(file)
	val parser = new bpmn_parser(is)
	val goals = parser.goals_from_InputStream
	val initial = parser.initial_state

	val yellowpage: List[ServiceDescr] = load_dummy_yellow_page()
	val my_domain = load_dummy_domain()
	val tool = new Sol2MOISE(my_domain, GoalModel(goals), yellowpage, initial)
	val p = new scala.xml.PrettyPrinter(160, 4)

	println(p.format(tool.moise_spec))


	private def load_dummy_yellow_page() : List[ServiceDescr] = {
		/* capability*/
		val s1 = ServiceDescr("a1",AbstractCapability(
			id = "prepare",
			params = List(),//DomainVariable("TYPE","doc_type")

			pre = Predicate("received", List(AtomTerm("issue_vote_list"))),

			post = Predicate("initial", List(AtomTerm("issue_list"))),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
				AddOperator(Predicate("initial", List(AtomTerm("issue_list")))),
				RmvOperator(Predicate("received", List(AtomTerm("issue_vote_list"))))
			))),

			future = List.empty
		))

		val s2 = ServiceDescr("a1",AbstractCapability(
			id = "discussion",
			params = List(),//DomainVariable("TYPE","doc_type")

			pre = Disjunction( List(
				Predicate("initial", List(AtomTerm("issue_list"))),
				Conjunction( List(
					Predicate("sent", List(AtomTerm("vote_results"))),
					Predicate("final", List(AtomTerm("issue_list"))),
					Negation(Predicate("agreement", List(AtomTerm("issue_votes"))) )
				))
			)),

			post = Predicate("ready", List(AtomTerm("issue_list"))),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					AddOperator(Predicate("ready", List(AtomTerm("issue_list")))),
					RmvOperator(Predicate("sent", List(AtomTerm("vote_results")))),
					RmvOperator(Predicate("initial", List(AtomTerm("issue_list")))),
					RmvOperator(Predicate("final", List(AtomTerm("issue_list"))))
				))),

			future = List.empty
		))

		val s3 = ServiceDescr("a2",AbstractCapability(
			id = "voting",
			params = List(),//DomainVariable("TYPE","doc_type")

			pre = Predicate("ready", List(AtomTerm("issue_list"))),

			post = Predicate("final", List(AtomTerm("issue_list"))),

			effects = Array(
				EvolutionGrounding("agree",Array[EvoOperator](
					AddOperator(Predicate("in_vote", List(AtomTerm("issue_list")))),
					AddOperator(Predicate("sent", List(AtomTerm("vote_announcement")))),
					AddOperator(Predicate("received", List(AtomTerm("vote")))),
					AddOperator(Predicate("sent", List(AtomTerm("vote_results")))),
					AddOperator(Predicate("final", List(AtomTerm("issue_list")))),
					AddOperator(Predicate("available", List(AtomTerm("issue_votes")))),
					AddOperator(Predicate("agreement", List(AtomTerm("issue_votes")))),

					RmvOperator(Predicate("ready", List(AtomTerm("issue_list"))))
				)),

				EvolutionGrounding("disagree",Array[EvoOperator](
					AddOperator(Predicate("in_vote", List(AtomTerm("issue_list")))),
					AddOperator(Predicate("sent", List(AtomTerm("vote_announcement")))),
					AddOperator(Predicate("received", List(AtomTerm("vote")))),
					AddOperator(Predicate("sent", List(AtomTerm("vote_results")))),
					AddOperator(Predicate("final", List(AtomTerm("issue_list")))),
					AddOperator(Predicate("available", List(AtomTerm("issue_votes")))),

					RmvOperator(Predicate("agreement", List(AtomTerm("issue_votes")))),
					RmvOperator(Predicate("ready", List(AtomTerm("issue_list"))))
				))
			),

			future = List.empty
		))

		List(s1,s2,s3)
	}

	private def load_dummy_domain() : Domain = {
		val dom_types : Array[DomainType] = Array(
			StringEnum_DomainType("doc_type",Array("issue_list","issue_votes")),
			StringEnum_DomainType("msg_type",Array("issue_vote_list","vote_results","vote_announcement","vote","paper_submission"))
		)

		val preds : Array[DomainPredicate] = Array(
			DomainPredicate("initial",List(DomainVariable("TYPE","doc_type"))),
			DomainPredicate("ready",List(DomainVariable("TYPE","doc_type"))),
			DomainPredicate("in_vote",List(DomainVariable("TYPE","doc_type"))),
			DomainPredicate("available",List(DomainVariable("TYPE","doc_type"))),
			DomainPredicate("final",List(DomainVariable("TYPE","doc_type"))),
			DomainPredicate("agreement",List(DomainVariable("TYPE","doc_type"))),
			DomainPredicate("unagreement",List(DomainVariable("TYPE","doc_type"))),
			DomainPredicate("received",List(DomainVariable("TYPE","msg_type"))),
			DomainPredicate("sent",List(DomainVariable("TYPE","msg_type"))),
		)

		Domain("workflow",preds,dom_types,Array.empty)
	}

}

