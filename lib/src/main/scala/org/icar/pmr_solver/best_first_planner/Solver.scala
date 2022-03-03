package org.icar.pmr_solver.best_first_planner

import org.icar.pmr_solver.{SolverConfiguration, TerminationDescription}
import org.icar.rete.{RETE, RETEBuilder, RETEMemory}
import org.icar.sublevel._
import org.icar.symbolic.{AvailableActions, Domain, GoalModel, GoalState, Problem}

/******* NOTES AND COMMENTS ********/
// Luca: to implement:
// 1. TODO persistent temporal properties: a goal like Globally(...)... it will be never be satisfied!!! this is a problem for the wts completeness
// 2. TODO invariants associated to the evolution scenario haven't been tested until now
// 3. TODO violation of goals (Globally, Until...) can produce violations that will discard the solution

/******* SYMBOLIC SOLVER ********/
class Solver(rete:RETE, specifications:Array[RawGoal], val available_actions:Array[RawAction], val available_perturb:Array[RawAction], qos : RawState => Float, conf : SolverConfiguration) {
	var spec_map : Map[String,RawGoal] = init_goal_map(specifications)
	var solution_set = new SolutionSet(rete.memory, qos, specifications)
	var num_nodes : Int = 0
	var elapsed : Long= 0

	def init_goal_map(specifications : Array[RawGoal]) : Map[String,RawGoal] = {
		var goalmap : Map[String,RawGoal] = Map.empty
		for (s<-specifications)
			goalmap += (s.id -> s)
		goalmap
	}

	/* solver loop with termination conditions */
	def iterate_until_termination() : Int = {
		val start_timestamp: Long = System.currentTimeMillis
		num_nodes=0

		var n_iteration = 0

		while (!TerminationDescription.check_termination(conf.termination,start_timestamp,n_iteration,solution_set.full_wts.length)) {
			iteration()

//			println("iteration"+n_iteration)
//			for (wts <- opt_solution_set.get.wts_list)
//				println( wts.to_graphviz(node => node.toString) )
//			println("end of iteration"+n_iteration)
//			println()

			n_iteration += 1
		}

		val end_timestamp: Long = System.currentTimeMillis
		elapsed = end_timestamp-start_timestamp

//		println("WTS list")
//		for (wts <- opt_solution_set.get.wts_list)
//			println( wts.to_graphviz(node => node.toString) )
//		println("end of WTS list")
//		println()

		n_iteration
	}

	/* Main algorithm of the Solver class */
	private def iteration() : Unit = {
		val frontier_node: Option[RawFrontierItem] = solution_set.get_next_node
		if (frontier_node.isDefined) {
			val focus_node_rete_memory: RETEMemory = frontier_node.get.rete_memory
			val focus_node = focus_node_rete_memory.current
			//val focus_node_supervisor: RawGoalSetSupervisor = frontier_node.get.sup

			val actions : Array[RawAction] = applicable_capabilities(focus_node)
			val envs = applicable_perturbations(focus_node)

			var exp_list : List[RawExpansion] = List.empty
			for (a <- actions) {
				exp_list = encode_expansion(focus_node_rete_memory,a,ext = false) :: exp_list
				num_nodes += a.effects.length
			}
			for (e <- envs) {
				exp_list = encode_expansion(focus_node_rete_memory,e,ext = true) :: exp_list
			}

			if (exp_list.nonEmpty) {
				update_global_frontier(exp_list)
				update_solset(focus_node,exp_list)
			}
		}
	}

	private def applicable_capabilities(node : RawState) : Array[RawAction] = {for (a<-available_actions if node.satisfies(a.pre)) yield a}
	private def applicable_perturbations(node : RawState) : Array[RawAction] = {for (a<-available_perturb if node.satisfies(a.pre)) yield a}

	/* extract the node-expansion from an action */
	private def encode_expansion(start_rete_memory : RETEMemory, action : RawAction, ext : Boolean = false) : RawExpansion = {
		val source_node : RawState = start_rete_memory.current
		val trajectory = for (effect <- action.effects) yield calculate_evolution(start_rete_memory,effect)
		RawExpansion(action,source_node,trajectory,action.invariants, ext)
	}

	/* evaluate how evolution impacts the goals */
	private def calculate_evolution(start_rete_memory : RETEMemory, evo_description : RawEvolution) : RawEvoScenario = {
		rete.memory = start_rete_memory.copy
		rete.extend(evo_description)
		val new_state = rete.state
//		val next = supervisor.getNext(new_state)
		val score =qos(new_state)
		val dest = RawFrontierItem(score,rete.memory)
		RawEvoScenario(evo_description.name,evo_description.probability,dest)
	}

	/* given a focus node and a set of expansions, it updates all the corresponding WTS where the exp(s) apply */
	private def update_solset(focus : RawState, exp_list: List[RawExpansion]): Unit = {
		/* check if the expansion is appliable to all the WTS that are not complete */
		var new_wts_list : List[WTSGraph] = List.empty
		for (wts <- solution_set.wts_list) {
			if (!wts.nodes.contains(focus))
				new_wts_list = wts :: new_wts_list
			else if ( wts.isFullSolution )
				new_wts_list = wts :: new_wts_list
			else
				new_wts_list = update_wts(wts,focus, exp_list) ::: new_wts_list
		}

		solution_set.wts_list = new_wts_list
	}

	private def update_global_frontier(exp_list: List[RawExpansion]) : Unit = {
		/* update the frontier */
		for (e<-exp_list)
			for (t<-e.probtrajectory)
				//if (!t.dest.sup.check_exit_node)
				solution_set.global_frontier = t.dest :: solution_set.global_frontier
	}

	private def update_wts(wts:WTSGraph, focus : RawState, exp_list: List[RawExpansion]) : List[WTSGraph] = {
		require(wts.nodes.contains(focus))

		if (wts.wts_labelling.nodes_labelling(focus).is_frontier && exp_list.nonEmpty) {
			var updated_list : List[WTSGraph] = List.empty

			for (exp <- exp_list) {
				val pre_test = check_pre_expansion_validity_test(wts,exp)
				if (pre_test) {
					val frontier_expansion : (Set[RawFrontierItem],Set[RawWTSArc]) = decode_expansion(wts,exp)
					val exp_nodes = frontier_expansion._1
					val new_transitions = frontier_expansion._2

					var new_frontier : Set[RawState] = Set.empty
					exp_nodes.foreach(n=>new_frontier += n.rete_memory.current)

					val post_test = check_post_expansion_validity_test(wts,new_frontier,new_transitions)
					if (post_test) {
						val updated_labelling = update_wts_labelling(wts,focus,exp_nodes,new_transitions,exp.invariants)

						val is_full_solution = check_full_solution(wts.nodes++new_frontier, updated_labelling.nodes_labelling, updated_labelling.goal_sat_list)

						/* FINALLY, the new list of WTS will contain the cloned updated WTS */
						val new_wts = WTSGraph(
							wts.start,                          	//initial node
							wts.nodes++new_frontier,               	//nodes
							wts.transitions++new_transitions,   	//transitions
							updated_labelling,                   	//labelling
							is_full_solution
						)

						updated_list = new_wts :: updated_list
					} else {
						//println("discarded because of POST-TEST")
					}
				} else {
					//println("discarded because of PRE-TEST")
				}

			}
			updated_list

		} else {
			List(wts)
		}

	}

	private def update_wts_labelling(wts: WTSGraph, focus: RawState, new_frontier: Set[RawFrontierItem], new_transitions: Set[RawWTSArc], invariants:List[RawPredicate]) : WTSLabelling = {
		// ** we derive the nodes_labelling from the previous wts  **
		var updated_node_labelling : Map[RawState,StateLabel] = wts.wts_labelling.nodes_labelling // node_labelling del nuovo WTS inizialmente uguale a quello vecchio
		var updated_goal_sat_list : Set[String] = wts.wts_labelling.goal_sat_list

		// 1. FOCUS NODE: setting is_frontier to false and is_terminal to true only if there are no expansions
		val focus_labelling = wts.wts_labelling.nodes_labelling(focus)
		val focus_is_frontier = false
		val focus_is_terminal = new_frontier.isEmpty
		val previous_focus_label : StateLabel = wts.wts_labelling.nodes_labelling(focus)
		val updated_focus_label = previous_focus_label.copy(is_frontier = focus_is_frontier, is_terminal = focus_is_terminal)
		updated_node_labelling += (focus -> updated_focus_label)

		// 2. EXP NODES: update_node_labelling, for each new node, and update the new goal_sat_list
		for (node <- new_frontier) {
			var updated_injected_goals: Map[String, RawPredicate] = focus_labelling.map_of_injected_goals
			var updated_active_goals: Map[String, RawLTLFormula] = focus_labelling.map_of_active_goals
			var updated_goal_trigger_for_list : Set[String] = Set.empty
			var updated_goal_exit_for_list : Set[String] = Set.empty

			// for each injected goal...
			for (gs <- focus_labelling.map_of_injected_goals) {
				val gid = gs._1
				val gpre = gs._2
				if (node.rete_memory.current.satisfies(gpre)) {
					updated_injected_goals = updated_injected_goals - gid		// remove the goal from the list of injected

					val goal_post: RawLTL = spec_map(gid).post
					val sup: RawLTLFormula = RawLTLFormula(true,goal_post)
					val update_sup: RawLTLFormula = sup.next(node.rete_memory.current)
					updated_active_goals += (gid -> update_sup)		// add the goal to the list of active

					updated_goal_trigger_for_list += gid		// mark the node as triggering for the goal
				}
			}

			// for each active goal...
			for (gs <- focus_labelling.map_of_active_goals) {
				val gid = gs._1
				val gsup = gs._2

				val update_sup: RawLTLFormula = gsup.next(node.rete_memory.current)

				// fully satisfaction
				if (update_sup.next_ltl == RawTT() && update_sup.success) {
					updated_active_goals = updated_active_goals - gid		// remove the goal from the list of active
					updated_goal_exit_for_list += gid		// mark the node as exit for the goal
					updated_goal_sat_list += gid				// add the node to the list of addressed for the whole wts

				// goal violation
				} else if (!update_sup.success) {
					// TODO this condition should discard the whole expansion (how? returning a negative quality of solution?)

				// otherwise
				} else {
					updated_active_goals -= gid
					updated_active_goals += (gid -> update_sup)		// update the value of the correspondent supervisor
				}
			}
			val node_is_frontier = true
			val node_is_terminal = false //updated_sup.check_exit_node
			val updated_metric = node.score

			val updated_label = StateLabel(
				updated_injected_goals,
				updated_active_goals,
				updated_goal_trigger_for_list,
				updated_goal_exit_for_list,
				is_terminal = node_is_terminal,
				is_frontier = node_is_frontier,
				updated_metric
			)

			updated_node_labelling = updated_node_labelling + (node.rete_memory.current -> updated_label)
		}

		val is_full_solution = false //check_full_solution_condition(wts)

		// Quality: delegate to specific function
		val updated_quality = 0.0f//calculate_quality_of_solution(wts,updated_frontier,updated_node_labelling,new_nodes,new_transitions,new_perturbations)

		val updated_invariants = wts.wts_labelling.invariants ::: invariants
		WTSLabelling(updated_node_labelling, updated_quality,updated_invariants,is_full_solution,updated_goal_sat_list)
	}

	private def check_node_trigger_for(node: RawFrontierItem) : Set[String] = {
		var triggered_set : Set[String] = Set.empty

		val test_node = node.rete_memory.current
		for (goal<-specifications)
			if ( test_node.satisfies(goal.pre) )
				triggered_set += goal.id

		triggered_set
	}

	/* extract transitions and frontier from an expansion */
	private def decode_expansion(wts: WTSGraph, exp : RawExpansion) : (Set[RawFrontierItem],Set[RawWTSArc]) = {
		var new_frontier : Set[RawFrontierItem] = Set.empty
		var new_transition : Set[RawWTSArc] = Set.empty

		for (evolution_part <- exp.probtrajectory) {
			if (!wts.nodes.contains(evolution_part.dest.rete_memory.current))
				new_frontier += evolution_part.dest

			new_transition = new_transition + RawWTSArc(exp.from, evolution_part.dest.rete_memory.current, evolution_part.probability, exp.due_to, evolution_part.name)
		}

		(new_frontier,new_transition)
	}

	private def check_pre_expansion_validity_test(wts: WTSGraph, exp: RawExpansion) : Boolean = {
		var allowed_by_invariants = true

		for (i<-wts.wts_labelling.invariants)
			for (t<-exp.probtrajectory)
				if (!t.dest.rete_memory.current.satisfies(i))
					allowed_by_invariants=false

		var multiple_cap_test = true
		if(!conf.sol_conf.allow_cap_multiple_instance) {
			for (t<-wts.transitions)
				if (t.action.id == exp.due_to.id)
					multiple_cap_test=false
		}
		allowed_by_invariants && multiple_cap_test
	}

	private def check_post_expansion_validity_test(wts: WTSGraph, new_nodes: Set[RawState], new_transitions: Set[RawWTSArc]):Boolean = {
		var self_loop_test = true
		if (!conf.sol_conf.allow_self_loop)
			for (t<-new_transitions)
				if (t.origin == t.destination)
					self_loop_test=false

		var loop_test = true
		if (!conf.sol_conf.allow_loop)
			for (t<-new_transitions)
				if (wts.nodes.contains(t.destination))
					loop_test=false

		self_loop_test && loop_test
	}

	// check when a wts is complete (no need of further expansions)
	private def check_full_solution(nodes : Set[RawState], nodes_labelling : Map[RawState, StateLabel], goal_sat_list : Set[String]) : Boolean = {
		//check all the goals are fully addressed
		val all_goals_are_sat = goal_sat_list.size == specifications.size

		//check all terminal node are exit nodes without trigger
		var all_terminal_are_exit = true
		var no_terminal_has_trigger = true
		for (node <- nodes) {
			if (nodes_labelling(node).is_frontier) {
				if (!nodes_labelling(node).isExit())
					all_terminal_are_exit = false

				if (nodes_labelling(node).isTrigger())
					no_terminal_has_trigger = false
			}
		}

		all_goals_are_sat && all_terminal_are_exit && no_terminal_has_trigger
	}



}

/******* SOLVER FACTORY ********/
object Solver {
	def apply(problem: Problem,domain: Domain,qos : RawState => Float,conf : SolverConfiguration) : Solver = {
		val map = new HL2Raw_Map(domain)

		val I = RawState.factory(map.state_of_world(problem.I.statements),domain.axioms,map)
		val rete = RETEBuilder.factory(domain.axioms,map,I)
		rete.execute

		val specifications: Array[RawGoal] = for (g<-problem.goal_model.goals) yield map.goal_spec(g)

		val available_actions = (for (a<-problem.actions.sys_action) yield map.system_action(a)).flatten
		val available_perturb = (for (a<-problem.actions.env_action) yield map.system_action(a)).flatten

		new Solver(rete,specifications,available_actions,available_perturb,qos,conf)
	}

	def mixed_factory(goal_model:GoalModel,actions:AvailableActions,I:RawState,domain: Domain,qos : RawState => Float,conf : SolverConfiguration) : Solver = {
		val map = new HL2Raw_Map(domain)
		val rete = RETEBuilder.factory(domain.axioms,map,I)
		rete.execute

		val specifications: Array[RawGoal] = for (g<-goal_model.goals) yield map.goal_spec(g)
//		val init_supervisor = RawGoalSetSupervisor.factory(rete.state,specifications)

		val available_actions = (for (a<-actions.sys_action) yield map.system_action(a)).flatten
		val available_perturb = (for (a<-actions.env_action) yield map.system_action(a)).flatten

		new Solver(rete,specifications,available_actions,available_perturb,qos,conf)
	}
}









