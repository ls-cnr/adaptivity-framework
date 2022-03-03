package org.icar.pmr_solver.best_first_planner

import org.icar.pmr_solver.SolutionConfiguration
import org.icar.rete.RETEMemory
import org.icar.sublevel.{RawGoal, RawGoalSetSupervisor, RawPredicate, RawState}
import org.icar.symbolic.GoalState

/******* SOLUTION SET ********/
// note: global frontier is a general list of nodes to be expended in all the existing WTS.

class SolutionSet(val rete_memory : RETEMemory, qos : RawState => Float, specifications:Array[RawGoal]) {
	val initial_state = rete_memory.current
	val initial_score = qos(initial_state)
	var global_frontier : List[RawFrontierItem] = List( RawFrontierItem(initial_score,rete_memory) )
	var wts_list : List[WTSGraph] = init()

	private def init() : List[WTSGraph] = {
		var all_goal_are_injected : Map[String,RawPredicate] = Map.empty
		for (g<-specifications)
			all_goal_are_injected += (g.id -> g.pre)

		val init_label = StateLabel(
			all_goal_are_injected,
			Map.empty,	// set of active goal is empty
			Set("root"),
			Set.empty,
			is_terminal=false,
			is_frontier = true,
			metric = 0
		)

		val labelling = WTSLabelling (
			Map(initial_state->init_label),
			0,
			List.empty,
			false,
			Set.empty
		)

		List(WTSGraph(
			initial_state,
			Set(initial_state),
			Set.empty,
			labelling,
			is_full_solution = false
		))
	}

	def full_wts : Array[WTSGraph] = {
		wts_list.filter( _.isFullSolution ).toArray
	}

	def partial_wts : Array[WTSGraph] = {
		wts_list.filter( _.isPartialSolution ).toArray
	}

	/*
	 * returns the most promising node to be expanded.

	 * for all the WTS(s),
	 *  explore the wts frontier and
	 *  get the node with highest metric
	 */
	def get_next_node : Option[RawFrontierItem] = {
		var somenode : Option[RawFrontierItem] = None

		if (global_frontier.nonEmpty){
			global_frontier = global_frontier.sorted

			somenode = Some(global_frontier.head)

			global_frontier = global_frontier.tail
		}
		somenode
	}

	def all_solutions_to_graphviz(pretty_string: RawState => String) : String = {
		def node_label(n:RawState, wts_counter: Int, pretty_string: RawState => String) : String = {
			if (n==initial_state)
				pretty_string(initial_state)
			else
				s"${wts_counter}_${pretty_string(n)}"
				//wts_counter+"_"+pretty_string(n)
		}

		var string = "digraph WTS {\n"

		string += "\""+pretty_string(initial_state)+"\" [style=bold,color=yellow];\n"

		var wts_counter = 1

		for (wts <- wts_list) {
			for (n <- wts.nodes if n!=initial_state) {
				string += "\""+node_label(n,wts_counter,pretty_string)+"\""

				if (wts.wts_labelling.nodes_labelling(n).isExit())
					string += "[style=bold,color=green];\n"
				else
					string += "[color=black];\n"
			}


			for (t <- wts.transitions) {
				string += "\""+node_label(t.origin,wts_counter,pretty_string)+"\""
				string += "->"
				string += "\""+node_label(t.destination,wts_counter,pretty_string)+"\""
				string += "[label=\""+t.action.id+"_"+t.scenario_name+"\"];\n"
			}

//			for (t <- wts.perturbations) {
//				string += "\""+node_label(t.origin,wts_counter,pretty_string)+"\""
//				string += "->"
//				string += "\""+node_label(t.destination,wts_counter,pretty_string)+"\""
//				string += "[style=dotted, label=\""+t.action.id+"_"+t.probability+"% \"];\n"
//			}

			wts_counter += 1
		}

		string += "}\n"

		string
	}

}


