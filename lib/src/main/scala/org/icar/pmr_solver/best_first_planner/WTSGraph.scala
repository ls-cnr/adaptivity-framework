package org.icar.pmr_solver.best_first_planner

import org.icar.symbolic.{CapabilityGrounding, EndEvent, GoalState, HL_PredicateFormula, JoinGateway, SequenceFlow, Solution, SolutionTask, SplitGateway, StartEvent, StateOfWorld, True, WorkflowItem}
import org.icar.rete.RETEMemory
import org.icar.sublevel.{RawAction, RawGoal, RawGoalSetSupervisor, RawLTLFormula, RawPredicate, RawState}

import java.io.{File, PrintWriter}


/******* NOTES AND COMMENTS ********/
// Luca: FullSolution - improvement: it is necessary to check for loop safety
// a loop is valid if there is the possibility to leave it and go towards a terminal state



/******* GRAPH/NODE LABELLING ********/
case class WTSLabelling(
	                       nodes_labelling : Map[RawState,StateLabel],  // each node is associated to a set of properties
	                       quality_of_solution : Float,     // global quality of the (partial) solution
	                       invariants : List[RawPredicate],  // conditions that must hold in any new node
	                       full_solution : Boolean,
												 goal_sat_list : Set[String] = Set.empty
                       )

case class StateLabel(
											 map_of_injected_goals : Map[String, RawPredicate],
											 map_of_active_goals : Map[String,RawLTLFormula],
											 //goal_states : Map[String,GoalState.Value], // map describing the current state of each system goals (see GoalState)
											 //sup_array : RawGoalSetSupervisor, // the node has a goal supervisor (sat state and next goal)
											 trigger_for : Set[String],
											 exit_for : Set[String],
											 is_terminal: Boolean, // a node is terminal it has been focused but it has not outgoing arcs
											 is_frontier : Boolean, // the node is yet to be expanded
											 metric : Float // quality of the node (higher is better)
                     ) {
	// the node fully address at least one of the goals
	def isTrigger() : Boolean = trigger_for.nonEmpty

	// the node triggers at least on of the goals
	// (by default, the first node of a WTS is the trigger for the "virtual" root goal)
	def isExit() : Boolean = exit_for.nonEmpty
}

/******* WTS GRAPH ********/
case class RawWTSArc(
											origin : RawState,
											destination : RawState,
											probability : Float,
											action: RawAction,
											scenario_name : String
										)

case class WTSGraph(
										 start : RawState,
	                   nodes : Set[RawState],
	                   transitions : Set[RawWTSArc],
	                   wts_labelling : WTSLabelling,
										 is_full_solution : Boolean
                 ) {

	def node_is_terminal(node: RawState) : Boolean = wts_labelling.nodes_labelling(node).is_terminal

	def isFullSolution : Boolean = is_full_solution
	def isPartialSolution : Boolean = !is_full_solution

	def to_graphviz(pretty_string: RawState => String) : String = {
		var string = "digraph WTS {\n"

		for (n <- nodes) {
			string += "\""+pretty_string(n)+"\""

			if (wts_labelling.nodes_labelling(n).isExit())
				string += "[style=bold,color=green];\n"
			else
				string += "[color=black];\n"
		}

		for (t <- transitions) {
			string += "\""+pretty_string(t.origin)+"\""
			string += "->"
			string += "\""+pretty_string(t.destination)+"\""
			string += "[label=\""+t.action.id+"_"+t.scenario_name+"\"];\n"
		}

		string + "}\n"
	}

	def node_deco_graphviz(node: RawState) : String = {
		val injected = wts_labelling.nodes_labelling(node).map_of_injected_goals
		val active = wts_labelling.nodes_labelling(node).map_of_active_goals
		val triggerfor = wts_labelling.nodes_labelling(node).trigger_for
		val exitfor = wts_labelling.nodes_labelling(node).exit_for

		var inj_synt = ""
		for (i <- injected) inj_synt += i._1+";"
		var act_synt = ""
		for (a <- active) act_synt += a._1+";"
		val tri_synt = triggerfor.mkString(";")
		val exi_synt = exitfor.mkString(";")

		raw"inj=($inj_synt)\n act=($act_synt)\n trig=($tri_synt)\n exit=($exi_synt)"
	}


	def to_decorated_graphviz(pretty_string: RawState => String) : String = {


		var string = "digraph WTS {\n"

		for (n <- nodes) {
			string += "\""+pretty_string(n)+"\"[label=\""+node_deco_graphviz(n)+"\"]\n"
		}

		for (t <- transitions) {
			string += "\""+pretty_string(t.origin)+"\""
			string += "->"
			string += "\""+pretty_string(t.destination)+"\""
			string += "[label=\""+t.action.id+"_"+t.scenario_name+"\"];\n"
		}

		string + "}\n"
	}


	def update_wts_file(pretty_string: RawState => String,file:String) : Unit = {
		val pw = new PrintWriter(new File(file))
		pw.write(to_decorated_graphviz(pretty_string))
		pw.close
	}


}





