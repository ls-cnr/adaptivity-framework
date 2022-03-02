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
	                   wts_labelling : WTSLabelling
                 ) {

	def node_is_terminal(node: RawState) : Boolean = wts_labelling.nodes_labelling(node).is_terminal

	def isFullSolution(specifications:Array[RawGoal]) : Boolean = {
		//check all the goals are fully addressed
		val all_goals_are_sat = wts_labelling.goal_sat_list.size == specifications.size

		//check all terminal node are exit nodes without trigger
		var all_terminal_are_exit = true
		var no_terminal_has_trigger = true
		for (node <- nodes) {
			if (wts_labelling.nodes_labelling(node).is_terminal) {
				if (!wts_labelling.nodes_labelling(node).isExit())
					all_terminal_are_exit = false

				if (wts_labelling.nodes_labelling(node).isTrigger())
					no_terminal_has_trigger = false
			}
		}

		all_goals_are_sat && all_terminal_are_exit && no_terminal_has_trigger
	}
	def isPartialSolution(specifications:Array[RawGoal]) : Boolean = !isFullSolution(specifications)

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

//		for (t <- perturbations) {
//			string += "\""+pretty_string(t.origin)+"\""
//			string += "->"
//			string += "\""+pretty_string(t.destination)+"\""
//			string += "[style=dotted, label=\""+t.action.id+"_"+t.probability+"% \"];\n"
//		}

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

		//"[label=\"inj=("+inj_synt+"){\n}act=("+inj_synt+"){\n}trig=("+tri_synt+"){\n}exit=("+exi_synt+")\""
		//"\""+pretty_string(node)+"[i:"+inj_synt+"][a:"+act_synt+"][t:"+tri_synt+"][e:"+exi_synt+"]\""
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

		//		for (t <- perturbations) {
		//			string += "\""+pretty_string(t.origin)+"\""
		//			string += "->"
		//			string += "\""+pretty_string(t.destination)+"\""
		//			string += "[style=dotted, label=\""+t.action.id+"_"+t.probability+"% \"];\n"
		//		}

		string + "}\n"
	}


	def update_wts_file(pretty_string: RawState => String,file:String) : Unit = {
		val pw = new PrintWriter(new File(file))
		pw.write(to_decorated_graphviz(pretty_string))
		pw.close
	}


}

object WTSGraph {
//
//	def check_full_solution(terminal_states:Set[RawState],frontier_states:Set[RETEMemory],labelling : Map[RawState,StateLabel]) : Boolean = {
//		var full=true
//
//		for (terminal_node <- terminal_states) {
//			val sup_array = labelling(terminal_node).sup_array
//			if (!sup_array.check_exit_node)
//				full=false
//		}
//		for (frontier_node_memory <- frontier_states) {
//			val sup_array = labelling(frontier_node_memory.current).sup_array
//			if (!sup_array.check_exit_node)
//				full=false
//		}
//
//		full
//	}
//
//
//
//
//	/*
//	 * This function calculates the quality of a WTS
//	 *
//	 * PROVVISORIAMENTE: quality = average of frontier/terminal node values
//	 */
//	private def calculate_quality_of_solution(old_wts: WTSGraph, updated_frontier : Set[RawState], updated_node_labelling: Map[RawState,StateLabel], new_nodes: Set[RawState], new_transition: Set[RawWTSArc], new_perturb: Set[RawWTSArc]): Float = {
//		var q : Float = 0
//		for (f <- updated_frontier)
//			q+=updated_node_labelling(f).metric
//
//		q/updated_frontier.size
//	}





	// obsolete: use WTS2Solution class instead
	def WTStoSolutionDEPRECATED(wts:WTSGraph, I : StateOfWorld) : Solution = {
		var map:Map[RawState,WorkflowItem] = Map.empty
		var wfitems: Set[WorkflowItem] = Set(StartEvent(),EndEvent())
		var wfflow: List[SequenceFlow] = List.empty

		var task_id=0; var split_id=0; var join_id=0
		visit_node(wts.start)


		def visit_node(focus: RawState):WorkflowItem = {
			if (map.contains(focus))
				map(focus)
			else {
				val incoming = wts.transitions.filter(_.destination == focus)
				val outgoing = wts.transitions.filter(_.origin == focus)


				if (incoming.size == 0) {
					val next = visit_transitions(outgoing,focus)
					addSequenceFlow(StartEvent(), next, "", True())
					map += (focus -> StartEvent())
					StartEvent()

					/*
				} else if (incoming.size > 1) {
					val join = JoinGateway(join_id)
					join_id += 1
					wfitems = wfitems + join
					map += (focus -> join)

					val next = visit_transitions(outgoing)
					addSequenceFlow(join, next, "", True())

					join
*/
				} else {
					visit_transitions(outgoing,focus)
				}
			}
		}

		def visit_transitions(outs:Set[RawWTSArc],focus: RawState) : WorkflowItem = {
			if (outs.size==0){
				map += (focus -> EndEvent())
				EndEvent()

			} else if (outs.size==1){
				val task = addTask(task_id,outs.head.action.grounding)
				map += (focus -> task)
				val next = visit_node(outs.head.destination)

				addSequenceFlow(task, next, "", True())
				task

			} else {
				val tasks = for (t<-outs.toList) yield t.action
				if (tasks.toSet.size==1) {
					val task = addTask(task_id,tasks.head.grounding)
					map += (focus -> task)

					val outports = for (t<-outs.toList) yield t.scenario_name
					val split = addSplitGateway(split_id,outports.toList)

					addSequenceFlow(task, split, "", True())

					for (t<-outs) {
						val next = visit_node(t.destination)
						addSequenceFlow(split, next, t.scenario_name, True())
					}
					task



				} else {
					val outports = for (t<-outs.toList) yield t.scenario_name
					val split = addSplitGateway(split_id,outports.toList)
					map += (focus -> split)

					for (t<-outs) {
						val task = addTask(task_id,t.action.grounding)

						val next = visit_node(t.destination)
						addSequenceFlow(task, split, "", True())
						addSequenceFlow(split, next, t.scenario_name, True())
					}
					split
				}
			}
		}

		def addTask(id:Int,grounding : CapabilityGrounding) : SolutionTask = {
			var exists=false
			var opttask:Option[SolutionTask]=None
			for (i<-wfitems if i.isInstanceOf[SolutionTask]) {
				val task = i.asInstanceOf[SolutionTask]
				if (task.grounding==grounding){
					exists=true
					opttask = Some(task)
				}
			}

			if (!exists) {
				val task = SolutionTask(task_id,grounding)
				task_id += 1
				wfitems = wfitems+task
				opttask = Some(task)
			}
			opttask.get
		}

		def addSplitGateway(id:Int,outport:List[String]) : SplitGateway = {
			var exists=false
			var optgw:Option[SplitGateway]=None
			for (i<-wfitems if i.isInstanceOf[SplitGateway]) {
				val gw = i.asInstanceOf[SplitGateway]
				if (gw.outport==outport){
					exists=true
					optgw = Some(gw)
				}
			}

			if (!exists) {
				val gw = SplitGateway(split_id,outport)
				split_id += 1
				wfitems = wfitems+gw
				optgw = Some(gw)
			}
			optgw.get
		}

		def addSequenceFlow(from:WorkflowItem,to:WorkflowItem,scenario:String="",condition:HL_PredicateFormula=True()) : Unit = {
			if (!wfflow.contains(SequenceFlow(from,to,scenario,condition))) {
				wfflow = SequenceFlow(from,to,scenario,condition) :: wfflow
			}
		}

		def optimize : Unit = {
			for (i<-wfitems if (i.isInstanceOf[SolutionTask] || i.isInstanceOf[SplitGateway])) {
				val in_flows = wfflow.filter(_.to==i)

				if (in_flows.size>1) {
					/* add a Join Gateway */

					val gw = JoinGateway(join_id)
					join_id += 1
					wfitems = wfitems+gw

					addSequenceFlow(gw,i,"")

					in_flows.foreach( x => wfflow = wfflow.filter(_ != x) )
					in_flows.foreach( x => addSequenceFlow(x.from,gw,x.scenario,x.condition) )
				}
			}
		}

		optimize

		Solution(
			I,
			wfitems.toArray,
			wfflow.toArray,
			true//wts.isFullSolution
		)
	}

}




