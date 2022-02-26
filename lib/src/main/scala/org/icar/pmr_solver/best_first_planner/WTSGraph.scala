package org.icar.pmr_solver.best_first_planner

import java.io.{File, PrintWriter}
import org.icar.symbolic.{CapabilityGrounding, EndEvent, HL_PredicateFormula, JoinGateway, SequenceFlow, Solution, SolutionTask, SplitGateway, StartEvent, StateOfWorld, True, WorkflowItem}
import org.icar.rete.RETEMemory
import org.icar.sublevel.{RawAction, RawGoalModelSupervisor, RawPredicate, RawState}


/******* NOTES AND COMMENTS ********/
// Luca: FullSolution - improvement: it is necessary to check for loop safety
// a loop is valid if there is the possibility to leave it and go towards a terminal state



/******* GRAPH/NODE LABELLING ********/
case class WTSLabelling(
	                       nodes_labelling : Map[RawState,StateLabel],  // each node is associated to a set of properties
	                       quality_of_solution : Float,     // global quality of the (partial) solution
	                       invariants : List[RawPredicate],  // conditions that must hold in any new node
	                       full_solution : Boolean
                       )

case class StateLabel(
	                     sup_array : RawGoalModelSupervisor,    // the node has a goal supervisor (sat state and next goal)
	                     is_terminal: Boolean,                  // the node is terminal
	                     is_frontier : Boolean,                 // the node is part of the frontier
	                     is_exit : Boolean,                     // the node fully address all the goals
	                     leads_to_exit : Boolean,               // not implemented: the node leads to an exit node
	                     metric : Float                         // quality of the node (higher is better)
                     )

/******* WTS GRAPH ********/
case class RawWTSArc(origin : RawState, destination : RawState, probability : Float, action: RawAction, scenario_name : String)

case class WTSGraph(
	                   start : RawState,
	                   nodes : Set[RawState],
	                   transitions : Set[RawWTSArc],
	                   perturbations : Set[RawWTSArc],

	                   wts_labelling : WTSLabelling
                 ) {

	def node_is_terminal(node: RawState) : Boolean = wts_labelling.nodes_labelling(node).is_terminal

	def isFullSolution : Boolean = wts_labelling.full_solution
	def isPartialSolution : Boolean = !isFullSolution

	def to_graphviz(pretty_string: RawState => String) : String = {
		var string = "digraph WTS {\n"

		for (n <- nodes) {
			string += "\""+pretty_string(n)+"\""

			if (wts_labelling.nodes_labelling(n).is_exit)
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

		for (t <- perturbations) {
			string += "\""+pretty_string(t.origin)+"\""
			string += "->"
			string += "\""+pretty_string(t.destination)+"\""
			string += "[style=dotted, label=\""+t.action.id+"_"+t.probability+"% \"];\n"
		}

		string + "}\n"
	}

	def update_wts_file(pretty_string: RawState => String,file:String) : Unit = {
		val pw = new PrintWriter(new File(file))
		pw.write(to_graphviz(pretty_string))
		pw.close
	}


}

object WTSGraph {

	def check_full_solution(terminal_states:Set[RawState],frontier_states:Set[RETEMemory],labelling : Map[RawState,StateLabel]) : Boolean = {
		var full=true

		for (terminal_node <- terminal_states) {
			val sup_array = labelling(terminal_node).sup_array
			if (!sup_array.check_exit_node)
				full=false
		}
		for (frontier_node_memory <- frontier_states) {
			val sup_array = labelling(frontier_node_memory.current).sup_array
			if (!sup_array.check_exit_node)
				full=false
		}

		full
	}

	def check_pre_expansion_validity_test(wts: WTSGraph, exp: RawExpansion, conf: SolutionConfiguration):Boolean = {
		var multiple_cap_test = true
		var allowed_by_invariants = true
		for (i<-wts.wts_labelling.invariants)
			for (t<-exp.probtrajectory)
				if (!t.dest.rete_memory.current.satisfies(i))
					allowed_by_invariants=false

		if(!conf.allow_cap_multiple_instance) {
			for (t<-wts.transitions)
				if (t.action.id == exp.due_to.id)
					multiple_cap_test=false
		}
		allowed_by_invariants && multiple_cap_test
	}

	def check_post_expansion_validity_test(wts: WTSGraph, new_nodes: Set[RawState], new_transitions: Set[RawWTSArc], conf: SolutionConfiguration):Boolean = {
		var self_loop_test = true
		if (!conf.allow_self_loop)
			for (t<-new_transitions)
				if (t.origin == t.destination)
					self_loop_test=false

		var loop_test = true
		if (!conf.allow_loop)
			for (t<-new_transitions)
				if (wts.nodes.contains(t.destination))
					loop_test=false

		self_loop_test && loop_test
	}

	def update_wts(wts:WTSGraph, focus : RawState, exp_due_to_system: List[RawExpansion], exp_due_to_environment: List[RawExpansion], qos : RawState => Float, conf : SolutionConfiguration) : List[WTSGraph] = {
		require(wts.nodes.contains(focus))

		if (wts.wts_labelling.nodes_labelling(focus).is_frontier && exp_due_to_system.nonEmpty) {

			var updated_list : List[WTSGraph] = List.empty

			for (exp <- exp_due_to_system) {
				val pre_test = check_pre_expansion_validity_test(wts,exp,conf)
				if (pre_test) {
					val sys_res : (Set[RawFrontierItem],Set[RawWTSArc]) = apply_sys_exp(wts,exp)
					val env_res : (Set[RawFrontierItem],Set[RawWTSArc]) = apply_env_exp(wts,exp_due_to_environment)

					var exp_frontier = sys_res._1++env_res._1

					var new_states : Set[RawState] = Set.empty
					exp_frontier.foreach(n=>new_states += n.rete_memory.current)

					val post_test = check_post_expansion_validity_test(wts,new_states,sys_res._2,conf)
					if (post_test) {
						val updated_labelling = update_wts_labelling(wts,focus,exp_frontier,sys_res._2,env_res._2,qos,exp.invariants)

						/* FINALLY, the new list of WTS will contain the cloned updated WTS */
						val new_wts = WTSGraph(
							wts.start,                          //initial node
							wts.nodes++new_states,               //nodes
							wts.transitions++sys_res._2,        //transitions
							wts.perturbations++env_res._2,      //perturbations
							updated_labelling                   //labelling
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

		} else if (exp_due_to_environment.nonEmpty) {
			val env_res: (Set[RawFrontierItem], Set[RawWTSArc]) = apply_env_exp(wts, exp_due_to_environment)

			val exp_nodes: Set[RawFrontierItem] = env_res._1
			var new_wts_states : Set[RawState] = Set.empty
			exp_nodes.foreach(n=>new_wts_states += n.rete_memory.current)

			/*
				note for EVOLUTION CONSTRAINTS
				do perturbations produce new invariants?
			*/
			val updated_labelling = update_wts_labelling(wts, focus, exp_nodes, Set.empty, env_res._2, qos, List.empty)

			//val quality = calculate_quality_of_solution(wts,focus,updated_frontier,new_nodes,sys_res._2,env_res._2)

			/* FINALLY, the new list of WTS will contain the cloned updated WTS */
			val new_wts = WTSGraph(
				wts.start, //initial node
				wts.nodes ++ new_wts_states, //nodes
				wts.transitions, //transitions
				wts.perturbations ++ env_res._2, //perturbations
				updated_labelling //labelling
			)

			/* FINALLY, the new list of WTS will contain the cloned updated WTS */
			List(new_wts)

		} else {
			List(WTSGraph(
				wts.start, //initial node
				wts.nodes, //nodes
				wts.transitions, //transitions
				wts.perturbations, //perturbations
				update_wts_labelling(wts, focus, Set.empty, Set.empty, Set.empty, qos, List.empty) //labelling
			))
		}

	}

	private def update_wts_labelling(wts: WTSGraph, focus: RawState, new_frontier: Set[RawFrontierItem], new_transitions: Set[RawWTSArc], new_perturbations: Set[RawWTSArc], qos : RawState => Float, invariants:List[RawPredicate]) : WTSLabelling = {
		var check_full_solution_condition = true

/*
		// ** list of Frontier and Terminal nodes **
		// 1. operations on the frontier: ALWAYS remove the focus node (LATER add all new nodes that are not exit nodes)
		var updated_frontier = wts.wts_labelling.frontier.filter( _ != focus )
		// 2. operation on the list of terminal: add focus only IF there are no expansions
		val updated_terminal = if (new_nodes.isEmpty) wts.wts_labelling.terminal + focus else wts.wts_labelling.terminal
*/

		// ** single nodes' Labelling **
		var updated_node_labelling : Map[RawState,StateLabel] = wts.wts_labelling.nodes_labelling
		// 1. change label of focus node: setting is_frontier to false and is_terminal to true only if there are no expansions
		val previous_focus_label : StateLabel = wts.wts_labelling.nodes_labelling(focus)
		val updated_focus_label = previous_focus_label.copy(is_frontier = false, is_terminal = (new_frontier.isEmpty))
		updated_node_labelling += (focus -> updated_focus_label)

		// for each new node, calculate the new goal_supervisor_array, if it is exit_node, the updated_metric
		//val focus_supervisor = wts.wts_labelling.labelling(focus).sup_array
		for (node <- new_frontier) {
			val updated_sup  = node.sup
			val is_exit = updated_sup.check_exit_node
			val updated_metric = node.score
			val updated_label = StateLabel(updated_sup,is_exit,!is_exit,is_exit,is_exit,updated_metric)

			//if (is_exit)
				//check_full_solution_condition=true

			updated_node_labelling = updated_node_labelling + (node.rete_memory.current -> updated_label)
		}

		var full_solution = false

		//if (check_full_solution_condition) {
		var local_frontier_nodes = wts.wts_labelling.nodes_labelling.filter(_._2.is_frontier)
		if (local_frontier_nodes.isEmpty)
			full_solution=true

//			full_solution = true
//			for (l<-updated_node_labelling.values) {
//				if (l.is_terminal && !l.is_exit)
//					full_solution=false
//				if (l.is_frontier && !l.is_exit)
//					full_solution=false
//			}
//		}

		// Quality: delegate to specific function
		val updated_quality = 0//calculate_quality_of_solution(wts,updated_frontier,updated_node_labelling,new_nodes,new_transitions,new_perturbations)

		val updated_invariants = wts.wts_labelling.invariants ::: invariants
		WTSLabelling(updated_node_labelling, updated_quality.toFloat,updated_invariants,full_solution)
	}


	/*
	 * This function calculates the quality of a WTS
	 *
	 * PROVVISORIAMENTE: quality = average of frontier/terminal node values
	 */
	private def calculate_quality_of_solution(old_wts: WTSGraph, updated_frontier : Set[RawState], updated_node_labelling: Map[RawState,StateLabel], new_nodes: Set[RawState], new_transition: Set[RawWTSArc], new_perturb: Set[RawWTSArc]): Float = {
		var q : Float = 0
		for (f <- updated_frontier)
			q+=updated_node_labelling(f).metric

		q/updated_frontier.size
	}


	private def apply_sys_exp(wts: WTSGraph, exp : RawExpansion) : (Set[RawFrontierItem],Set[RawWTSArc]) = {
		var new_frontier : Set[RawFrontierItem] = Set.empty
		var new_transition : Set[RawWTSArc] = Set.empty

		for (evolution_part <- exp.probtrajectory) {
			if (!wts.nodes.contains(evolution_part.dest.rete_memory.current))
				new_frontier += evolution_part.dest

			new_transition = new_transition + RawWTSArc(exp.from, evolution_part.dest.rete_memory.current, evolution_part.probability, exp.due_to, evolution_part.name)
		}

		(new_frontier,new_transition)
	}

	private def apply_env_exp(wts: WTSGraph, exps : List[RawExpansion]) : (Set[RawFrontierItem],Set[RawWTSArc]) = {
		var new_frontier : Set[RawFrontierItem] = Set.empty
		var new_perturb : Set[RawWTSArc] = Set.empty

		for (pert <- exps) {
			for (perturb_part <- pert.probtrajectory) {
				if (!wts.nodes.contains(perturb_part.dest.rete_memory.current))
					new_frontier += perturb_part.dest

				new_perturb = new_perturb + RawWTSArc(pert.from, perturb_part.dest.rete_memory.current, perturb_part.probability, pert.due_to, perturb_part.name)
			}
		}

		(new_frontier,new_perturb)
	}

	def WTStoSolution(wts:WTSGraph, I : StateOfWorld) : Solution = {
		var map:Map[RawState,WorkflowItem] = Map.empty
		var wfitems: Set[WorkflowItem] = Set(StartEvent(),EndEvent())
		var wfflow: List[SequenceFlow] = List.empty

		var task_id=0; var split_id=0; var join_id=0
		visit_node(wts.start)


		def visit_node(focus: RawState):WorkflowItem = {
			if (map.contains(focus))
				map(focus)
			else {
				val incoming = wts.transitions.filter(_.destination == focus) ++ wts.perturbations.filter(_.destination == focus)
				val outgoing = wts.transitions.filter(_.origin == focus) ++ wts.perturbations.filter(_.origin == focus)


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
			wts.isFullSolution
		)
	}

}




