package org.icar.bpmn2goal

import org.icar.symbolic.{EndEvent, JoinGateway, Solution, SplitGateway, StartEvent, WorkflowItem, SolutionTask => WorkflowTask}

/* this class hierarchy is useful for the Sol2MOISE */
abstract class WorkflowPattern()
case class ActivityPattern(task: WorkflowTask) extends WorkflowPattern
case class SequencePattern(children:List[WorkflowPattern]) extends WorkflowPattern
case class LoopPattern(before_check:List[WorkflowPattern],after_check:List[WorkflowPattern],exit:SplitGateway) extends WorkflowPattern
case class ChoicePattern(children:List[WorkflowPattern]) extends WorkflowPattern
case class ParallelPattern(children:List[WorkflowPattern]) extends WorkflowPattern

class SolutionPattern(sol:Solution) {

	val start_event = sol.wfitems.filter(i => i == StartEvent(0,s"startEvent_${0}")).head
	var visited: List[WorkflowItem] = List(StartEvent(0,s"startEvent_${0}"))

	var first_node: WorkflowItem = sol.successors(start_event).head
	var root_pattern = decompose(first_node,visited, "main")

	def to_graphviz() : String = {
		seq_id=0; ch_id=0; loop_id=0
		var string = "digraph PatternTree {\n"

		if (root_pattern._1.isDefined) {
			val tree = root_pattern._1.get

			string += "\"workflow\";\n"
			string += node_to_graphviz(tree,"workflow")

		}
		string + "}\n"
	}

	private var seq_id=0; private var ch_id=0; private var loop_id=0;
	private def get_seq_id : String = {seq_id+= 1; "Seq_"+seq_id}
	private def get_choice_id : String = {ch_id+= 1; "Choice_"+ch_id}
	private def get_loop_id : String = {loop_id+= 1; "Loop_"+loop_id}
	private def node_to_graphviz(node: WorkflowPattern, root_node : String, label:String=""):String = {
		var string = ""
		node match {
			case SequencePattern(children) =>
				val SEQ_ID = get_seq_id
				string += "\""+SEQ_ID+"\";\n"
				string += "\""+root_node+"\" -> \""+SEQ_ID+"\"[label=\""+label+"\"];\n"

				var num = 1
				children.foreach(x => {string += node_to_graphviz(x,SEQ_ID,num.toString); num+=1})

			case ChoicePattern(children) =>
				val SEQ_ID = get_choice_id
				string += "\""+SEQ_ID+"\";\n"
				string += "\""+root_node+"\" -> \""+SEQ_ID+"\"[label=\""+label+"\"];\n"

				children.foreach(x => string += node_to_graphviz(x,SEQ_ID))

			case LoopPattern(before_check, after_check, exit) =>
				val SEQ_ID = get_loop_id
				string += "\""+SEQ_ID+"\";\n"
				string += "\""+root_node+"\" -> \""+SEQ_ID+"\"[label=\""+label+"\"];\n"

				before_check.foreach(x => string += node_to_graphviz(x,SEQ_ID,"before check"))
				after_check.foreach(x => string += node_to_graphviz(x,SEQ_ID,"after check"))

			case ActivityPattern(task) =>
				string += "\""+task.grounding.capability.id+"\";\n"
				string += "\""+root_node+"\" -> \""+task.grounding.capability.id+"\"[label=\""+label+"\"];\n"

		}
		string
	}

	private def decompose(pivot_node: WorkflowItem, visited: List[WorkflowItem], inside : String): (Option[WorkflowPattern], WorkflowItem, List[WorkflowItem]) = {
		var node_pointer = pivot_node
		var new_visited = visited
		var children: List[WorkflowPattern] = List.empty

		var terminate: Boolean = false
		var error: Boolean = false
		while (terminate == false && error==false) {
			node_pointer match {
				case start: StartEvent =>
					node_pointer = sol.successors(node_pointer).head

				case task: WorkflowTask =>
					if (visited.contains(node_pointer)) {
						error = true
					} else {
						children = ActivityPattern( task ) :: children
						new_visited = node_pointer :: new_visited

						if (inside=="backward_loop") {
							node_pointer = sol.predecessors(node_pointer).head
						} else {
							node_pointer = sol.successors(node_pointer).head
						}
					}

				case xor: SplitGateway =>
					new_visited = node_pointer :: new_visited

					if (inside=="forward_loop" || inside=="backward_loop"){
						terminate=true

					} else {
						val result = expore_if_choice(xor, new_visited)
						if (result._1.isDefined) {
							val patt = result._1.get
							children = patt :: children
							node_pointer = result._2
							new_visited = result._3
						}
					}

				case join: JoinGateway =>
					new_visited = node_pointer :: new_visited

					if (inside=="choice") {
						terminate=true

					} else {
						val result = explore_if_loop(join,new_visited)
						if (result._1.isDefined) {
							val patt = result._1.get
							children = patt :: children
							node_pointer = result._2
							new_visited = result._3
						}
					}

				case end : EndEvent =>
					new_visited = node_pointer :: new_visited

					if (inside=="forward_loop"){
						error = true

					} else {
						terminate=true
					}
			}
		}

		if (!error)
			(Some(SequencePattern(children.reverse)), node_pointer, new_visited)
		else
			(None,pivot_node,visited)
	}

	private def expore_if_choice(pivot_node: SplitGateway, visited: List[WorkflowItem]): (Option[WorkflowPattern], WorkflowItem, List[WorkflowItem]) = {
		var branches :List[WorkflowPattern]=List.empty
		var ending_points : Set[WorkflowItem]=Set.empty
		val split_successors = sol.successors(pivot_node)

		var new_visited = visited

		var error: Boolean = false
		for (i<-split_successors if !error) {
			val result = decompose(i,new_visited,"choice")
			if (result._1.isDefined) {
				branches = result._1.get :: branches
				if (result._2.isInstanceOf[EndEvent]) ending_points += result._2
				new_visited = result._3
			} else {
				error=true
			}
		}

		if (ending_points.size>1)
			error = true

		if (!error) {
			val last = if (ending_points.size==1) sol.successors(ending_points.head).head  else EndEvent(0,s"endEvent_${0}")
			(Some(ChoicePattern( branches) ), last, new_visited)
		} else {
			(None,pivot_node,visited)
		}
	}

	private def explore_if_loop(pivot_node: JoinGateway,visited: List[WorkflowItem]): (Option[WorkflowPattern], WorkflowItem, List[WorkflowItem]) = {
		var before :List[WorkflowPattern]=List.empty
		var after :List[WorkflowPattern]=List.empty

		val split_successors = sol.successors(pivot_node)
		val split_predecessors = sol.predecessors(pivot_node)

		var new_visited = visited

		/* forward */
		var exit_split : Set[WorkflowItem]=Set.empty
		var error: Boolean = false
		for (i<-split_successors if !error) {
			val result = decompose(i,new_visited,"forward_loop")
			if (result._1.isDefined) {
				before = result._1.get :: before
				exit_split += result._2
				new_visited = result._3
			} else {
				error=true
			}
		}
		/* backward */
		for (i<-split_predecessors if !error && !new_visited.contains(i)) {
			val result = decompose(i,new_visited,"backward_loop")
			if (result._1.isDefined) {
				after = result._1.get :: after
				exit_split += result._2
				new_visited = result._3
			} else {
				error=true
			}
		}
		if (exit_split.size!=1)
			error = true


		if (!error) {
			val successors = sol.successors(exit_split.head).filter( !new_visited.contains(_))
			val last = successors.head

			/* what if successors contains more? */

			(Some(LoopPattern( before, after, exit_split.head.asInstanceOf[SplitGateway]) ), last, new_visited)
		} else {
			(None,pivot_node,visited)
		}
	}
}