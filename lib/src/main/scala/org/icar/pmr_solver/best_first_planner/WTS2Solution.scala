package org.icar.pmr_solver.best_first_planner

import org.icar.sublevel.RawState
import org.icar.symbolic.{CapabilityGrounding, EndEvent, HL_PredicateFormula, JoinGateway, SequenceFlow, Solution, SolutionTask, SplitGateway, StartEvent, StateOfWorld, True, WorkflowItem}

trait GoalPattern
case class StructuredLoop(goal_id:String, condition : String) extends GoalPattern


class WTS2Solution(wts:WTSGraph, I : StateOfWorld, patterns:List[GoalPattern]=List.empty) {

  val startEventID = 0

  var wfitems: Set[WorkflowItem] = Set(StartEvent(startEventID, s"start_${startEventID}"))
  var wfflow: List[SequenceFlow] = List.empty
  var map:Map[RawState,WorkflowItem] = Map.empty

  var task_id=1; var split_id=1; var join_id=1; var end_id = 1

  visit_node(wts.start, StartEvent(startEventID, s"start_${startEventID}"))

  for (pattern <- patterns)
    apply_pattern(pattern)

  def visit_node(wts_node: RawState, connect_from : WorkflowItem, scenario:String="") : Unit = {
    if (!map.contains(wts_node)) {
      val outgoing = wts.transitions.filter(_.origin == wts_node)
      if (outgoing.size == 0) {
        // last element
        val end_element = addEnd()
        map += (wts_node -> end_element)
        addSequenceFlow(connect_from,end_element,scenario)

      } else if (outgoing.size == 1) {
        // standard task
        val tx = outgoing.head
        val item = visit_transition(tx)
        val ref_item = map(wts_node)
        addSequenceFlow(connect_from, ref_item,scenario)

      } else {
        // task with many outcomes
        val item = visit_XOR(outgoing)
        val ref_item = map(wts_node)
        addSequenceFlow(connect_from, ref_item,scenario)
      }

    } else {
        // loop
        val item = addMergeGateway

        addSequenceFlow(connect_from,item,scenario)
        val arrival_item = map(wts_node)

        map -= (wts_node)
        map += (wts_node -> item)
        addSequenceFlow(item,arrival_item)
    }
  }

  def visit_transition(arc: RawWTSArc) : WorkflowItem = {
    val item = addTask(arc.action.grounding)
    map += (arc.origin -> item)
    visit_node(arc.destination,item)
    item
  }

  def visit_XOR(arcs: Set[RawWTSArc]) : WorkflowItem = {
    val source = arcs.head.origin
    val capability = arcs.head.action.grounding
    val outports = for (t<-arcs.toList) yield t.scenario_name

    val task_item = addTask(capability)
    map += (source -> task_item)

    val split_item = addSplitGateway(outports)
    addSequenceFlow(task_item,split_item)

    for (tx <- arcs) {
      visit_node(tx.destination,split_item,tx.scenario_name)
    }

    task_item
  }

  def addTask(grounding : CapabilityGrounding) : SolutionTask = {
    val task = SolutionTask(task_id,grounding)
    task_id += 1
    wfitems = wfitems+task
    task
  }

  def addEnd() : EndEvent = {
    val end = EndEvent(end_id, s"endEvent_${end_id}")
    end_id += 1
    wfitems = wfitems+end
    end
  }

  def addSplitGateway(outport:List[String]) : SplitGateway = {
    val gw = SplitGateway(split_id,outport)
    split_id += 1
    wfitems = wfitems+gw
    gw
  }

  def addMergeGateway : JoinGateway = {
    val gw = JoinGateway(join_id)
    join_id += 1
    wfitems = wfitems+gw
    gw
  }

  def addSequenceFlow(from:WorkflowItem,to:WorkflowItem,scenario:String="",condition:HL_PredicateFormula=True()) : Unit = {
    if (!wfflow.contains(SequenceFlow(from,to,scenario,condition))) {
      wfflow = SequenceFlow(from,to,scenario,condition) :: wfflow
    }
  }


  def apply_pattern(pattern: GoalPattern) : Unit = {
    pattern match {
      case p:StructuredLoop =>
        apply_structured_loop(p.goal_id, p.condition)
    }
  }

  def apply_structured_loop(goal_id: String, condition: String): Unit = {
    var applicability = check_structured_loop_applicability(goal_id)

    if (applicability) {
      for (node<-map.keys) {
        if (wts.wts_labelling.nodes_labelling(node).trigger_for.contains(goal_id)) {
          val item_node = map(node)

          // put join
          val pattern_join = addMergeGateway

          // put flow from node_prec to join
          val flowfrom_precs = wfflow.filter(_.to==item_node)
          for (i<-flowfrom_precs) {
            addSequenceFlow(i.from, pattern_join)
          }
          // separate node from node_prec
          wfflow = wfflow.filter(_.to!=item_node)

          // put flow from join to node
          addSequenceFlow(pattern_join,item_node)

          //visit all path until exit
          val arriving_nodes = visit_path_until_exit(node,goal_id)

          //for each arriving_node
          for (arr_node <- arriving_nodes) {
            val item_arriving_node = map(arr_node)

            //put split
            val pattern_split = addSplitGateway(List(condition,"otherwise"))

            //put flow from split to join with specified condition
            addSequenceFlow(pattern_split, pattern_join, condition)

            //put flow from join to arriving_node.prec
            val flowto_prec = wfflow.filter(_.to==item_arriving_node)
            for (f<-flowto_prec) {
              addSequenceFlow(f.from, pattern_split)
            }
            //remove flow from arriving_node to arriving_node.succ
            wfflow = wfflow.filter(_.to!=item_arriving_node)

            //put flow from arriving_node to split
            addSequenceFlow(pattern_split, item_arriving_node, "otherwise")

          }
        }
      }
    }
  }
  def check_structured_loop_applicability(str: String):Boolean = {
    //check at least 1 trigger && at least 1 exit
    var check_trigger = false
    var check_exit = false
    for (node<-map.keys) {
      if (wts.wts_labelling.nodes_labelling(node).trigger_for.contains(str))
        check_trigger = true
      if (wts.wts_labelling.nodes_labelling(node).exit_for.contains(str))
        check_exit = true
    }

    check_trigger && check_exit
  }

  def visit_path_until_exit(node: RawState, goal_id: String): List[RawState] = {
    if (wts.wts_labelling.nodes_labelling(node).exit_for.contains(goal_id))
      List(node)
    else {
      val item = map(node)
      if (item.isInstanceOf[EndEvent])
        List()
      else {
        val outgoing_flows = wfflow.filter(_.from==item)
        var list : List[RawState] = List.empty
        for (f <- outgoing_flows) {
          val dest = f.to
          val dest_state = inverse_map(dest)
          list = visit_path_until_exit(dest_state,goal_id) ::: list
        }
        list
      }
    }
  }






  def to_graphviz() : String = {
    var string = "digraph Solution {\n"+"rankdir=LR\n"+"{rank=min; \"start\"}\n"+"{rank=max; \"end\"}\n"

    for (n <- wfitems)
      string += "\""+print_item(n)+"\""+print_item_decoration(n)

    for (f <- wfflow) {
      string += "\""+print_item(f.from)+"\""
      string += "->"
      string += "\""+print_item(f.to)+"\""
      string += "[label=\""+f.scenario+"\"];\n"
    }
    string + "}\n"
  }

  def inverse_map(item: WorkflowItem):RawState = {
    map.filter(_._2 == item).head._1
  }

  def to_graphviz_with_states() : String = {
    var state_counter=1
    var string = "digraph Solution {\n"+"rankdir=LR\n"+"{rank=min; \"start\"}\n"+"{rank=max; \"end\"}\n"

    for (n <- wfitems)
      string += "\""+print_item(n)+"\""+print_item_decoration(n)

    for (f <- wfflow) {
      val item : WorkflowItem = f.to

      if (map.exists(_._2 == item)) {
        val state = inverse_map(item)
        val trigger = wts.wts_labelling.nodes_labelling(state).trigger_for.mkString(";")
        val exit = wts.wts_labelling.nodes_labelling(state).exit_for.mkString(";")

        string += "\""+state_counter+"\"[label=\""+raw"trig=($trigger)\n exit=($exit)"+"\"];\n"

        string += "\""+print_item(f.from)+"\""
        string += "->"
        string += "\""+state_counter+"\";\n"

        string += "\""+state_counter+"\""
        string += "->"
        string += "\""+print_item(f.to)+"\""
        string += "[label=\""+f.scenario+"\"];\n"

        state_counter += 1
      } else {
        string += "\""+print_item(f.from)+"\""
        string += "->"
        string += "\""+print_item(f.to)+"\""
        string += "[label=\""+f.scenario+"\"];\n"
      }
    }
    string + "}\n"
  }


  private def print_item(n: WorkflowItem): String = {
    n match {
      case StartEvent(_,_) => "start"
      case EndEvent(_,_) => "end"
      case SolutionTask(id, grounding) => s"${id}_${grounding.unique_id}"
      case JoinGateway(id) => "J"+id
      case SplitGateway(id, outport) => s"S${id}"
    }
  }
  private def print_item_decoration(n: WorkflowItem): String = {
    n match {
      case StartEvent(_,_) => "[shape=doublecircle,color=black];\n"
      case EndEvent(_,_) => "[shape=doublecircle,color=green];\n"
      case SolutionTask(_, _) => "[shape=box,color=black];\n"
      case JoinGateway(_) => "[shape=diamond,color=black];\n"
      case SplitGateway(_, _) => "[shape=diamond,color=black];\n"
    }
  }

  def toSolution : Solution = {
    Solution(
      I,
      wfitems.toArray,
      wfflow.toArray,
      wts.isFullSolution
    )

  }


}
