package org.icar.pmr_solver.best_first_planner

import org.icar.sublevel.RawState
import org.icar.symbolic.{CapabilityGrounding, EndEvent, HL_PredicateFormula, JoinGateway, SequenceFlow, Solution, SolutionTask, SplitGateway, StartEvent, StateOfWorld, True, WorkflowItem}

class WTS2Solution(wts:WTSGraph, I : StateOfWorld) {
  var wfitems: Set[WorkflowItem] = Set(StartEvent(),EndEvent())
  var wfflow: List[SequenceFlow] = List.empty
  var map:Map[RawState,WorkflowItem] = Map.empty

  var task_id=1; var split_id=1; var join_id=1

  visit_node(wts.start, StartEvent())

  def toSolution : Solution = {
    Solution(
      I,
      wfitems.toArray,
      wfflow.toArray,
      true//wts.isFullSolution
    )

  }

  def visit_node(wts_node: RawState, connect_from : WorkflowItem, scenario:String="") : Unit = {
    if (!map.contains(wts_node)) {
      val outgoing = wts.transitions.filter(_.origin == wts_node)
      if (outgoing.size == 0) {
        // first element
        addSequenceFlow(connect_from,EndEvent(),scenario)

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

  private def print_item(n: WorkflowItem): String = {

    n match {
      case StartEvent() => "start"
      case EndEvent() => "end"
      case SolutionTask(id, grounding) => s"${id}_${grounding.unique_id}"
      case JoinGateway(id) => "J"+id
      case SplitGateway(id, outport) => s"S${id}"
    }
  }
  private def print_item_decoration(n: WorkflowItem): String = {
    n match {
      case StartEvent() => "[shape=doublecircle,color=black];\n"
      case EndEvent() => "[shape=doublecircle,color=green];\n"
      case SolutionTask(_, _) => "[shape=box,color=black];\n"
      case JoinGateway(_) => "[shape=diamond,color=black];\n"
      case SplitGateway(_, _) => "[shape=diamond,color=black];\n"
    }
  }


}
