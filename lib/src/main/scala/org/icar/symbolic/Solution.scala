package org.icar.symbolic

import java.io.{File, PrintWriter}

abstract class WorkflowItem
case class StartEvent() extends WorkflowItem
case class EndEvent() extends WorkflowItem
case class SolutionTask(id:Int, grounding : CapabilityGrounding) extends WorkflowItem
case class SplitGateway(id:Int,outport:List[String]) extends WorkflowItem
case class JoinGateway(id:Int) extends WorkflowItem

case class SequenceFlow(from:WorkflowItem,to:WorkflowItem,scenario:String="",condition:HL_PredicateFormula=True())


case class Solution(
	                   start:StateOfWorld,
	                   wfitems: Array[WorkflowItem],
	                   wfflow:Array[SequenceFlow],
	                   complete: Boolean
					) {

	def successors(task: WorkflowItem) : Array[WorkflowItem] = {
		val out = wfflow.filter( _.from==task )
		val succs: Array[WorkflowItem] =for (o<-out) yield o.to
		succs
	}

	def predecessors(task: WorkflowItem) : Array[WorkflowItem] = {
		val in = wfflow.filter( _.to==task )
		val preds: Array[WorkflowItem] =for (i<-in) yield i.from
		preds
	}

	def branch_condition(gateway:SplitGateway,scenario:String) : Option[HL_PredicateFormula] = {
		val out = wfflow.filter( f => f.from==gateway && f.scenario==scenario )
		if (out.isEmpty)
			None
		else if (out.head.to.isInstanceOf[SolutionTask]) {
			val task = out.head.to.asInstanceOf[SolutionTask]
			val real_pre = task.grounding.capability.pre
			val assigned = task.grounding.grounding
			val pre_with_assignements = HL_PredicateFormula.substitution(real_pre,assigned)
			if (out.head.condition == True())
				Some(pre_with_assignements)
			else
				Some(Conjunction[HL_PredicateFormula](List(out.head.condition,pre_with_assignements)))
		} else {
			None
		}
	}


	private def print_item(n: WorkflowItem): String = {
		n match {
			case StartEvent() => "start"
			case EndEvent() => "end"
			case SolutionTask(_, grounding) => grounding.unique_id
			case JoinGateway(id) => "J"+id
			case SplitGateway(id, outport) => "S"+id
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

	def update_wts_file(file:String) : Unit = {
		val pw = new PrintWriter(new File(file))
		pw.write(to_graphviz())
		pw.close
	}

}


