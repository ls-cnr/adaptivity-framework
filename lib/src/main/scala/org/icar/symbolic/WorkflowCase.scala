package org.icar.symbolic

import org.icar.sublevel.{HL2Raw_Map, RawPredicate, RawState}


abstract class WorkflowReference
case class SimpleItem(item:WorkflowItem) extends WorkflowReference
case class ConditionedItem(cond:RawPredicate,task:SolutionTask) extends WorkflowReference
case class WaitItem(task:SolutionTask) extends WorkflowReference
case class MultiItem(decision:SplitGateway,succs:Array[Branch]) extends WorkflowReference

case class Branch(scenario:String,scenario_condition:RawPredicate)
case class ExecutionLog(time_stamp:Long,task:SolutionTask)


class WorkflowCase(val domain: Domain, workflow:Solution, val execute:SolutionTask=>Unit ) {
	val raw_map = new HL2Raw_Map(domain)
	var case_pool : Set[WorkflowReference] = Set(SimpleItem(StartEvent()))
	var process_log_list : List[ExecutionLog] = List.empty

	def progress(state:RawState) : Unit = {
		for (ref<-case_pool) {
			progress(ref,state)
		}
	}
	def external_progress(completed_task:SolutionTask) : Unit = {
		for (ref<-case_pool if ref.isInstanceOf[WaitItem]) {
			val wait = ref.asInstanceOf[WaitItem]
			if (wait.task==completed_task) {
				case_pool = (case_pool - ref) + successors_reference(completed_task)
				val timestamp: Long = System.currentTimeMillis
				process_log_list = ExecutionLog(timestamp,completed_task)::process_log_list
			}
		}
	}

	private def progress(ref:WorkflowReference, state:RawState) : Unit = {
		ref match {
			case SimpleItem(item) =>
				case_pool = (case_pool - ref) + successors_reference(item)

			case ConditionedItem(cond, task) =>
				val condition = state.satisfies(cond)
				if (condition){
					execute(task)
					case_pool = (case_pool - ref) + WaitItem(task)
				}
			case MultiItem(decision, branches) =>
				val opt_winning_branch = check_branches(branches,state)
				if (opt_winning_branch.isDefined)
					case_pool = (case_pool - ref) + successors_reference(decision,Some(opt_winning_branch.get.scenario))
			case _ =>
		}
	}

	private def check_branches(branches: Array[Branch], state: RawState):Option[Branch] = {
		var opt_branch : Option[Branch] = None
		for (a_branch<-branches if !opt_branch.isDefined) {
			if (state.satisfies(a_branch.scenario_condition))
				opt_branch=Some(a_branch)
		}
		opt_branch
	}

	private def successors_reference(item: WorkflowItem, scenario:Option[String]=None): WorkflowReference = {
		item match {
			case StartEvent() => SimpleItem(item)
			case EndEvent() => SimpleItem(item)
			case JoinGateway(_) => SimpleItem(item)
			case t@SolutionTask(_, grounding) =>
				val real_pre = grounding.capability.pre
				val assigned = grounding.grounding
				val pre_with_assignements = HL_PredicateFormula.substitution(real_pre,assigned)
				val raw_pre: RawPredicate = raw_map.predicate_formula(pre_with_assignements)
				ConditionedItem(raw_pre,t)
			case s@SplitGateway(_, outports) =>
				var outgoing_branches:List[Branch] = List.empty
				for (scenario <- outports) {
					val opt_branch_condition = workflow.branch_condition(s,scenario)
					if (opt_branch_condition.isDefined){
						val pre_with_assignements = opt_branch_condition.get
						val raw_pre: RawPredicate = raw_map.predicate_formula(pre_with_assignements)
						outgoing_branches = Branch(scenario,raw_pre) :: outgoing_branches
					}
				}
				MultiItem(s,outgoing_branches.toArray)
		}
	}


}
