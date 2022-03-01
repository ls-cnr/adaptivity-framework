package org.icar.pmr_solver

case class SolverConfiguration (termination : TerminationDescription,sol_conf : SolutionConfiguration)

/******* SOLVER TERMINATION CONDITION ********/
abstract class TerminationDescription
case class SolutionTermination(n_sol: Int) extends TerminationDescription
case class TimeTermination(millisec: Long) extends TerminationDescription
case class IterationTermination(its: Int) extends TerminationDescription
case class AndTermination(left:TerminationDescription,right:TerminationDescription) extends TerminationDescription
case class OrTermination(left:TerminationDescription,right:TerminationDescription) extends TerminationDescription


object TerminationDescription {
	def check_termination(term_condition : TerminationDescription, start : Long, it : Int, num_exit_nodes : Int) : Boolean = {
		term_condition match {
			case SolutionTermination(n_sol) =>
				var ret = false
				if (num_exit_nodes >= n_sol)
						ret=true
				ret

			case TimeTermination(time) =>
				val c_time = System.currentTimeMillis
				val delta_time = c_time-start
				delta_time >= time

			case IterationTermination(max_it) =>
				it >= max_it

			case AndTermination(l,r) =>
				check_termination(l,start,it,num_exit_nodes) && check_termination(r,start,it,num_exit_nodes)

			case OrTermination(l,r) =>
				check_termination(l,start,it,num_exit_nodes) || check_termination(r,start,it,num_exit_nodes)

			case _ =>
				false
		}
	}

}

/******* SOLUTION VALIDITY CONDITION ********/
case class SolutionConfiguration(
	                                allow_loop : Boolean,
	                                allow_self_loop : Boolean,
	                                allow_cap_multiple_instance : Boolean,
	                                allow_parallel_action : Boolean
                                )


