package org.icar.pmr_solver

import org.icar.pmr_solver.planning_domain.{AAL4E_cognitive_stimulation, GenericPlannerExecutor, IDS_like_domain, RandomlyGeneratedDomain}
import org.icar.sublevel.RawState

object Test_Solver_AAL4E extends App {
  val my_domain = AAL4E_cognitive_stimulation.my_domain
  val my_problem = AAL4E_cognitive_stimulation.my_problem
  val map = AAL4E_cognitive_stimulation.map
  val qos : RawState => Float = AAL4E_cognitive_stimulation.qos
  val conf = SolverConfiguration(
    IterationTermination(50),
    SolutionConfiguration(
      allow_self_loop = false,
      allow_cap_multiple_instance = true,
      allow_loop = true,
      allow_parallel_action = true))

  GenericPlannerExecutor.run_solver(my_problem, my_domain, qos, map, conf)

}


object Test_Solver_IDSlike extends App {
  val my_domain = IDS_like_domain.my_domain
  val my_problem = IDS_like_domain.my_problem
  val map = IDS_like_domain.map
  val qos : RawState => Float = IDS_like_domain.qos
  val conf = SolverConfiguration(
    IterationTermination(50),
    SolutionConfiguration(
      allow_self_loop = false,
      allow_cap_multiple_instance = true,
      allow_loop = true,
      allow_parallel_action = true))

  GenericPlannerExecutor.run_solver(my_problem, my_domain, qos, map, conf)

}


object Test_Solver_Random extends App {
  val rand_domain = new RandomlyGeneratedDomain(4,3 )
  val my_domain = rand_domain.my_domain
  val my_problem = rand_domain.my_problem
  val map = rand_domain.map
  val qos : RawState => Float = rand_domain.qos
  val conf = SolverConfiguration(
    //SolutionTermination(1),
    IterationTermination(100),
    SolutionConfiguration(
      allow_self_loop = false,
      allow_cap_multiple_instance = true,
      allow_loop = true,
      allow_parallel_action = true))

  GenericPlannerExecutor.run_solver(my_problem, my_domain, qos, map, conf)

}
