package org.icar.pmr_solver

import org.icar.GoalSPECParser.Goal2BPMN
import org.icar.grounding.NETTUNIT.{NETTUNITProcessDecorator, Test_BestFirstSolver_Repository}
import org.icar.grounding.SolutionGrounder
import org.icar.grounding.groundingStrategy.TabuGroundingStrategy
import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution}
import org.icar.pmr_solver.planning_domain.{AAL4E_cognitive_stimulation, GenericPlannerExecutor, IDS_like_domain, RandomlyGeneratedDomain}
import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic.{Domain, Problem}

import scala.Console.{BLACK_B, BOLD, RESET, YELLOW}

object Test_Solver_AAL4E extends App {
  val my_domain = AAL4E_cognitive_stimulation.my_domain
  val my_problem = AAL4E_cognitive_stimulation.my_problem
  val map = AAL4E_cognitive_stimulation.map
  val qos: RawState => Float = AAL4E_cognitive_stimulation.qos
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
  val qos: RawState => Float = IDS_like_domain.qos
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

  for (size <- 1 to 5) {
    for (t <- 1 to 10) {
      val rand_domain = new RandomlyGeneratedDomain(3 + size, 3)
      val my_domain = rand_domain.my_domain
      val my_problem = rand_domain.my_problem
      val map = rand_domain.map
      val qos: RawState => Float = rand_domain.qos
      val conf = SolverConfiguration(
        IterationTermination(100),
        SolutionConfiguration(
          allow_self_loop = false,
          allow_cap_multiple_instance = true,
          allow_loop = true,
          allow_parallel_action = true))

      test_converter(my_problem, my_domain, qos, map, conf)
    }
  }

  def test_converter(my_problem: Problem, my_domain: Domain, qos: RawState => Float, map: HL2Raw_Map, conf: SolverConfiguration): Unit = {
    /* the solver */
    val solver = Solver(my_problem, my_domain, qos, conf)
    val its = solver.iterate_until_termination()
    if (!solver.solution_set.wts_list.isEmpty) {
      for (wts <- solver.solution_set.full_wts) {
        val start_time: Long = System.currentTimeMillis()
        val converter = new WTS2Solution(wts, my_problem.I)
        val end_time: Long = System.currentTimeMillis()
        val total_time: Long = end_time - start_time
        print("wts size= " + wts.nodes.size)
        println("=> time = " + total_time)


        val capabilityRepository = Test_BestFirstSolver_Repository(my_problem.actions.sys_action.toList)
        val grounder = new SolutionGrounder(capabilityRepository, new TabuGroundingStrategy(2))
        grounder.setProcessDecorator(NETTUNITProcessDecorator)
        val solution = grounder.groundSolution(converter)
        val theBPMN = Goal2BPMN.getBPMN(solution, "myBPMNProcess", "process_0")

        Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~${RESET}")
        Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}BPMN PROCESS!${RESET}")
        Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~${RESET}")
        Console.out.println(theBPMN.toString())
        Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~${RESET}")

      }
    }
  }

}
