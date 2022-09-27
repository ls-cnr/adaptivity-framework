package org.icar.nettunit_solver

import org.icar.GoalSPECParser.{Goal2BPMN, testParserImpl}
import org.icar.bpmn2goal.bpmn_parser
import org.icar.grounding.NETTUNIT.{AAL4E_Repository, NETTUNITProcessDecoratorStrategy, Test_BestFirstSolver_Repository}
import org.icar.grounding.SolutionGrounder
import org.icar.grounding.groundingStrategy.TabuGroundingStrategy
import org.icar.pmr_solver.{IterationTermination, SolutionConfiguration, SolverConfiguration}
import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution}
import org.icar.pmr_solver.planning_domain.{AAL4E_cognitive_stimulation, GenericPlannerExecutor, IDS_like_domain, RandomlyGeneratedDomain}
import org.icar.service.BPMN2GoalSPEC
import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic.{Domain, Problem}
import scalaj.http.{Http, HttpOptions}

import java.io.{ByteArrayInputStream, InputStream}
import scala.Console.{BLACK_B, BOLD, RESET, YELLOW}

object Test_NETTUNIT extends App {
  val bpmnProcessID = "NETTUNITProcess"



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

  //GenericPlannerExecutor.run_solver(my_problem, my_domain, qos, map, conf)

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

      //val list_of_goals = testParserImpl.demo_goal()

      val capabilityRepository = AAL4E_Repository(my_problem.actions.sys_action.toList)
      val grounder = new SolutionGrounder(capabilityRepository, new TabuGroundingStrategy(2))
      grounder.setProcessDecorator(NETTUNITProcessDecoratorStrategy)
      val solution = grounder.groundSolution(converter)
      val theBPMN = Goal2BPMN.getBPMN(solution, "myBPMNProcess", bpmnProcessID)

      val is = new ByteArrayInputStream(theBPMN.toString().getBytes)
      val parser = new bpmn_parser(is)
      val goalString = parser.fullFromInputStream

      Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~${RESET}")
      Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}BPMN PROCESS!${RESET}")
      Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~${RESET}")
      Console.out.println(theBPMN.toString())
      Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~${RESET}")

      //executeWithFlowable(theBPMN.toString())

    }
  }
}
