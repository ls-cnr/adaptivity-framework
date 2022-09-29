package org.icar.nettunit_solver

import org.icar.GoalSPECParser.Goal2BPMN
import org.icar.GoalSPECParser.NETTUNIT.NETTUNITParser
import org.icar.bpmn2goal.bpmn_parser
import org.icar.grounding.NETTUNIT.{AAL4E_Repository, NETTUNITProcessDecoratorStrategy}
import org.icar.grounding.SolutionGrounder
import org.icar.grounding.groundingStrategy.TabuGroundingStrategy
import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution}
import org.icar.pmr_solver.planning_domain.GenericPlannerExecutor
import org.icar.pmr_solver.{IterationTermination, SolutionConfiguration, SolverConfiguration}
import org.icar.sublevel.RawState
import org.icar.symbolic.Problem

import java.io.ByteArrayInputStream
import scala.Console.{BLACK_B, BOLD, RESET, YELLOW}

object Test_NETTUNIT extends App {
  val bpmnProcessID = "NETTUNITProcess"

  val goalModel = NETTUNITParser.loadGoalModelFromFile("/Users/dguastel/Desktop/goaltreeNETTUNIT.txt")
  val my_problem = Problem(NETTUNITDefinitions.initial, goalModel, NETTUNITDefinitions.availableActions)
  val my_domain = NETTUNITDefinitions.my_domain
  val map = NETTUNITDefinitions.map
  val qos: RawState => Float = NETTUNITDefinitions.qos
  val conf = SolverConfiguration(
    IterationTermination(50),
    SolutionConfiguration(
      allow_self_loop = false,
      allow_cap_multiple_instance = true,
      allow_loop = true,
      allow_parallel_action = true))

  val r = GenericPlannerExecutor.run_solver(my_problem, my_domain, qos, map, conf)

  val do_only_generic = true
  if (!do_only_generic) {
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

        val wtsDot = wts.to_graphviz(s => s.toString)
        val solutionDot = wts.to_graphviz(s => s.toString)

        //val list_of_goals = testParserImpl.demo_goal()
        /*
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
        */
      }
    }
  }


}
