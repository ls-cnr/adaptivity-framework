package org.icar.nettunit_solver

import org.icar.GoalSPECParser.Goal2BPMN
import org.icar.GoalSPECParser.NETTUNIT.NETTUNITParser
import org.icar.bpmn2goal.bpmn_parser
import org.icar.grounding.AAL4E.AAL4E_Repository
import org.icar.grounding.NETTUNIT.{NETTUNITProcessDecoratorStrategy, NETTUNITRepository}
import org.icar.grounding.SolutionGrounder
import org.icar.grounding.groundingStrategy.TabuGroundingStrategy
import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution}
import org.icar.pmr_solver.planning_domain.GenericPlannerExecutor
import org.icar.pmr_solver.{IterationTermination, SolutionConfiguration, SolverConfiguration}
import org.icar.sublevel.RawState
import org.icar.symbolic.Problem
import scalaj.http.Http

import java.io.ByteArrayInputStream
import scala.Console.{BLACK_B, BOLD, RESET, YELLOW}

object Test_NETTUNIT extends App {
  val bpmnProcessID = "NETTUNITProcess"

  val goalModelPath = getClass.getResource("/NETTUNIT/goaltreeNETTUNIT.txt").getFile
  val goalModel = NETTUNITParser.loadGoalModelFromFile(goalModelPath)
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

  //val r = GenericPlannerExecutor.run_solver(my_problem, my_domain, qos, map, conf)

  val do_only_generic_planner = false
  if (!do_only_generic_planner) {
    val solver = Solver(my_problem, my_domain, qos, conf)
    val its = solver.iterate_until_termination()
    if (!solver.solution_set.wts_list.isEmpty) {
      val wts = solver.solution_set.full_wts(0) //Get first working WTS
      val start_time: Long = System.currentTimeMillis()
      val converter = new WTS2Solution(wts, my_problem.I)
      val end_time: Long = System.currentTimeMillis()
      val total_time: Long = end_time - start_time
      print("wts size= " + wts.nodes.size)
      println("=> time = " + total_time)

      // Abstract WF to BPMN
      val capabilityRepository = NETTUNITRepository
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

      // WF to execution (flowable)
      executeWithFlowable(theBPMN.toString())
    }
  }

  def executeWithFlowable(processDef: String): Unit = {
    Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}DEPLOYING BPMN PROCESS${RESET}")
    //replace tee with a dummy variable...
    val teeSymbol = "\u22A4"
    val repl = "${myVariable}"
    val newProcessDef = processDef.replace(teeSymbol, repl)
    val resultDeploy = Http(s"http://localhost:8080/NETTUNIT/deployProcess/${bpmnProcessID}")
      .postData(newProcessDef)
      .header("Content-Type", "application/xml").asString

    Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}EXECUTING BPMN PROCESS${RESET}")

    val body = s"{\n  \"emergencyID\":\"$bpmnProcessID\",\n  \"empName\":\"safety_manager\",\n  \"requestDescription\":\"fire in refinery\"\n}"
    //val requestBody = s"{\n  \"emergencyID\":\"$bpmnProcessID\",\n  \"requestDescription\":\"ciao\"\n}"
    val resultApply = Http(s"http://localhost:8080/NETTUNIT/incident/apply")
      .postData(body)
      .header("Content-Type", "application/json").asString
  }
}
