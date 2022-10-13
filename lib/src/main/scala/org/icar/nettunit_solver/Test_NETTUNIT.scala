package org.icar.nettunit_solver

import org.icar.GoalSPECParser.Goal2BPMN
import org.icar.GoalSPECParser.NETTUNIT.NETTUNITParser
import org.icar.bpmn2goal.Item
import org.icar.grounding.NETTUNIT.{NETTUNITProcessDecoratorStrategy, NETTUNITRepository}
import org.icar.grounding.SolutionGrounder
import org.icar.grounding.groundingStrategy.{FirstWorkingGroundingStrategy, TabuGroundingStrategy}
import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution}
import org.icar.pmr_solver.{IterationTermination, SolutionConfiguration, SolverConfiguration}
import org.icar.sublevel.RawState
import org.icar.symbolic.{GoalModel, Problem}
import scalaj.http.Http

import scala.Console.{BLACK_B, BOLD, RESET, YELLOW}

object Test_NETTUNIT /*extends App */ {
  val bpmnProcessID = "NETTUNITProcess"
  val FlowableAddress = "localhost"
  val FlowablePort = "8080"
  val deployAndExecuteFromMUSA = false
  val goalModelPath = getClass.getResource("/NETTUNIT/goaltreeNETTUNIT.txt").getFile
  val goalModel = NETTUNITParser.loadGoalModelFromFile(goalModelPath)

  /**
   *
   */
  //val groundingStrategy = new TabuGroundingStrategy(3)
  val groundingStrategy = new FirstWorkingGroundingStrategy
  val capabilityRepository = NETTUNITRepository
  val grounder = new SolutionGrounder(capabilityRepository, groundingStrategy)
  grounder.setProcessDecorator(NETTUNITProcessDecoratorStrategy)

  def main(args: Array[String]): Unit = {
    goalModel2BPMN(goalModel)
  }

  def failCapability(className: String): Unit = {
    val theCapabilities = capabilityRepository.getFromServiceImplName(className)
    theCapabilities.foreach(cap => groundingStrategy.notWorkingCapability(cap.realization))
  }

  def getSolutionItems[T <: Item : Manifest](goalModel: GoalModel): List[T] = {
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

    val solver = Solver(my_problem, my_domain, qos, conf)
    val its = solver.iterate_until_termination()
    if (!solver.solution_set.wts_list.isEmpty) {
      val wts = solver.solution_set.full_wts(0) //Get first working WTS
      val converter = new WTS2Solution(wts, my_problem.I)

      def aux(wfItems: List[Item]): List[T] = wfItems match {
        case (head: T) :: tail => head :: aux(tail)
        case _ :: tail => aux(tail)
        case Nil => List()
      }
      aux(grounder.groundSolution(converter).items.toList)
    }
    else {
      List()
    }

  }

  def goalModel2BPMN(goalModel: GoalModel, processName: String = "myBPMNProcess"): String = {
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

    val solver = Solver(my_problem, my_domain, qos, conf)
    val its = solver.iterate_until_termination()
    if (!solver.solution_set.wts_list.isEmpty) {
      val wts = solver.solution_set.full_wts(0) //Get first working WTS
      val converter = new WTS2Solution(wts, my_problem.I)

      // Abstract WF to BPMN
      val solution = grounder.groundSolution(converter)
      val theBPMN = Goal2BPMN.getBPMN(solution, processName, bpmnProcessID)
      if (deployAndExecuteFromMUSA) {
        executeWithFlowable(theBPMN.toString())
      }

      groundingStrategy.update()

      theBPMN.toString()

    }
    else {
      ""
    }

  }

  def executeWithFlowable(processDef: String): Unit = {
    Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}DEPLOYING BPMN PROCESS${RESET}")
    //replace tee with a dummy variable...
    val teeSymbol = "\u22A4"
    val repl = "${myVariable}"
    val newProcessDef = processDef.replace(teeSymbol, repl)
    val resultDeploy = Http(s"http://${FlowableAddress}:${FlowablePort}/NETTUNIT/deployProcess/${bpmnProcessID}")
      .postData(newProcessDef)
      .header("Content-Type", "application/xml").asString

    Console.out.println(s"${RESET}${BLACK_B}${YELLOW}${BOLD}EXECUTING BPMN PROCESS${RESET}")

    val body = s"{\n  \"emergencyPlanID\":\"$bpmnProcessID\",\n  \"empName\":\"safety_manager\",\n  \"requestDescription\":\"fire in refinery\"\n}"
    val resultApply = Http(s"http://${FlowableAddress}:${FlowablePort}/NETTUNIT/incident/apply")
      .postData(body)
      .header("Content-Type", "application/json").asString
  }
}
