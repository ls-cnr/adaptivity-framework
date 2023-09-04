package org.sfax

import org.icar.GoalSPECParser.Goal2BPMN
import org.icar.GoalSPECParser.NETTUNIT.NETTUNITParser
import org.icar.bpmn2goal.Item
import org.icar.grounding.NETTUNIT.{NETTUNITProcessDecoratorStrategy, NETTUNITRepository}
import org.icar.grounding.SolutionGrounder
import org.icar.grounding.groundingStrategy.FirstWorkingGroundingStrategy
import org.icar.nettunit_solver.NETTUNITDefinitions
import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution}
import org.icar.pmr_solver.{IterationTermination, SolutionConfiguration, SolverConfiguration}
import org.icar.sublevel.RawState
import org.icar.symbolic.{GoalModel, Problem}
import scalaj.http.Http

import scala.Console.{BLACK_B, BOLD, RESET, YELLOW}

object Test_NETTUNIT_sfax /*extends App */ {
  val bpmnProcessID = "NETTUNITProcess"
  val FlowableAddress = "localhost"
  val FlowablePort = "8080"
  val deployAndExecuteFromMUSA = false
  val goalModelPath = getClass.getResource("/NETTUNIT/goaltreeNETTUNIT.txt").getFile


  /*the grounding strategy (to bind abstract to concrete capabilities)*/
  val groundingStrategy = new FirstWorkingGroundingStrategy
  /*the concrete capabilities repository*/
  val capabilityRepository = NETTUNITRepository
  val grounder = new SolutionGrounder(capabilityRepository, groundingStrategy)
  grounder.setProcessDecorator(NETTUNITProcessDecoratorStrategy)

  /**
   * ENTRY POINT
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val goalModel = NETTUNITParser.loadGoalModelFromFile(goalModelPath)
    goalModel2BPMN(goalModel)
  }

  def goalModel2BPMN(goalModel: GoalModel, processName: String = "CrossBorderEmergencyPlan"): String = {
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
    if (solver.solution_set.wts_list.nonEmpty) {
      val wts = solver.solution_set.full_wts(0) //Get first working WTS
      val converter = new WTS2Solution(wts, my_problem.I)

      // Abstract WF to BPMN
      val solution = grounder.groundSolution(converter)
      val theBPMN = Goal2BPMN.getBPMN(solution, processName, bpmnProcessID)

      // BPMN to XML
      val str = theBPMN.toString()
      print("output BPMN:\n"+str)
      theBPMN.toString()
    }
    else {
      ""
    }

  }

  def failCapability(className: String): Unit = {
    val theCapabilities = capabilityRepository.getFromServiceImplName(className)
    theCapabilities.foreach(cap => groundingStrategy.notWorkingCapability(cap.realization))
  }

  def restoreCapability(className: String): Unit = {
    val theCapabilities = capabilityRepository.getFromServiceImplName(className)
    theCapabilities.foreach(cap => groundingStrategy.workingCapability(cap.realization))
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
    if (solver.solution_set.wts_list.nonEmpty) {
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


}
