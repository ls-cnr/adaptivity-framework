package org.icar.grounding

import org.icar.bpmn2goal.{Converging, Data, DataType, Diverging, Event, EventType, Gateway, GatewayType, Item, Task, UnspecifiedDirection, Workflow}
import org.icar.grounding.constraints.GroundingConstraint
import org.icar.pmr_solver.best_first_planner.WTS2Solution
import org.icar.symbolic.{EndEvent, ExclusiveGateway, JoinGateway, SequenceFlow, SolutionTask, SplitGateway, StartEvent, WorkflowItem}

import scala.collection.mutable.ListBuffer

/**
 *
 * @param repository
 */
class SolutionGrounder(repository: CapabilityRepository) {
  /**
   * List of constraints that can be applied when grounding tasks. These are used to avoid that certain concrete
   * capability is grounded because, for example, it doesn't satisfy a temporal constraint, or simply because the
   * service is not available.
   */
  var groundingConstraints: ListBuffer[GroundingConstraint] = ListBuffer()

  /**
   * Add a new grounding constraint.
   *
   * @param groundingConstraint
   */
  def addConstraint(groundingConstraint: GroundingConstraint): Unit = groundingConstraints.addOne(groundingConstraint)

  /**
   * Ground the input solution to a concrete solution that can be later converted into a BPMN notation and injected
   * into the workflow engine for its execution
   *
   * @param abstractWF
   * @param applyConstraints if true, the grounding constraints will be applied
   * @return
   */
  def groundSolution(abstractWF: WTS2Solution, applyConstraints: Boolean = false): Workflow = {
    //Ground all workflow items...
    val groundedTasks = groundSolutionTasks(getSolutionTasks(abstractWF), applyConstraints)
    val gateways = groundGateways(abstractWF)
    val events = groundEvents(getStartEvents(abstractWF) ++ getEndEvents(abstractWF))
    val flows = groundSequenceFlows(abstractWF.wfflow, groundedTasks ++ gateways ++ events)

    //Assemble a new "concrete" workflow
    val destWF = Workflow(Array[DataType](),
      (groundedTasks ++ gateways ++ events).toArray,
      flows.toArray,
      Array[Data]())

    //return the new workflow
    destWF
  }

  /**
   * Ground the gateways in the input abstract workflow. Gateways can be of three types: join, split or exclusive. These
   * types are are mapped to [[Gateway]], specifying their type using the enumeration [[GatewayType]]
   *
   * @param abstractWF
   * @return
   */
  def groundGateways(abstractWF: WTS2Solution): List[Item] = {
    getJoinGateways(abstractWF).map(gt => Gateway(gt.getStringID(), gt.getStringID(), GatewayType.Join.toString, Converging())) ++
      getSplitGateways(abstractWF).map(gt => Gateway(gt.getStringID(), gt.getStringID(), GatewayType.Split.toString, Diverging())) ++
      getExclusiveGateways(abstractWF).map(gt => Gateway(gt.getStringID(), gt.getStringID(), GatewayType.Exclusive.toString, UnspecifiedDirection()))
  }

  /**
   * Do the grounding of sequence flow in an abstract workflow, and returns sequence flows that can be used in a
   * (concrete) Workflow instance. All workflow items including events, tasks, gateways are also provided as
   * input: these are necessary to register start and end points in the sequence flows.
   *
   * @param flows
   * @param allItems a list including grounded [[Task]]s, [[Event]]s and [[Gateway]]s
   * @return
   */
  def groundSequenceFlows(flows: List[SequenceFlow], allItems: List[Item]): List[org.icar.bpmn2goal.SequenceFlow] = {
    def aux(flows: List[SequenceFlow], allItems: List[Item], itemID: Int): List[org.icar.bpmn2goal.SequenceFlow] = flows match {
      case Nil => List()
      case (head: SequenceFlow) :: tail =>
        aux(tail, allItems, itemID + 1) ++ List(org.icar.bpmn2goal.SequenceFlow(s"SequenceFlow_${itemID}",
          // find the "from" Item matching the head ID
          allItems.find(p => p.id == head.from.getStringID()).orNull,
          // find the "to" Item matching the head ID
          allItems.find(p => p.id == head.to.getStringID()).orNull,
          // copy the condition
          Some(head.condition)))
    }

    aux(flows, allItems, 0)
  }

  /**
   * Ground the events (start/end)
   *
   * @param eventsList
   * @return
   */
  def groundEvents(eventsList: List[WorkflowItem]): List[Item] = {
    def aux(events: List[WorkflowItem]): List[Item] = events match {
      case (head: StartEvent) :: tail => Event(head.getStringID(), head.name, EventType.Start.toString, null) :: aux(tail)
      case (head: EndEvent) :: tail => Event(head.getStringID(), head.name, EventType.End.toString, null) :: aux(tail)
      case _ :: tail => aux(tail)
      case Nil => List()
    }

    aux(eventsList)
  }

  /**
   * Do grounding for solution tasks. This takes in input a list of [[SolutionTask]] extracted from
   * an abstract workflow, and create for each task a matching [[ConcreteCapability]] instance (if any). The
   * concrete capability specifies the complete path of the java/scala class that realizes the service in the
   * solution task. The mapping between services and [[ConcreteCapability]] are in the [[CapabilityRepository]]
   * of this class.
   */
  def groundSolutionTasks(solutionTasks: List[SolutionTask], applyConstraints: Boolean): List[Item] = {
    val outputTasks = new ListBuffer[Item]()
    for (task <- solutionTasks) {
      val taskID = task.grounding.capability.id

      // find the matching services
      var concreteCapabilities = findMatchingServices(taskID)

      // apply the grounding constraints, if any/specified
      if (applyConstraints) {
        concreteCapabilities = concreteCapabilities.filter(cc => checkConstraints(cc))
      }

      if (concreteCapabilities.length > 0) {
        //Get the first available capability
        //TODO a better strategy?
        outputTasks.addOne(concreteCapabilities.head.toServiceTask())
      }

    }
    outputTasks.toList
  }

  /**
   * Check if the input capability satisfied the grounding constraints
   *
   * @param capability
   * @return
   */
  def checkConstraints(capability: ConcreteCapability): Boolean = {
    def aux(constraints: List[GroundingConstraint]): Boolean = constraints match {
      case head :: tail if head.checkConstraint(capability) => true || aux(tail)
      case _ :: tail => false || aux(tail)
      case Nil => false
    }

    aux(groundingConstraints.toList)
  }

  /**
   * Returns a list of concrete capabilities that realize the input service name
   *
   * @param serviceName
   * @return
   */
  def findMatchingServices(serviceName: String): List[ConcreteCapability] =
    repository.getFromServiceName(serviceName).map(l => l.realization)

  def getSolutionTasks(theWorkflow: WTS2Solution): List[SolutionTask] = getWorkflowItems[SolutionTask](theWorkflow)

  def getStartEvents(theWorkflow: WTS2Solution): List[WorkflowItem] = getWorkflowItems[StartEvent](theWorkflow)

  def getEndEvents(theWorkflow: WTS2Solution): List[WorkflowItem] = getWorkflowItems[EndEvent](theWorkflow)

  def getJoinGateways(theWorkflow: WTS2Solution): List[JoinGateway] = getWorkflowItems[JoinGateway](theWorkflow)

  def getSplitGateways(theWorkflow: WTS2Solution): List[SplitGateway] = getWorkflowItems[SplitGateway](theWorkflow)

  def getExclusiveGateways(theWorkflow: WTS2Solution): List[ExclusiveGateway] = getWorkflowItems[ExclusiveGateway](theWorkflow)

  //NOTE: using ":Manifest" avoid warning about type pattern T being unchecked 'since it is eliminated by erasure'
  def getWorkflowItems[T <: WorkflowItem : Manifest](theWorkflow: WTS2Solution): List[T] = {
    def aux(wfItems: List[WorkflowItem]): List[T] = wfItems match {
      case (head: T) :: tail => head.asInstanceOf[T] :: aux(tail)
      case _ :: tail => aux(tail)
      case Nil => List()
    }

    aux(theWorkflow.wfitems.toList)
  }


}
