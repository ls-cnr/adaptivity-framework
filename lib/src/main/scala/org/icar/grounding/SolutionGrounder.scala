package org.icar.grounding

import org.icar.bpmn2goal._
import org.icar.grounding.constraints.GroundingConstraint
import org.icar.grounding.groundingStrategy.GroundingStrategy
import org.icar.grounding.processDecorator.ProcessDecoratorStrategy
import org.icar.pmr_solver.best_first_planner.WTS2Solution
import org.icar.symbolic.{SequenceFlow, _}

import scala.collection.mutable.ListBuffer

/**
 * This class "ground" an abstract workflow to a concrete one. The grounding operation maps every item within the
 * workflow to entities that can be later converted to BPMN.
 *
 * @author Davide Guastella
 * @param repository a repository of [[ConcreteCapability]] that are used to ground abstract capabilities
 * @param groundingStrategy
 */
class SolutionGrounder(repository: CapabilityRepository, groundingStrategy: GroundingStrategy) {
  /**
   *
   */
  var processDecorator: Option[ProcessDecoratorStrategy] = None

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
   * Set the decoration strategy to be applied '''*AFTER*''' grounding an abstract workflow
   *
   * @param decorator
   */
  def setProcessDecorator(decorator: ProcessDecoratorStrategy): Unit = processDecorator = Some(decorator)

  /**
   * Ground the input solution to a concrete solution that can be later converted into a BPMN notation and injected
   * into the workflow engine for its execution. The steps performed are the following:
   *
   *  1. Ground each item of the abstract workflow
   *  1. Assemble a new [[Workflow]] using the grounded items
   *  1. apply the [[ProcessDecoratorStrategy]], if any
   *
   * @param abstractWF
   * @param applyConstraints if true, the grounding constraints will be applied
   * @return
   */
  def groundSolution(abstractWF: WTS2Solution, applyConstraints: Boolean = false): Workflow = {
    //STEP 1 -> Ground items
    val groundedTasks = groundSolutionTasks(getSolutionTasks(abstractWF), applyConstraints)
    val gateways = groundGateways(abstractWF)
    val events = groundEvents(getStartEvents(abstractWF) ++ getEndEvents(abstractWF))

    val flows = groundSequenceFlows(abstractWF.wfflow, groundedTasks ++ gateways ++ events)

    //STEP 2 -> Assemble a new "concrete" workflow
    val destWF = Workflow(Array[DataType](),
      (groundedTasks ++ gateways ++ events).toArray,
      flows.toArray,
      Array[Data]())

    //STEP 3 -> Apply process decorator (if any)
    processDecorator.map(pd => pd.apply(destWF)) match {
      case Some(wf) => wf
      case _ => destWF
    }
  }

  /**
   * Ground the gateways in the input abstract workflow. Gateways can be of three types: join, split or exclusive. These
   * types are are mapped to [[Gateway]], specifying their type using the enumeration [[GatewayType]]
   *
   * @param abstractWF
   * @return
   */
  def groundGateways(abstractWF: WTS2Solution): List[Item] =
    getJoinGateways(abstractWF).map(gt => Gateway(gt.getStringID(), gt.getStringID(), GatewayType.Join.toString, Converging())) ++
      getSplitGateways(abstractWF).map(gt => Gateway(gt.getStringID(), gt.getStringID(), GatewayType.Split.toString, Diverging())) ++
      getExclusiveGateways(abstractWF).map(gt => Gateway(gt.getStringID(), gt.getStringID(), GatewayType.Exclusive.toString, UnspecifiedDirection()))

  /**
   * maps an endpoint(start/end) of a sequence flow from an abstract item ([[WorkflowItem]]) to a concrete item
   * ([[Item]] or [[Event]] or [[Gateway]]). All concrete items are assumed to be previously grounded and present
   * in the [[allItems]] list.
   *
   * @param workflowItem the abstract workflow item to be mapped
   * @param allItems     concrete workflow items ([[Item]] or [[Event]] or [[Gateway]])
   * @return Given the input abstract item (workflowItem), this function returns the corresponding concrete item
   *         (i.e., with the same ID), otherwise [[None]]
   */
  private def getSequenceFlowEndPoint(workflowItem: WorkflowItem, allItems: List[Item]): Option[Item] =
    allItems.find(st => st.id == workflowItem.getStringID())

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
        /*map the endpoints (start/end) from abstract items ([[WorkflowItem]] ) to concrete items, that is, Item or
        Event or Gateway. */
        val fromItem = allItems.find(st => st.id == head.from.getStringID()).orNull
        val toItem = allItems.find(st => st.id == head.to.getStringID()).orNull

        if (fromItem == null || toItem == null) {
          println("here")
        }

        aux(tail, allItems, itemID + 1) ++ List(org.icar.bpmn2goal.SequenceFlow(s"SequenceFlow_${itemID}",
          fromItem,
          toItem,
          Some(head.condition))) // copy the condition
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
    groundHumanTasks(solutionTasks.filter(task => task.grounding.capability.isHuman)) ++
      groundNonHumanTasks(solutionTasks.filter(task => !task.grounding.capability.isHuman), applyConstraints)
  }

  def groundHumanTasks(taskList: List[SolutionTask]): List[Item] =
    taskList.map(t => Task(s"ht_${t.id}", t.grounding.capability.id, tasktype = "human"))

  def groundNonHumanTasks(taskList: List[SolutionTask], applyConstraints: Boolean): List[Item] = {
    val outputTasks = new ListBuffer[Item]()

    for (task <- taskList) {
      val serviceName = task.grounding.capability.id
      val solutionTaskID = task.id

      // find the matching services
      var concreteCapabilities = findMatchingServices(serviceName)

      // apply the grounding constraints, if any/specified
      if (applyConstraints) {
        concreteCapabilities = concreteCapabilities.filter(cc => checkConstraints(cc))
      }

      if (concreteCapabilities.length > 0) {
        //Get the first available capability according to the chosen grounding strategy
        //NOTE: concrete capabilities are SERVICE TASKS
        val concreteCapability = groundingStrategy.apply(concreteCapabilities).withID(solutionTaskID).toServiceTask()
        outputTasks.addOne(concreteCapability)
      }
    }

    outputTasks.toList
  }

  /**
   * Check if the input capability satisfies the grounding constraints
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
