package org.icar.grounding.NETTUNIT.ProcessDecorator

import org.icar.bpmn2goal.{ErrorEventDefinition, Event, EventType, Flow, Item, SequenceFlow, ServiceTask, Workflow}
import org.icar.grounding.processDecorator.ProcessDecorator

/**
 * Decorate all service task with a boundary event error that goes to a service task. This last refers to a java/scala
 * class that call MUSA for re-adapting the workflow (following a failure during the execution of a task).
 */
class FlowableBoundaryErrorEventDecorator extends ProcessDecorator {

  private val errorDef = "REQUIRE_ORCHESTRATION"
  private val className = "com.flowable.myAdaptationClass"
  val adaptationRequestTask = ServiceTask("adaptationTask", "adaptationTask", className, None)
  val endEv = Event("failureEndEvent", "failureEndEvent", EventType.End.toString, null)
  val adaptationTaskSeqFlow = SequenceFlow(s"adaptationTaskFlowEnd", adaptationRequestTask, endEv, None)

  /**
   * Decorate all ServiceTask with a BoundaryEvent
   *
   * @param items the grounded workflow items
   * @param itemID
   * @return
   */
  private def decorateItems(items: List[Item]): List[Item] = {
    def decorateItemsAux(items: List[Item], itemID: Int): List[Item] = items match {
      case (head: ServiceTask) :: tail => {
        val ev = Event(s"boundaryError_${head.id}",
          s"boundaryError_${head.label}",
          EventType.Boundary.toString,
          ErrorEventDefinition(head.id, errorDef))
        head :: ev :: decorateItemsAux(tail, itemID + 1)
      }
      case (head: Item) :: tail => head :: decorateItemsAux(tail, itemID + 1)
      case Nil => List()
    }

    decorateItemsAux(items, 0)
  }

  /**
   *
   * @param items
   * @param itemID
   * @return
   */
  private def decorateSequenceFlows(items: List[Item]): List[Flow] = {
    def decorateSequenceFlowsAux(items: List[Item], itemID: Int): List[Flow] = items match {
      case (ev: Event) :: tail if ev.eventtype == EventType.Boundary.toString => {
        val seqFlow = SequenceFlow(s"flow_boundaryError_${ev.id}", ev, adaptationRequestTask, None)
        seqFlow :: decorateSequenceFlowsAux(tail, itemID + 1)
      }
      case (_: Item) :: tail => decorateSequenceFlowsAux(tail, itemID + 1)
      case Nil => List()
    }

    decorateSequenceFlowsAux(items, 0)
  }

  override def apply(wf: Workflow): Workflow = {
    // Decorate items. Add a boundary event to each service task
    val decoratedItems = decorateItems(wf.items.toList)
    // add sequence flows from boundary event to a dummy service task responsible
    // for invoking MUSA (for re-adaptation)
    val additionalSeqFlows = decorateSequenceFlows(decoratedItems)
    // put all sequence flows together
    val seqFlows = wf.flows.toList ++ List(adaptationTaskSeqFlow) ++ additionalSeqFlows

    // return the decorated workflow
    wf.copy(wf.datatypes,
      (decoratedItems ++ List(adaptationRequestTask, endEv)).toArray,
      seqFlows.toArray,
      wf.data)
  }
}


