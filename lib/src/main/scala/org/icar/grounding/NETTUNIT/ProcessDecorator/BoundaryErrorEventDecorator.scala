package org.icar.grounding.NETTUNIT.ProcessDecorator

import org.icar.bpmn2goal._
import org.icar.grounding.NETTUNIT.NETTUNITProcessDecoratorStrategy.{adaptationRequestTask, boundaryErrorEventErrCode}

/**
 * Decorate all service task with a boundary event error that goes to a service task. This last refers to a java/scala
 * class that call MUSA for re-adapting the workflow (following a failure during the execution of a task).
 */
class BoundaryErrorEventDecorator extends NETTUNITProcessDecorator {

  /**
   * Decorate all ServiceTask with a BoundaryEvent
   *
   * @param items the grounded workflow items
   * @param itemID
   * @return
   */
  def decorateItems(items: List[Item]): List[Item] = {
    def decorateItemsAux(items: List[Item], itemID: Int): List[Item] = items match {
      case (head: ServiceTask) :: tail => {
        val ev = Event(s"boundaryError_${head.id}",
          s"boundaryError_${head.label}",
          EventType.Boundary.toString,
          FlowableErrorEventDefinition(head.id, boundaryErrorEventErrCode))
        head :: ev :: decorateItemsAux(tail, itemID + 1)
      }
      case (head: TriggerableServiceTask) :: tail => {
        val ev = Event(s"boundaryError_${head.id}",
          s"boundaryError_${head.label}",
          EventType.Boundary.toString,
          FlowableErrorEventDefinition(head.id, boundaryErrorEventErrCode))
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
  def decorateSequenceFlows(items: List[Item], flows:List[Flow]): List[Flow] = {
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
}


