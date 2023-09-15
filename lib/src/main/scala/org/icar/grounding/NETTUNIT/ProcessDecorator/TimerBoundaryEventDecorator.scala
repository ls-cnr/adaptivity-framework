package org.icar.grounding.NETTUNIT.ProcessDecorator

import org.icar.bpmn2goal._
import org.icar.grounding.NETTUNIT.NETTUNITProcessDecoratorStrategy.adaptationRequestTask

class TimerBoundaryEventDecorator extends NETTUNITProcessDecorator {

  val dummyTimeCondition = "PT20S"

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
        val ev = Event(s"boundaryTimer_${head.id}",
          s"boundaryTimer_${head.label}",
          EventType.Boundary.toString,
          FlowableTimerEventDefinition(head.id, dummyTimeCondition))

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
  def decorateSequenceFlows(items: List[Item], flows: List[Flow]): List[Flow] = {
    def decorateSequenceFlowsAux(items: List[Item], itemID: Int): List[Flow] = items match {
      case (ev: Event) :: tail if ev.eventtype == EventType.Boundary.toString => {
        val seqFlow = SequenceFlow(s"flow_boundaryTimer_${ev.id}", ev, adaptationRequestTask, None)
        seqFlow :: decorateSequenceFlowsAux(tail, itemID + 1)
      }
      case (_: Item) :: tail => decorateSequenceFlowsAux(tail, itemID + 1)
      case Nil => List()
    }

    decorateSequenceFlowsAux(items, 0)
  }

}
