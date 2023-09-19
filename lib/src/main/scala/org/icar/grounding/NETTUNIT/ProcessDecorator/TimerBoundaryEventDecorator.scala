package org.icar.grounding.NETTUNIT.ProcessDecorator

import org.DEMO.NETTUNITRepositoryDEMO
import org.icar.bpmn2goal._
import org.icar.grounding.NETTUNIT.NETTUNITProcessDecoratorStrategy.adaptationRequestTask

class TimerBoundaryEventDecorator extends NETTUNITProcessDecorator {

  val dummyTimeCondition = "PT20S"

  def getTimeConditionForTask(taskLabel: String): String = {
    taskLabel match {
      case "involve_pertinent_roles_ct_mayor" => "PT30S"
      case _ => "PT3H"
    }
  }


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
        //Do I have a time constraint for the task?

        val abst = NETTUNITRepositoryDEMO.getFromServiceImplName(head.className)
        abst
        if (getTimeConditionForTask(abst.head.serviceName).isEmpty) {
          //No, do not add any timer
          head :: decorateItemsAux(tail, itemID + 1)
        }
        else {
          //Yes, add the boundary event and decorate the task
          val ev = Event(s"boundaryTimer_${head.id}",
            s"boundaryTimer_${head.label}",
            EventType.Boundary.toString,
            FlowableTimerEventDefinition(head.id, /*dummyTimeCondition*/ getTimeConditionForTask(head.label)))
          head :: ev :: decorateItemsAux(tail, itemID + 1)
        }

      }
      case (head: TriggerableServiceTask) :: tail => {
        if (getTimeConditionForTask(head.label).isEmpty)
          head :: decorateItemsAux(tail, itemID + 1)
        else {
          val ev = Event(s"boundaryTimer_${head.id}",
            s"boundaryTimer_${head.label}",
            EventType.Boundary.toString,
            FlowableTimerEventDefinition(head.id, /*dummyTimeCondition*/ getTimeConditionForTask(head.label)))
          head :: ev :: decorateItemsAux(tail, itemID + 1)
        }
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
