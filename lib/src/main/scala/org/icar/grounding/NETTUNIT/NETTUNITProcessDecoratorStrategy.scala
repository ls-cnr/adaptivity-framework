package org.icar.grounding.NETTUNIT

import org.icar.bpmn2goal._
import org.icar.grounding.NETTUNIT.ProcessDecorator.{BoundaryErrorEventDecorator, TimerBoundaryEventDecorator}
import org.icar.grounding.processDecorator.ProcessDecoratorStrategy

object NETTUNITProcessDecoratorStrategy extends ProcessDecoratorStrategy {

  val boundaryErrorEventErrCode = "REQUIRE_ORCHESTRATION"
  val readaptationClassName = "com.flowable.myAdaptationClass"
  val adaptationRequestTask = ServiceTask("adaptationTask", "adaptationTask", readaptationClassName, None)
  val adaptationEndEvent = Event("failureEndEvent", "failureEndEvent", EventType.End.toString, null)
  val adaptationTaskSeqFlow = SequenceFlow(s"adaptationTaskFlowEnd", adaptationRequestTask, adaptationEndEvent, None)

  def filterItemsToDecorate(items: List[Item]): List[Item] = {
    def aux(items: List[Item]): List[Item] = items match {
      case (head: ServiceTask) :: tail if head.id == adaptationRequestTask.id => aux(tail)
      case (head: Event) :: tail if head.id == adaptationEndEvent.id => aux(tail)
      case head :: tail => head :: aux(tail)
      case Nil => List()
    }

    aux(items)
  }

  def containsFailureFlows(flows: List[Flow]): Boolean = {
    def aux(flows: List[Flow]): Boolean = flows match {
      case (head: SequenceFlow) :: tail if head.id == adaptationTaskSeqFlow.id => true || aux(tail)
      case _ :: tail => false || aux(tail)
      case Nil => false
    }

    aux(flows)
  }

  def containsFailureItems(items: List[Item]): Boolean = {
    def aux(items: List[Item]): Boolean = items match {
      case (head: ServiceTask) :: tail if head.id == adaptationRequestTask.id => true || aux(tail)
      case (head: Event) :: tail if head.id == adaptationEndEvent.id => true || aux(tail)
      case _ :: tail => false || aux(tail)
      case Nil => false
    }

    aux(items)
  }

  addDecorator(new BoundaryErrorEventDecorator)
  addDecorator(new TimerBoundaryEventDecorator)
}

