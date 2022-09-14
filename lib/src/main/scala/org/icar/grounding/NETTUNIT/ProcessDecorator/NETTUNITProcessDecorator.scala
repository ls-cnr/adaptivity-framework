package org.icar.grounding.NETTUNIT.ProcessDecorator

import org.icar.bpmn2goal.{Flow, Item, Workflow}
import org.icar.grounding.NETTUNIT.NETTUNITProcessDecoratorStrategy
import org.icar.grounding.NETTUNIT.NETTUNITProcessDecoratorStrategy.{adaptationEndEvent, adaptationRequestTask, adaptationTaskSeqFlow}
import org.icar.grounding.processDecorator.ProcessDecorator

abstract class NETTUNITProcessDecorator extends ProcessDecorator {

  def decorateItems(items: List[Item]): List[Item]

  def decorateSequenceFlows(items: List[Item]): List[Flow]

  override def apply(wf: Workflow): Workflow = {

    //First, filter the items so to avoid, for example, that the re-organization is decorated
    val filteredItems = NETTUNITProcessDecoratorStrategy.filterItemsToDecorate(wf.items.toList)

    val decoratedItems = NETTUNITProcessDecoratorStrategy.containsFailureItems(filteredItems) match {
      case true => decorateItems(filteredItems) //Add only the boundary events to filtered items
      case false => decorateItems(filteredItems) ++ List(adaptationRequestTask, adaptationEndEvent) //Add boundary
      //events and the end event and the re-organization task
    }

    val seqFlows = NETTUNITProcessDecoratorStrategy.containsFailureFlows(wf.flows.toList) match {
      case true => decorateSequenceFlows(decoratedItems) ++ wf.flows.toList
      case false => decorateSequenceFlows(decoratedItems) ++ wf.flows.toList ++ List(adaptationTaskSeqFlow)
    }

    // return the decorated workflow
    wf.copy(wf.datatypes, decoratedItems.toArray, seqFlows.toArray, wf.data)
  }
}
