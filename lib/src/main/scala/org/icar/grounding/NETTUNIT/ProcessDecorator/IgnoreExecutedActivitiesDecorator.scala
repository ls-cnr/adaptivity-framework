package org.icar.grounding.NETTUNIT.ProcessDecorator

import org.icar.bpmn2goal.{Diverging, Event, EventType, Flow, FlowableErrorEventDefinition, Gateway, GatewayType, Item, SequenceFlow, ServiceTask, Task}
import org.icar.grounding.NETTUNIT.NETTUNITProcessDecoratorStrategy.{adaptationRequestTask, boundaryErrorEventErrCode}
import org.icar.symbolic.{AtomTerm, Negation, True}

class IgnoreExecutedActivitiesDecorator(replaceFlows: Boolean = false) extends NETTUNITProcessDecorator(replaceFlows) {

  /**
   * Decorate all ServiceTask with a BoundaryEvent
   *
   * @param items the grounded workflow items
   * @param itemID
   * @return
   */
  def decorateItems(items: List[Item]): List[Item] = {

    def decorateItemsAux(items: List[Item], itemID: Int): List[Item] = items match {
      case (ev: Event) :: tail if ev.eventtype == EventType.End.toString =>
        ev :: decorateItemsAux(tail, itemID + 1)
      case (head: Item) :: tail =>
        val gt = Gateway(s"gt_${head.id}", s"gt_${head.id}", GatewayType.Exclusive.toString, Diverging())
        head :: gt :: decorateItemsAux(tail, itemID + 1)
      //case (head: Item) :: tail => head :: decorateItemsAux(tail, itemID + 1)
      case Nil => List()
    }

    decorateItemsAux(items, 0)
  }

  /**
   *
   * @param items list of flows
   * @param itemID
   * @return
   */
  def decorateSequenceFlows(items: List[Item], flows: List[Flow]): List[Flow] = {

    def decorateFlowsAux(items: List[Item], flows: List[Flow], itemID: Int): List[Flow] = flows match {
      case (head: SequenceFlow) :: tail =>
        val the_gt = items.find(i => i.id.equals(s"gt_${head.start.id}"))
        the_gt.isDefined match {
          case true =>
            val sq_to_gt = SequenceFlow(s"sq_${head.id}_to_gt", head.start, the_gt.get, Some(True()))
            val condition = head.end match {
              case i:ServiceTask => Negation(AtomTerm(i.label))
              case i:Task => Negation(AtomTerm(i.label))
              case i:Event => Negation(AtomTerm(i.label))
              case i=>Negation(AtomTerm(i.id))
            }

            val sq_from_gt = SequenceFlow(s"sq_${head.id}_from_gt", the_gt.get, head.end, Some(Negation(condition)))
            val next_gt = items.find(i => i.id.equals(s"gt_${head.end.id}"))
            next_gt.isDefined match {
              case true =>
                val sq_from_gt_to_other_gt = SequenceFlow(s"sq_${head.id}_to_next_gt", the_gt.get, next_gt.get, Some(condition))
                sq_to_gt :: sq_from_gt :: sq_from_gt_to_other_gt :: decorateFlowsAux(items, tail, itemID + 1)
              case false =>
                sq_to_gt :: sq_from_gt :: decorateFlowsAux(items, tail, itemID + 1)
            }
          case false =>
            decorateFlowsAux(items, tail, itemID + 1)
        }
      case (_: Item) :: tail => decorateFlowsAux(items, tail, itemID + 1)
      case Nil => List()
    }

    decorateFlowsAux(items, flows, 0)
  }
}
