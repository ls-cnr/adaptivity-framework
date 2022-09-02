package org.icar.bpmn2goal

import org.icar.symbolic.HL_PredicateFormula

abstract class GatewayDirection
case class Diverging() extends GatewayDirection
case class Converging() extends GatewayDirection
case class UnspecifiedDirection() extends GatewayDirection

case class DataType(id: String, name : String)

abstract class EventDefinition
//davide
case class ErrorEventDefinition(errType: String) extends EventDefinition
case class MessageEventDefinition(mess: Message) extends EventDefinition
case class SignalEventDefinition(signal: Signal) extends EventDefinition
case class TimerEventDefinition(timertype:String, timecondition : String) extends EventDefinition
case class EmptyEventDefinition() extends EventDefinition

abstract class Item(val id: String)
case class Task(override val id: String, label: String, tasktype : String, message_opt : Option[Message] = None) extends Item(id)
case class Event(override val id: String, label : String, eventtype : String, definition : EventDefinition) extends Item(id)
case class Gateway(override val id: String, label : String, gwtype : String, direction : GatewayDirection) extends Item(id)


case class ServiceTask(override val id: String, label: String, className: String, extElems : Option[FlowableExtentionElements]) extends Item(id)
//davide
case class FlowableExtentionElements(listeners: List[FlowableExecutionListener])
//davide
case class FlowableExecutionListener(event: String, className: String)


abstract class Flow(id: String)
abstract class MessageFlow(id : String) extends Flow(id)
case class SequenceFlow(id: String, start : Item, end : Item, condition : Option[HL_PredicateFormula]) extends Flow(id)
case class InMessageFlow(id: String, receiver : Task, mess : Message) extends MessageFlow(id)
case class OutMessageFlow(id: String, sender : Task, mess : Message) extends MessageFlow(id)
case class DataInputFlow(id: String, target : Task, data : DataObjectRef) extends Flow(id)
case class DataOutputFlow(id: String, source : Task, data : DataObjectRef) extends Flow(id)
case class BoundaryFlow(id: String, source : Task, boundary : Event) extends Flow(id)

abstract class Data(id: String)
case class DataObjectRef(id: String, objecttype : DataType, state: Option[String]) extends Data(id)
case class Message(id: String, label: String) extends Data(id)
case class Signal(id:String, label:String) extends Data(id)


case class Workflow(datatypes : Array[DataType], items : Array[Item], flows : Array[Flow], data : Array[Data]) {
	def complexity : Double = {
		var c : Double=0
		for (i <- items if i.isInstanceOf[Gateway]) {
			val g : Gateway = i.asInstanceOf[Gateway]

			g.gwtype match {
				case "exclusive" =>
					val n = outgoing_flows(g).length
					c += n

				case "parallel" =>
					c += 1

				case "inclusive" =>
					val n = outgoing_flows(g).length
					c += scala.math.pow(2,n)-1

				case _ =>

			}

		}

		c
	}

	def control_flows : Array[Flow] = for (f<-flows if f.isInstanceOf[SequenceFlow]) yield f

	def outgoing_flows(item : Item) : Array[SequenceFlow] = {
		var sel_flows : List[SequenceFlow] = List.empty

		for (f <- flows if f.isInstanceOf[SequenceFlow]) {
			val seq = f.asInstanceOf[SequenceFlow]
			if (seq.start == item)
				sel_flows = seq :: sel_flows
		}

		sel_flows.toArray
	}
}
