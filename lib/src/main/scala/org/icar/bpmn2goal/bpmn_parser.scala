package org.icar.bpmn2goal

import scala.collection.mutable.ArrayBuffer
import scala.xml.{Elem, Node, NodeSeq, XML}
import java.io.{InputStream}
import org.icar.symbolic.{FormulaParser, GroundPredicate, HL_PredicateFormula, StateOfWorld, Conjunction}




class bpmn_parser(is : InputStream) {

	var datatypes : ArrayBuffer[DataType] = ArrayBuffer()
	var dataobjects : List[DataObjectRef] = List()
	var messages : List[Message] = List()
	var signals : List[Signal] = List()
	var data : List[Data] = List()
	var items : List[Item] = List()
	var flows : List[Flow] = List()

	val node = XML.load(is)
	val w: Option[Workflow] = extract_workflow(node)



	def goals_from_InputStream : List[GoalSPEC] = {
		var goals : List[GoalSPEC] = List.empty

		if (w.isDefined) {
			val workflow = w.get
			val wfstate = new WorkflowState(workflow)
			for (i <- workflow.items if (i.isInstanceOf[Task])) {
				val task = i.asInstanceOf[Task]
				val formula = wfstate.temporal_goal(i)
				if (formula.isDefined)
					goals = GoalSPEC(task.label.replaceAll(" ","_"),formula.get) :: goals
			}
		}

		goals
	}

	def initial_state : StateOfWorld = {
		def flatten_predicate_formula(pre: HL_PredicateFormula): List[GroundPredicate] = {
			var preds : List[GroundPredicate] = List.empty

			pre match {
				case GroundPredicate(functional, terms) => preds = List(GroundPredicate(functional, terms))
				case Conjunction(formulas) => for (f<-formulas) preds = preds ::: flatten_predicate_formula(f.asInstanceOf[HL_PredicateFormula])
				case _ =>
			}

			preds
		}

		var preds : List[GroundPredicate] = List.empty
		if (w.isDefined) {
			val workflow = w.get
			val wfstate = new WorkflowState(workflow)
			for (i <- workflow.items if (i.isInstanceOf[Event])) {
				val event = i.asInstanceOf[Event]
				if (event.eventtype == "start") {
					val outs = workflow.outgoing_flows(event)
					for (o <- outs) {
						val next = o.end
						val pre: HL_PredicateFormula = wfstate.goal_trigger_condition(next)
						preds = flatten_predicate_formula(pre)
					}
				}
			}
		}

		StateOfWorld(preds)
	}

	def goal_string_from_InputStream : String = {
		var goal_string = ""

		if (w.isDefined) {
			val workflow = w.get
			val wfstate = new WorkflowState(workflow)
			for (i <- workflow.items if (i.isInstanceOf[Task])) {
				val task = i.asInstanceOf[Task]
				val pre = wfstate.goal_trigger_condition(i)
				val post = wfstate.goal_final_state(i)

				goal_string += "GOAL: " + task.label.replace("\n", " ")+"\n"
				goal_string += "\tpre: " + pre +"\n"
				goal_string += "\tpost: " + post +"\n\n"

			}
		}
		goal_string
	}

	def fullFromInputStream : String = {
		var goal_string = ""

		if (w.isDefined) {
			val workflow = w.get
			val wfstate = new WorkflowState(workflow)
			for (i <- workflow.items if (i.isInstanceOf[Task])) {
				val task = i.asInstanceOf[Task]
				val ws = wfstate.waited_state(i)
				val gs = wfstate.generated_state(i)
				val pre_inf = wfstate.predecessors_influence(i)
				val post_inf = wfstate.successors_influence(i)

				val pre = wfstate.goal_trigger_condition(i)
				val post = wfstate.goal_final_state(i)

				val goal = wfstate.temporal_goal(i)

				goal_string += "GOAL: " + task.label.replace("\n", " ")+"\n"
				goal_string += "\tws: " + ws +"\n"
				goal_string += "\tgs: " + gs +"\n"
				goal_string += "\tpre-inf: " + pre_inf +"\n"
				goal_string += "\tpost-inf: " + post_inf +"\n"
				goal_string += "\tpre-cond: " + pre +"\n"
				goal_string += "\tfinal-state: " + post +"\n"
				if (goal.isDefined)
					goal_string += "\tLTL: "+goal.get+"\n"

				goal_string += "\n"

			}

		}
		goal_string
	}

	def fromFile(filename : String) : Option[Workflow] = {
		val node = XML.loadFile(filename)
		extract_workflow(node)
	}

	private def extract_workflow(node: Elem) = {
		if (node.label == "definitions") {
			datatypes = parse_items(node \\ "dataObject")

			messages = parse_messages(node \\ "message")
			dataobjects = parse_dataobjects(node \\ "dataObjectReference") ::: parse_datastore(node \\ "dataStore")
			signals = parse_signals(node \\ "signal")

			data = messages ::: dataobjects ::: signals

			val items_task = parse_task(node \\ "task", "task")
			val items_receivetask = parse_task(node \\ "receiveTask", "receive")
			val items_sendtask = parse_task(node \\ "sendTask", "send")
			val items_usertask = parse_task(node \\ "userTask", "user")
			val items_manualtask = parse_task(node \\ "manualTask", "user")
			val items_servicetask = parse_task(node \\ "serviceTask", "services")
			val items_scripttask = parse_task(node \\ "scriptTask", "script")

			val event_start = parse_event(node \\ "startEvent", "start")
			val event_end = parse_event(node \\ "endEvent", "end")
			val event_boundary = parse_event(node \\ "boundaryEvent", "boundary")
			val intermediate = parse_event(node \\ "intermediateCatchEvent", "interm_catch")

			val exclusive_gateways = parse_gateway(node \\ "exclusiveGateway", "exclusive")
			val parallel_gateways = parse_gateway(node \\ "parallelGateway", "parallel")
			val inclusive_gateways = parse_gateway(node \\ "inclusiveGateway", "inclusive")

			items = items_task ::: items_receivetask ::: items_sendtask ::: items_usertask ::: items_manualtask ::: items_servicetask ::: items_scripttask :::
				event_start ::: event_end ::: event_boundary ::: intermediate :::
				exclusive_gateways ::: parallel_gateways ::: inclusive_gateways

			val sequence_flows = parse_sequence_flow(node \\ "sequenceFlow")
			val message_flows = parse_message_flow(node \\ "messageFlow")
			val indataflow = parse_in_data_flow(node \\ "task" ++ node \\ "receiveTask" ++ node \\ "sendTask" ++ node \\ "userTask" ++ node \\ "serviceTask", items, dataobjects)
			val outdataflow = parse_out_data_flow(node \\ "task" ++ node \\ "receiveTask" ++ node \\ "sendTask" ++ node \\ "userTask" ++ node \\ "serviceTask", items, dataobjects)
			val boundary_flows = parse_boundary_flow(node \\ "boundaryEvent", items)


			flows = sequence_flows ::: message_flows ::: indataflow ::: outdataflow ::: boundary_flows

			Some(Workflow(datatypes.toArray, items.toArray, flows.toArray, data.toArray))

		} else {

			None
		}
	}

	private def parse_items(nodes: NodeSeq) : ArrayBuffer[DataType] = {
		var l = new ArrayBuffer[DataType]()

		for (p <- nodes) {
			val id = p \ "@id"
			val name = p \ "@name"

			l += DataType(id.text,name.text)
		}

		l
	}

	private def parse_messages(nodes: NodeSeq) : List[Message] = {
		var l = List[Message]()

		for (p <- nodes) {
			val id = p \ "@id"
			val label = p \ "@name"

			l = Message(id.text,label.text) :: l
		}

		l
	}

	private def parse_signals(nodes: NodeSeq) : List[Signal] = {
		var l = List[Signal]()

		for (p <- nodes) {
			val id = p \ "@id"
			val label = p \ "@name"

			l = Signal(id.text,label.text) :: l
		}

		l
	}

	private def parse_dataobjects(nodes: NodeSeq) : List[DataObjectRef] = {
		var l = List[DataObjectRef]()

		for (p <- nodes) {
			val id = p \ "@id"
			val ref = (p \ "@dataObjectRef").text

			val state_tag = (p \ "dataState")

			val datatype = search_datatype_by_id(ref)
			if (datatype != null) {
				if (state_tag.length > 0)
					l = DataObjectRef(id.text,datatype,Some((state_tag \ "@name").text)) :: l
				else
					l = DataObjectRef(id.text,datatype,None) :: l
			}
		}

		l
	}

	private def parse_datastore(nodes: NodeSeq) : List[DataObjectRef] = {
		var l = List[DataObjectRef]()

		for (p <- nodes) {
			val id = (p \ "@id").text
			val name = (p \ "@name").text

			l = DataObjectRef(id,DataType(id,name),None) :: l
		}

		l
	}




	private def parse_task(nodes: NodeSeq, tasktype : String) : List[Task] = {
		var l = List[Task]()

		for (p <- nodes) {
			val id = p \ "@id"
			val label = p \ "@name"

			val mess_id : String = (p \ "@messageRef").text
			if (!mess_id.isEmpty) {
				val message = search_message_by_id(mess_id)
				l = Task(id.text,label.text,tasktype,Some(message)) :: l
			} else {
				l = Task(id.text,label.text,tasktype) :: l
			}

		}

		l
	}

	private def parse_event(nodes: NodeSeq, eventtype : String) : List[Event] = {
		var l = List[Event]()

		for (p <- nodes) {
			val id = p \ "@id"
			val label = p \ "@name"

			val mess_id = ((p \ "messageEventDefinition") \ "@messageRef").text
			val sign_id = ((p \ "signalEventDefinition") \ "@signalRef").text

			val duration = (p \\ "timeDuration").text
			val timedate = (p \\ "timeDate").text
			val ripet = (p \\ "timeCycle").text

			if (!mess_id.isEmpty) {
				val message = search_message_by_id(mess_id)
				l = Event(id.text, label.text, eventtype, MessageEventDefinition(message)) :: l
			} else if (!sign_id.isEmpty) {
				val signal = search_signal_by_id(sign_id)
				l = Event(id.text, label.text, eventtype, SignalEventDefinition(signal)) :: l
			} else if (!duration.isEmpty) {
				l = Event(id.text, label.text, eventtype, TimerEventDefinition("duration",duration)) :: l
			} else if (!timedate.isEmpty) {
				l = Event(id.text, label.text, eventtype, TimerEventDefinition("timedate",timedate)) :: l
			} else if (!ripet.isEmpty) {
				l = Event(id.text, label.text, eventtype, TimerEventDefinition("repetition",ripet)) :: l
			} else if (eventtype=="start" || eventtype=="end") {
				l = Event(id.text, label.text, eventtype, EmptyEventDefinition() ) :: l
			}

		}
		l
	}


	private def parse_gateway(nodes: NodeSeq, gwtype : String) : List[Gateway] = {
		var l = List[Gateway]()

		for (p <- nodes) {
			val id = p \ "@id"
			val label = p \ "@name"

			val dir : GatewayDirection = parse_gateway_direction(p)

			l = Gateway(id.text,label.text,gwtype,dir) :: l
		}

		l
	}

	def parse_gateway_direction(p: Node): GatewayDirection = {
		val ins = p \\ "incoming"
		val outs = p \\ "outgoing"

		if (ins.size==1)
			Diverging()
		else if (outs.size==1)
			Converging()
		else
			UnspecifiedDirection()
	}


	private def parse_sequence_flow(nodes: NodeSeq) : List[SequenceFlow] = {
		var l = List[SequenceFlow]()

		for (p <- nodes) {
			val id = p \ "@id"
			val start_id = (p \ "@sourceRef").text
			val end_id = (p \ "@targetRef").text

			val start = search_item_by_id(start_id)
			val end = search_item_by_id(end_id)

			val cond_tag = p \\ "conditionExpression"
			var optional_condition : Option[HL_PredicateFormula] = None
			val text = cond_tag.text
			if (text.trim.length > 0) {
				val par = new FormulaParser()
				val parsing_action = par.parseAll(par.formula,text)
				if (parsing_action.successful)
					optional_condition = Some(parsing_action.get)
			}

			l = SequenceFlow(id.text, start, end, optional_condition) :: l

		}

		l
	}

	private def parse_message_flow(nodes: NodeSeq) : List[MessageFlow] = {
		var l = List[MessageFlow]()

		for (p <- nodes) {
			val id = p \ "@id"
			val start_id = (p \ "@sourceRef").text
			val end_id = (p \ "@targetRef").text
			val message_id = (p \ "@messageRef").text

			val message = search_message_by_id(message_id)
			val sender_task = search_item_by_id(start_id)
			val receiver_task = search_item_by_id(end_id)

			if (message != null && receiver_task != null && receiver_task.isInstanceOf[Task]) {
				l = InMessageFlow(id.text,receiver_task.asInstanceOf[Task],message) :: l
			} else if (message != null && sender_task != null && sender_task.isInstanceOf[Task]) {
				l = OutMessageFlow(id.text,sender_task.asInstanceOf[Task],message) :: l
			}
		}

		l
	}

	private def parse_in_data_flow(nodes: NodeSeq, items: List[Item], dataobjects: List[DataObjectRef]) : List[DataInputFlow] = {
		var l = List[DataInputFlow]()

		for (p <- nodes) {
			val taskid = (p \ "@id").text
			val task = search_item_by_id(taskid).asInstanceOf[Task]

			val data_inputs = p \\ "dataInputAssociation"
			for (d  <- data_inputs if d != null) {
				val id = (d \ "@id").text
				val dataid = (d \ "sourceRef").text
				val data = search_dataobjectred_by_id(dataid)

				if (data != null && task != null)
					l = DataInputFlow(id,task,data) :: l
			}

		}

		l
	}

	private def parse_out_data_flow(nodes: NodeSeq, items: List[Item], dataobjects: List[DataObjectRef]) : List[DataOutputFlow] = {
		var l = List[DataOutputFlow]()

		for (p <- nodes) {
			val taskid = (p \ "@id").text
			val task = search_item_by_id(taskid).asInstanceOf[Task]

			val data_inputs = p \\ "dataOutputAssociation"
			for (d  <- data_inputs if d != null) {
				val id = (d \ "@id").text
				val dataid = (d \ "targetRef").text
				val data = search_dataobjectred_by_id(dataid)

				if (data != null && task != null)
					l = DataOutputFlow(id,task,data) :: l
			}

		}

		l
	}

	private def parse_boundary_flow(nodes: NodeSeq, items: List[Item]): List[BoundaryFlow] = {
		var l = List[BoundaryFlow]()

		for (p <- nodes) {
			val id = (p \ "@id").text
			val taskid = (p \ "@attachedToRef").text
			val task = search_item_by_id(taskid).asInstanceOf[Task]
			val event = search_item_by_id(id).asInstanceOf[Event]

			l = BoundaryFlow(id,task,event) :: l
		}

		l
	}



	private def search_item_by_id(id: String) : Item = {
		var item : Item = null
		var remaining = items

		while (remaining.length>0 && item==null) {
			if (remaining.head.id==id)
				item = remaining.head
			remaining = remaining.tail
		}

		item
	}

	private def search_datatype_by_id(id: String) : DataType = {
		var item : DataType = null
		var remaining = datatypes

		while (remaining.length>0 && item==null){
			if (remaining.head.id==id)
				item = remaining.head
			remaining = remaining.tail
		}

		item
	}

	private def search_message_by_id(id: String) : Message = {
		var item : Message = null
		var remaining = messages

		while (remaining.length>0 && item==null) {
			if (remaining.head.id==id)
				item = remaining.head
			remaining = remaining.tail
		}

		item
	}

	private def search_signal_by_id(id: String) : Signal = {
		var item : Signal = null
		var remaining = signals

		while (remaining.length>0 && item==null) {
			if (remaining.head.id==id)
				item = remaining.head
			remaining = remaining.tail
		}

		item
	}

	private def search_dataobjectred_by_id(id: String) : DataObjectRef = {
		var item : DataObjectRef = null
		var remaining = dataobjects

		while (remaining.length>0 && item==null) {
			if (remaining.head.id==id)
				item = remaining.head
			remaining = remaining.tail
		}

		item
	}
}
