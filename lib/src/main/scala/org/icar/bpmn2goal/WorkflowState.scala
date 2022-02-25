package org.icar.bpmn2goal

import org.icar.symbolic.{AtomTerm, BiImplication, Conjunction, Disjunction, ExclDisj, Finally, GroundPredicate, HL_LTLFormula, HL_PredicateFormula, Implication, Negation, True, Until}

import scala.collection.mutable.ArrayBuffer

class WorkflowState(wf : Workflow) {

	def temporal_goal(item : Item) : Option[HL_LTLFormula] = {
		item match {
			case  t : Task =>
				val ws = predicate_to_temporal(task_waited_state(t))
				val gs = predicate_to_temporal(task_generated_state(t))
				val pred = predicate_to_temporal(predecessors_influence(t))
				val succ = predicate_to_temporal(successors_influence(t))

				val goal = Conjunction[HL_LTLFormula](List(
					Until(
						Negation[HL_LTLFormula](gs),
						Conjunction(List(ws,pred)
					)),
					Finally(
						Conjunction(List(
							gs,
							succ
						))
					)
				))

				Some(goal)
			case _ => None
		}

	}

	private def predicate_to_temporal(a:HL_PredicateFormula): HL_LTLFormula = {
		a match {
			case Conjunction(formula) =>
				val conv_formulas = for (f<-formula) yield predicate_to_temporal(f.asInstanceOf[HL_PredicateFormula])
				Conjunction(conv_formulas)

			case Disjunction(formula) =>
				val conv_formulas = for (f<-formula) yield predicate_to_temporal(f.asInstanceOf[HL_PredicateFormula])
				Disjunction(conv_formulas)

			case ExclDisj(formula) =>
				val conv_formulas = for (f<-formula) yield predicate_to_temporal(f.asInstanceOf[HL_PredicateFormula])
				ExclDisj(conv_formulas)

/*
			case ExclDisj(formula) =>
				val conv_formulas = for (f<-formula) yield predicate_to_temporal(f.asInstanceOf[HL_PredicateFormula])
				ExclDisj(conv_formulas)
*/

			case Implication(l,r) =>
				Implication(predicate_to_temporal(l.asInstanceOf[HL_PredicateFormula]),predicate_to_temporal(r.asInstanceOf[HL_PredicateFormula]))

			case BiImplication(l,r) =>
				BiImplication(predicate_to_temporal(l.asInstanceOf[HL_PredicateFormula]),predicate_to_temporal(r.asInstanceOf[HL_PredicateFormula]))

			case Negation(formula) =>
				Negation(predicate_to_temporal(formula.asInstanceOf[HL_PredicateFormula]))

			case _ => a.asInstanceOf[HL_LTLFormula]

		}

	}

	def waited_state(item : Item) : HL_PredicateFormula = {
		item match {
			case  t : Task => task_waited_state(t)
			case e : Event => event_waited_state(e)
			case g : Gateway => gateway_waited_state(g)
		}
	}

	def generated_state(item : Item) : HL_PredicateFormula = {
		item match {
			case  t : Task => task_generated_state(t)
			case e : Event => event_generated_state(e)
			case g : Gateway => gateway_generated_state(g)
		}
	}

	def successors_influence(item : Item) : HL_PredicateFormula = {
		var disj = ArrayBuffer[HL_PredicateFormula]()
		val out_seq = outgoing_seq(item)
		for (f <- out_seq)
			if (f.condition.isDefined)
				disj += conjunction_or_truth(inverse_propagation(f),f.condition.get)
			else
				disj += inverse_propagation(f)

		x_disjunction_or_truth(disj)
	}

	def predecessors_influence(item : Item) : HL_PredicateFormula = {
		var disj = ArrayBuffer[HL_PredicateFormula]()
		val in_seq = incoming_seq(item)
		for (f <- in_seq)
			if (f.condition.isDefined)
				disj += conjunction_or_truth(direct_propagation(f),f.condition.get)
			else
				disj += direct_propagation(f)

		x_disjunction_or_truth(disj)
	}

	def goal_trigger_condition(item : Item) : HL_PredicateFormula = {
		conjunction_or_truth(waited_state(item),predecessors_influence(item))
	}

	def goal_final_state(item : Item) : HL_PredicateFormula = {
		conjunction_or_truth(generated_state(item),successors_influence(item))
	}

	def direct_propagation(f: SequenceFlow): HL_PredicateFormula = {
		val pred = f.start
		pred match {
			case  t : Task => generated_state(t)
			case e : Event => generated_state(e)
			case g : Gateway =>
				if (g.gwtype=="exclusive") {
					var disj = ArrayBuffer[HL_PredicateFormula]()
					val in_seq = incoming_seq(g)
					for (f <- in_seq)
						if (f.condition.isDefined)
							disj += conjunction_or_truth(direct_propagation(f),f.condition.get)
						else
							disj += direct_propagation(f)

					x_disjunction_or_truth(disj)

				} else if (g.gwtype=="inclusive") {
					var conj = ArrayBuffer[HL_PredicateFormula]()
					val in_seq = incoming_seq(g)
					for (f <- in_seq)
						if (f.condition.isDefined)
							conj += conjunction_or_truth(direct_propagation(f),f.condition.get)
						else
							conj += direct_propagation(f)

					disjunction_or_truth(conj)

				} else if (g.gwtype=="parallel") {
					var conj = ArrayBuffer[HL_PredicateFormula]()
					val in_seq = incoming_seq(g)
					for (f <- in_seq)
						if (f.condition.isDefined)
							conj += conjunction_or_truth(direct_propagation(f),f.condition.get)
						else
							conj += direct_propagation(f)

					conjunction_or_truth(conj)

				} else {
					True()
				}
			case _ => True()
		}

	}

	def inverse_propagation(f: SequenceFlow): HL_PredicateFormula = {
		val pred = f.end
		pred match {
			case  t : Task => waited_state(t)
			case e : Event => waited_state(e)
			case g : Gateway =>
				if (g.gwtype=="exclusive") {
					var disj = ArrayBuffer[HL_PredicateFormula]()
					val out_seq = outgoing_seq(g)
					for (f <- out_seq)
						if (f.condition.isDefined)
							disj += conjunction_or_truth(inverse_propagation(f),f.condition.get)
						else
							disj += inverse_propagation(f)

					x_disjunction_or_truth(disj)

				} else if (g.gwtype=="inclusive") {
					var disj = ArrayBuffer[HL_PredicateFormula]()
					val out_seq = outgoing_seq(g)
					for (f <- out_seq)
						if (f.condition.isDefined)
							disj += conjunction_or_truth(direct_propagation(f),f.condition.get)
						else
							disj += inverse_propagation(f)

					disjunction_or_truth(disj)

				} else if (g.gwtype=="parallel") {
					var conj = ArrayBuffer[HL_PredicateFormula]()
					val out_seq = outgoing_seq(g)
					for (f <- out_seq)
						if (f.condition.isDefined)
							conj += conjunction_or_truth(inverse_propagation(f),f.condition.get)
						else
							conj += inverse_propagation(f)

					conjunction_or_truth(conj)

				} else {
					True()
				}
			case _ => True()
		}

	}


	private def task_waited_state(t: Item): HL_PredicateFormula = {
		var conj = ArrayBuffer[HL_PredicateFormula]()

		val data_in = expected_data(t)
		val mess_in = expected_messages(t)

		for (d<-data_in) {
			val state = if (d.state.isDefined) label(d.state.get) else "available"
			conj += GroundPredicate(state, List(AtomTerm(label(d.objecttype.name))))

		}

		for (m<-mess_in) {
			val state = "received"
			conj += GroundPredicate(state, List(AtomTerm(label(m.label))))

		}

		conjunction_or_truth(conj)
	}

	private def event_waited_state(t: Item): HL_PredicateFormula = True()

	private def gateway_waited_state(g: Gateway): HL_PredicateFormula = True()

	private def task_generated_state(t: Item): HL_PredicateFormula = {
		var normal_termination_terms = ArrayBuffer[HL_PredicateFormula]()

		val data_out = produced_data(t)
		val mess_out = produced_messages(t)

		for (d<-data_out) {
			val state = if (d.state.isDefined) label(d.state.get) else "available"
			normal_termination_terms +=  GroundPredicate(state, List(AtomTerm(label(d.objecttype.name))))
		}

		for (m<-mess_out) {
			val state = "sent"
			normal_termination_terms += GroundPredicate(state, List(AtomTerm(label(m.label))))
		}

		if (normal_termination_terms.isEmpty && t.isInstanceOf[Task]) {
			val task = t.asInstanceOf[Task]
			normal_termination_terms += GroundPredicate("done",List(AtomTerm(label(task.label))))
		}

		var normal_termination = conjunction_or_truth(normal_termination_terms)


		/* check for boundary conditions */
		var boundary_termination_terms = ArrayBuffer[HL_PredicateFormula](normal_termination)
		val boundaries = attached_bondary_conditions(t)
		for (bf <- boundaries) {
			val boundary_event = bf.boundary
			boundary_termination_terms += event_generated_state(boundary_event)
		}
		x_disjunction_or_truth(boundary_termination_terms)

	}

	private def attached_bondary_conditions(t: Item) : List[BoundaryFlow] = {
		var att : List[BoundaryFlow] = List()

		for (f <- wf.flows if f.isInstanceOf[BoundaryFlow]) {
			val bf = f.asInstanceOf[BoundaryFlow]
			if (bf.source==t)
				att = bf :: att
		}

		att
	}

	private def event_generated_state(t: Item): HL_PredicateFormula = {
		var conj = ArrayBuffer[HL_PredicateFormula]()

		if (t.isInstanceOf[Event]) {
			val event = t.asInstanceOf[Event]

			event.definition match {
				case e: EmptyEventDefinition =>

				case m: MessageEventDefinition =>
					val state = "received"
					if (!m.mess.label.isEmpty) {
						conj += GroundPredicate(state, List(AtomTerm(label(m.mess.label))) )
					} else {
						conj += GroundPredicate(state, List(AtomTerm(label(event.label))))
					}

				case s: SignalEventDefinition =>
					val state = "catched"
					conj += GroundPredicate(state, List(AtomTerm(label(s.signal.label))))

				case x: TimerEventDefinition =>
					x.timertype match {
						case "duration" =>
							if (event.eventtype=="boundary") {
								val task = search_task_via_boundary(t)
								val pre_inf = predecessors_influence(task)
								conj += GroundPredicate("after", List(AtomTerm(x.timecondition)))

							} else {
								val pre_inf = predecessors_influence(t)
								val p = GroundPredicate("after", List(AtomTerm(x.timecondition)))

							}
						case "timedate" =>
							conj += GroundPredicate("at", List(AtomTerm(x.timecondition)))

						case "repetition" =>
							conj += GroundPredicate("every", List(AtomTerm(x.timecondition)))

						case _ =>
					}
			}
		}

		conjunction_or_truth(conj)
	}

	private def search_task_via_boundary(t: Item) : Task = {
		var task : Task = null
		for (f <- wf.flows if f.isInstanceOf[BoundaryFlow]) {
			val bf = f.asInstanceOf[BoundaryFlow]
			if (bf.boundary==t)
				task = bf.source
		}
		task
	}

	private def gateway_generated_state(g: Gateway): HL_PredicateFormula = True()

	private def incoming_seq(item : Item) : List[SequenceFlow] = {
		var l = List[SequenceFlow]()

		for (f <- wf.flows if f.isInstanceOf[SequenceFlow]) {
			val flow = f.asInstanceOf[SequenceFlow]
			if (flow.end == item)
				l = flow :: l
		}

		l
	}

	private def outgoing_seq(item : Item) : List[SequenceFlow] = {
		var l = List[SequenceFlow]()

		for (f <- wf.flows if f.isInstanceOf[SequenceFlow]) {
			val flow = f.asInstanceOf[SequenceFlow]
			if (flow.start == item)
				l = flow :: l
		}

		l
	}

	private def expected_data(t: Item) : List[DataObjectRef] = {
		var l = List[DataObjectRef]()

		for (f <- wf.flows if f.isInstanceOf[DataInputFlow]) {
			val in_flow = f.asInstanceOf[DataInputFlow]
			if (in_flow.target == t)
				l = in_flow.data :: l
		}

		l
	}

	private def expected_messages(t: Item) : List[Message] = {
		var l = Set[Message]()

		/*for (f <- wf.flows if f.isInstanceOf[InMessageFlow]) {
		  val in_flow = f.asInstanceOf[InMessageFlow]
		  if (in_flow.receiver == t)
			l += in_flow.mess
		}*/

		if (t.isInstanceOf[Task]) {
			val task = t.asInstanceOf[Task]
			if (task.tasktype=="receive" && task.message_opt.isDefined) {
				val mess = task.message_opt.get
				l += mess
			}
		}

		l.toList
	}

	private def produced_data(t: Item) : List[DataObjectRef] = {
		var l = List[DataObjectRef]()

		for (f <- wf.flows if f.isInstanceOf[DataOutputFlow]) {
			val out_flow = f.asInstanceOf[DataOutputFlow]
			if (out_flow.source == t)
				l = out_flow.data :: l
		}

		l
	}

	private def produced_messages(t: Item) : List[Message] = {
		var l = Set[Message]()

		if (t.isInstanceOf[Task]) {
			val task = t.asInstanceOf[Task]
			if (task.tasktype=="send" && task.message_opt.isDefined) {
				val mess = task.message_opt.get
				l += mess
			}
		}

		l.toList
	}

	private def label(s : String) : String = s.replace("\n","_").replace(" ","_").toLowerCase

	private def disjunction_or_truth(term1:HL_PredicateFormula,term2:HL_PredicateFormula) : HL_PredicateFormula = {
		disjunction_or_truth(ArrayBuffer(term1,term2))
	}

	private def disjunction_or_truth(array : ArrayBuffer[HL_PredicateFormula]) : HL_PredicateFormula = {
		var array_no_true : scala.collection.mutable.Set[HL_PredicateFormula] = scala.collection.mutable.Set()

		for (a<-array if a != True())
			array_no_true += a

		if (array_no_true.isEmpty) {
			True()
		}else if (array_no_true.size==1) {
			array_no_true.head
		}else {
			Disjunction[HL_PredicateFormula](array_no_true.toList)
		}
	}

	private def x_disjunction_or_truth(term1:HL_PredicateFormula,term2:HL_PredicateFormula) : HL_PredicateFormula = {
		x_disjunction_or_truth(ArrayBuffer(term1,term2))
	}

	private def x_disjunction_or_truth(array : ArrayBuffer[HL_PredicateFormula]) : HL_PredicateFormula = {
		var array_no_true : scala.collection.mutable.Set[HL_PredicateFormula] = scala.collection.mutable.Set()

		for (a<-array if a != True())
			array_no_true += a

		if (array_no_true.isEmpty) {
			True()
		}else if (array_no_true.size==1) {
			array_no_true.head
		}else {
			ExclDisj[HL_PredicateFormula](array_no_true.toList)
		}
	}


	private def conjunction_or_truth(term1:HL_PredicateFormula,term2:HL_PredicateFormula) : HL_PredicateFormula = {
		conjunction_or_truth(ArrayBuffer(term1,term2))
	}

	private def conjunction_or_truth(array : ArrayBuffer[HL_PredicateFormula]) : HL_PredicateFormula = {
		var array_no_true : scala.collection.mutable.Set[HL_PredicateFormula] = scala.collection.mutable.Set()

		for (a<-array if a != True())
			array_no_true += a

		if (array_no_true.isEmpty) {
			True()
		} else if (array_no_true.size==1) {
			array_no_true.head
		}else{
			Conjunction[HL_PredicateFormula](array_no_true.toList)
		}
	}
}
