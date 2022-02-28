package org.icar.sublevel

case class RawLTLSupervisor(success : Boolean,next_ltl : RawLTL)

case class RawGoalModelSupervisor(sups : Map[String,RawLTLSupervisor]) {

	def check_exit_node : Boolean = {
		var exit=true
		for (s <- sups)
			if (s._2.next_ltl != RawTT() || !s._2.success)
				exit = false

//		var exit=false
//		for (s <- sups)
//			if (s.next_ltl == RawTT() && s.success)
//				exit = true

		exit
	}

	def getNext(state:RawState) : RawGoalModelSupervisor = {
		val map = for (goal_map<-sups) yield goal_map._1 -> compute_next(state,goal_map._2.next_ltl)
		RawGoalModelSupervisor(map)
	}

	private def compute_next(state : RawState, formula : RawLTL) : RawLTLSupervisor = {

		formula match {
			case RawTT() => RawLTLSupervisor(true, RawTT())
			case RawNeg(RawTT()) => compute_next(state,RawFF())

			case RawFF() => RawLTLSupervisor(false, RawFF())
			case RawNeg(RawFF()) => compute_next(state,RawTT())

			case RawVar(i) =>
				if (state satisfies RawVar(i))
					RawLTLSupervisor(true, RawTT())
				else
					RawLTLSupervisor(false, RawFF())

			case RawNeg(RawVar(i)) =>
				if (state satisfies RawVar(i))
					RawLTLSupervisor(false, RawFF())
				else
					RawLTLSupervisor(true, RawTT())

			case RawConj(l, r) =>
				val a = l.asInstanceOf[RawLTL]
				val b = r.asInstanceOf[RawLTL]
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)

				if (next_a.next_ltl != RawTT() && next_b.next_ltl != RawTT())
					RawLTLSupervisor(next_a.success && next_b.success, RawConj(next_a.next_ltl, next_b.next_ltl))

				else if (next_b.next_ltl != RawTT())
					RawLTLSupervisor(next_a.success && next_b.success, next_b.next_ltl)

				else if (next_a.next_ltl != RawTT())
					RawLTLSupervisor(next_a.success && next_b.success, next_a.next_ltl)

				else
					RawLTLSupervisor(next_a.success && next_b.success, RawTT())

			case RawNeg(RawConj(a, b)) => compute_next(state,RawDisj(RawNeg(a), RawNeg(b)))

			case RawDisj(l, r) =>
				val a = l.asInstanceOf[RawLTL]
				val b = r.asInstanceOf[RawLTL]
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)

				val a_test = next_a.success
				val next_a_formula = next_a.next_ltl

				val b_test = next_b.success
				val next_b_formula = next_b.next_ltl

				if (next_a_formula != RawTT() && next_b_formula != RawTT())
					RawLTLSupervisor(a_test || b_test, RawDisj(next_a_formula, next_b_formula))

//				else if (next_b_formula != RawTT())
//					RawLTLSupervisor(a_test || b_test, next_b_formula)
//
//				else if (next_a_formula != RawTT())
//					RawLTLSupervisor(a_test || b_test, next_a_formula)

				else
					RawLTLSupervisor(a_test || b_test, RawTT())

			case RawNeg(RawDisj(a, b)) => compute_next(state,RawConj(RawNeg(a), RawNeg(b)))

			case RawImpl(l, r) =>
				val a = l.asInstanceOf[RawLTL]
				val b = r.asInstanceOf[RawLTL]

				val next_a = compute_next(state,a)
				if (next_a.success) {
					val next_b = compute_next(state,b)
					RawLTLSupervisor(next_a.success && next_b.success, next_b.next_ltl)
				} else
					RawLTLSupervisor(true, RawImpl(l, r) )

			case RawNext(f) =>
				RawLTLSupervisor(true, f)

			case RawNeg(RawNext(f)) => compute_next(state,RawNext(RawNeg(f)))

			case RawUntil(a, b) =>
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)
				val a_test = next_a.success
				val next_a_formula = next_a.next_ltl
				val b_test = next_b.success
				val next_b_formula = next_b.next_ltl

				if (b_test)
					RawLTLSupervisor(true, RawTT())
				else if (a_test)
					RawLTLSupervisor(true, RawUntil(a, b))
				else
					RawLTLSupervisor(false, RawFF())

			case RawNeg(RawUntil(a, b)) => compute_next(state, RawRelease(RawNeg(a),RawNeg(b)))

			case RawRelease(a, b) =>
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)
				val a_test = next_a.success
				val next_a_formula = next_a.next_ltl
				val b_test = next_b.success
				val next_b_formula = next_b.next_ltl

				if (b_test) {
					if (a_test)
						RawLTLSupervisor(true, RawTT())
					else
						RawLTLSupervisor(true, RawRelease(a, b))
				} else
					RawLTLSupervisor(false,RawFF())

			case RawNeg(RawRelease(a, b)) => compute_next(state,RawNext(RawUntil(RawNeg(a),RawNeg(b))))

			case RawFinally(f) => compute_next(state,RawUntil(RawTT(),f))
			case RawNeg(RawFinally(f)) => compute_next(state,RawNeg(RawUntil(RawTT(),f)))

			case RawGlobally(f) => compute_next(state,RawNeg(RawFinally(RawNeg(f))))
			case RawNeg(RawGlobally(f)) => compute_next(state,RawFinally(RawNeg(f)))

			case RawNeg(RawNeg(f)) => compute_next(state,f.asInstanceOf[RawLTL])

			case _ => RawLTLSupervisor(false,RawFF())

		}

	}

}

object RawGoalModelSupervisor {

	def factory(init:RawState, goals: Array[RawGoal]) : RawGoalModelSupervisor= {
		var map : Map[String,RawLTLSupervisor] = Map.empty
		for (g<-goals)
			map += (g.id -> RawLTLSupervisor(true,g.raw_ltl))

		//val zero: Array[RawLTLSupervisor] = for (g<-goals) yield RawLTLSupervisor(true,g.raw_ltl)
		RawGoalModelSupervisor(map).getNext(init)
	}
}
