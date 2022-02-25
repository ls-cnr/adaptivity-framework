package org.icar.sublevel

object R2S {
	val Rinf : Float = 100000
	val Rmax : Float = 100
	val Rmin : Float = 1/100

	def metric(goal: RawLTL)(current: RawState) = calculate_resistance(current,goal)

	def calculate_goals_resistance(current: RawState, goals: Array[RawLTL]): Float = {
		var sum : Float = 0
		for (g<-goals) sum += calculate_resistance(current,g)
		sum
	}

	def calculate_resistance(current: RawState, goal: RawLTL): Float = {
		goal match {
			case v: RawVar => if (current.satisfies(v)) Rmin else Rmax
			case RawTT() => Rmin
			case RawFF() => Rinf

			case RawNeg(op) =>
				val o = op.asInstanceOf[RawLTL]
				1/calculate_resistance(current,o)
			case RawConj(left, right) =>
				val l=left.asInstanceOf[RawLTL]
				val r=right.asInstanceOf[RawLTL]
				calculate_resistance(current,l)+calculate_resistance(current,r)
			case RawDisj(left, right) =>
				val l=left.asInstanceOf[RawLTL]
				val r=right.asInstanceOf[RawLTL]
				parallel(calculate_resistance(current,l),calculate_resistance(current,r))
			case RawImpl(left, right) =>
				val l=left.asInstanceOf[RawLTL]
				val r=right.asInstanceOf[RawLTL]
				calculate_resistance(current,RawDisj(l,RawNeg(r)))
			case RawIff(left, right) =>
				val l=left.asInstanceOf[RawLTL]
				val r=right.asInstanceOf[RawLTL]
				calculate_resistance(current,RawConj(RawDisj(l,RawNeg(r)),RawDisj(r,RawNeg(l))))
			case RawNext(op) =>
				val o = op.asInstanceOf[RawLTL]
				calculate_resistance(current,o)
			case RawFinally(op) =>
				val o = op.asInstanceOf[RawLTL]
				calculate_resistance(current,o)
			case RawGlobally(op) =>
				val o = op.asInstanceOf[RawLTL]
				calculate_resistance(current,o)
			case RawUntil(left, right) =>
				val l=left.asInstanceOf[RawLTL]
				val r=right.asInstanceOf[RawLTL]
				calculate_resistance(current,l)+calculate_resistance(current,r)
			case RawRelease(left, right) =>
				val l=left.asInstanceOf[RawLTL]
				val r=right.asInstanceOf[RawLTL]
				calculate_resistance(current,l)+calculate_resistance(current,r)
			case _ => Rinf
		}
	}

	private def parallel(left: Float, right: Float): Float = {
		(left*right)/(left+right)
	}


}
