package org.icar.symbolic

case class StateOfWorld (statements : List[GroundPredicate]) {
	override def toString: String = {
		var a_string: String = ""
		a_string += "["
		for (i <- statements.indices) {
			a_string += statements(i).toString
			if (i<statements.length-1)
				a_string += ","
		}
		a_string + "]"
	}
}
