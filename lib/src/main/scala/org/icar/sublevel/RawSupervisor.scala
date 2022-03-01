package org.icar.sublevel


case class RawGoalSPECSupervisor(id:String,pre:RawLTLFormula,post:RawLTLFormula)

case class RawGoalSetSupervisor(sups : Map[String,RawLTLFormula]) {

	def check_exit_node : Boolean = {
		var exit=true
		for (s <- sups)
			if (s._2.next_ltl != RawTT() || !s._2.success)
				exit = false

		exit
	}

	def getNext(state:RawState) : RawGoalSetSupervisor = {
		val map = for (goal_map<-sups) yield goal_map._1 -> goal_map._2.next(state)
		RawGoalSetSupervisor(map)
	}


}

object RawGoalSetSupervisor {

	def factory(init:RawState, goals: Array[RawGoal]) : RawGoalSetSupervisor= {
		var map : Map[String,RawLTLFormula] = Map.empty
		for (g<-goals)
			map += (g.id -> RawLTLFormula(true,g.post))

		//val zero: Array[RawLTLSupervisor] = for (g<-goals) yield RawLTLSupervisor(true,g.raw_ltl)
		RawGoalSetSupervisor(map).getNext(init)
	}
}
