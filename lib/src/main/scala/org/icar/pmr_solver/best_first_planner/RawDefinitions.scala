package org.icar.pmr_solver.best_first_planner

import org.icar.rete.RETEMemory
import org.icar.sublevel.{RawAction, RawGoalSetSupervisor, RawPredicate, RawState}

/******* RAW DEFINITION ********/
case class RawFrontierItem(score:Float, rete_memory:RETEMemory) extends Ordered[RawFrontierItem] {
  override def compare(that: RawFrontierItem) : Int = that.score compare this.score
}
class Expansion
case class RawExpansion(due_to : RawAction, from : RawState, probtrajectory : Array[RawEvoScenario], invariants: List[RawPredicate], external : Boolean = false)
case class RawEvoScenario(name: String, probability : Float, dest : RawFrontierItem)



/******* OLD RAW DEFINITION ********/
//case class RawFrontierItem(score:Float, rete_memory:RETEMemory, sup:RawGoalSetSupervisor) extends Ordered[RawFrontierItem] {
//  override def compare(that: RawFrontierItem) : Int = that.score compare this.score
//}
//class Expansion
//case class RawExpansion(due_to : RawAction, from : RawState, probtrajectory : Array[RawEvoScenario], invariants: List[RawPredicate], external : Boolean = false)
//case class RawEvoScenario(name: String, probability : Float, dest : RawFrontierItem)
