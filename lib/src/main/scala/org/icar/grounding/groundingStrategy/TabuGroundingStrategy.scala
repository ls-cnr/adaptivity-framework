package org.icar.grounding.groundingStrategy

import org.icar.grounding.ConcreteCapability

/**
 * Tabu Search applied to capability grounding. This strategy applies a tabu search to the choice of concrete
 * capabilities to be grounded with abstract ones, in case multiple implementations are available for a certain service.
 *
 * @param tabuTurns the number of turns that a concrete capability passes in the tabu list. When into the tabu list, a
 *                  concrete capability cannot be ground to an abstract one matching the service name.
 * @author Davide Guastella
 */
class TabuGroundingStrategy(tabuTurns: Int) extends GroundingStrategy {

  val tabuList = collection.mutable.Map.empty[ConcreteCapability, Int]

  override def update(): Unit = {
    for ((capability, turns) <- tabuList) {
      turns - 1 match {
        case i if i > 0 => tabuList.update(capability, i)
        case i if i <= 0 => tabuList.remove(capability)
      }
    }
  }

  override def updateOne(cap: ConcreteCapability): Unit = {}

  def fitness(capability: ConcreteCapability): Double = {
    val serviceAvailability = 1.0
    val qualityOfService = 1.0
    serviceAvailability * qualityOfService
  }

  override def apply(inputCapabilities: List[ConcreteCapability]): ConcreteCapability = {
    var theMap = inputCapabilities.filter(cp => !tabuList.contains(cp)).map(cp => (cp, fitness(cp))).toMap

    //backup solution: if no alternative capability is found, then choose the previous one...
    if (theMap.isEmpty){
      theMap = inputCapabilities.map(cp => (cp, fitness(cp))).toMap
    }

    val bestCapability = theMap.maxBy { case (_, fitness) => fitness }._1
    tabuList.update(bestCapability, tabuTurns)
    bestCapability
  }
}
