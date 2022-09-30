package org.icar.symbolic

/** An AbstractCapability is a description of a class of actions the system is
 * able to perform in order to achieve some desired results
 * It is identified by an univoque name
 * some parameters and the description of its usage
 * Example:
 * Capability OpenSwitch[switchID : Switcher] {
 * ...
 * }
 *
 * @param id      univoque name of the Capability
 * @param params  list of parameters that characterizes different resulting actions
 * @param pre     description of preconditions in order the action may be seelcted
 * @param post    description of postconditions to be true after the action execution
 * @param effects description of how the state will be affected by the actions
 * @param future  conditions that will globally hold after the action execution
 *
 */
case class AbstractCapability(
                               id: String,
                               isHuman: Boolean, //[davide] is true, then the capability that realizes this service
                               // must be carried out by a human operator
                               params: List[DomainArgument],
                               //constraints : List[DomainVariableConstraint],
                               pre: HL_PredicateFormula,
                               post: HL_PredicateFormula,
                               effects: Array[EvolutionGrounding],
                               future: List[HL_PredicateFormula]
                             )

/**
 * Builder for empty AbstractCapability
 */
object AbstractCapability {
  def empty(name: String): AbstractCapability = AbstractCapability(name, false, List.empty, True(), True(), Array(), List.empty)

  def emptyHuman(name: String): AbstractCapability = AbstractCapability(name, true, List.empty, True(), True(), Array(), List.empty)

}

/** Class used for describing how an AbstractCapability modifies the current state of the world
 * The evolution represents the changes from W(t) to W(t+1) in terms of add/remove predicates
 *
 * @param name an evolution is marked with a name
 * @param evo  specifies the evolution
 */
case class EvolutionGrounding(name: String, evo: Array[EvoOperator])

case class ProbabilisticEvolutionGrounding(
                                            name: String,
                                            probability: Float,
                                            evo: Array[EvoOperator]
                                          )

sealed abstract class EvoOperator

/** Add a new predicate into the state
 *
 * @param p predicate to be added
 */
case class AddOperator(p: Predicate) extends EvoOperator

/** Remove a predicate (if it exists) from the state
 *
 * @param p predicate to be removed
 */
case class RmvOperator(p: Predicate) extends EvoOperator

//DEPRECATED
//case class Deprec_AddEvoOperator(add : GroundPredicate) extends EvoOperator
//case class Deprec_RemoveEvoOperator(rmv : GroundPredicate) extends EvoOperator
//case class Deprec_RemoveAllEvoOperator(rmv_all : String) extends EvoOperator
