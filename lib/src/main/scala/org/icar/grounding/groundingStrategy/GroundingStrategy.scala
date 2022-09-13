package org.icar.grounding.groundingStrategy

import org.icar.grounding.ConcreteCapability

/**
 * A GroundingStrategy defines how an abstract service should be associated with its realization, assuming that
 * different realizations may correspond to a specific service. For example, consider an abstract service of uploading
 * files to the cloud; some services might be GDrive, dropbox, onedrive. The corresponding grounding strategy will be
 * responsible for associating the abstract service with the most relevant (according to user-defined QoS metrics) and
 * available realization.
 *
 * @author Davide Guastella
 */
trait GroundingStrategy {

  /**
   * A strategy update is necessary when this holds a memory (or historic) containing information about, for example,
   * when capabilities have been used the last time, or how many times they have been used. This method address the
   * operations to be performed when an update of the memory is required.
   *
   * For example, think about a tabu search: in this case the capability should leave the tabu list after a
   * certain number of turns. In this case, the update method updates the number of turns that the capability has
   * passed within the list.
   */
  def update(): Unit

  /**
   * This method updates the strategy with respect to a precise realization of a service (a capability).
   * Using this method is useful when a process is running and a capability fails. For this reason, it is necessary to
   * update the grounding strategy so that when the workflow is reorganized, the capability will not be chosen in favor
   * of other capabilities that can realize the required service.
   *
   * @param cap
   */
  def updateOne(cap: ConcreteCapability): Unit

  /**
   * Apply this strategy
   *
   * @param inputCapabilities a list of [[ConcreteCapability]]
   * @return a [[ConcreteCapability]]
   */
  def apply(inputCapabilities: List[ConcreteCapability]): ConcreteCapability

}
