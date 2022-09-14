package org.icar.grounding.constraints

import org.icar.grounding.ConcreteCapability

/**
 * TODO
 * @param serviceName
 * @param concreteCapability
 */
abstract class GroundingConstraint(serviceName: String, concreteCapability: ConcreteCapability) {
  def checkConstraint(cap: ConcreteCapability): Boolean
}
