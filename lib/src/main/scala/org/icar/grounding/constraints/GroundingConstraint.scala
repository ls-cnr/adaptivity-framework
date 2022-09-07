package org.icar.grounding.constraints

import org.icar.grounding.ConcreteCapability

abstract class GroundingConstraint(serviceName: String, concreteCapability: ConcreteCapability) {
  def checkConstraint(cap: ConcreteCapability): Boolean
}
