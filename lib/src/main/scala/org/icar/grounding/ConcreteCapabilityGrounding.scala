package org.icar.grounding

/**
 * A mapping between a service and its realization (a concrete capability)
 *
 * @param serviceName
 * @param realization
 * @author Davide Guastella
 */
case class ConcreteCapabilityGrounding(serviceName: String, realization: ConcreteCapability)
