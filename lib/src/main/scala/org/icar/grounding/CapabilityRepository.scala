package org.icar.grounding

import scala.collection.mutable.ListBuffer

/**
 * A capability repository contains mappings that associate service names with their realizations. More precisely, a
 * repository contains a set of [[ConcreteCapabilityGrounding]] that associate service names (the names of abstract
 * capabilities in MUSA) with concrete capabilities. A [[ConcreteCapability]] specifies, for a service, the name of the
 * Scala/Java class containing the code that realizes the service.
 *
 * @author Davide Guastella
 */
abstract class CapabilityRepository {

  private val groundings = new ListBuffer[ConcreteCapabilityGrounding]

  /**
   * Return the capability groundings that match the input service name.
   */
  def getFromServiceName(serviceName: String): List[ConcreteCapabilityGrounding] = {
    def aux(sn: String, the_groundings: List[ConcreteCapabilityGrounding]): List[ConcreteCapabilityGrounding] = the_groundings match {
      case head :: tail if head.serviceName == sn => head :: aux(sn, tail)
      case _ :: tail => aux(sn, tail)
      case Nil => List()
    }

    aux(serviceName, groundings.toList)
  }

  /**
   * Add a new capability grounding
   *
   * @param serviceName the name of the service to be grounded
   * @param realization the concrete capability grounding
   */
  def add(serviceName: String, realization: ConcreteCapability): Unit =
    groundings.addOne(ConcreteCapabilityGrounding(serviceName, realization))

}


