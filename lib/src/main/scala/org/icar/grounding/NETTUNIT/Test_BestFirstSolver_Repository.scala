package org.icar.grounding.NETTUNIT

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}
import org.icar.symbolic.AbstractCapability

import scala.annotation.tailrec

case class Test_BestFirstSolver_Repository(capabilities: List[AbstractCapability]) extends CapabilityRepository {

  @tailrec
  final def aux(capabilities: List[AbstractCapability]): Unit = capabilities match {
    case (cap: AbstractCapability) :: tail => {
      for (typeID <- 1 to 10) {
        add(cap.id, // -> the service name
          ConcreteCapability(typeID,
            cap.id, // -> the service name (here we are in a concrete capability)
            false,
            cap.id, // -> the title of the concrete capability
            s"org.com.myclass_${cap.id}_type${typeID}", // -> the class that is used to realize the service
            Some(s"com.cnr.startEvent_${cap.id}_type${typeID}"))) // -> the listener executed when the BPMN task associated to this capability begins its execution
      }
      aux(tail)
    }
    case _ :: _ =>
    case Nil =>
  }

  aux(capabilities)
}