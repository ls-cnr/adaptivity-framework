package org.icar.grounding.NETTUNIT

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}

object Test_BestFirstSolver_Repository extends CapabilityRepository {

  def aux(current_idx: Int, limit: Int): Int = current_idx match {
    case i if i < limit => {
      add(s"CAP${i}", // -> the service name
        ConcreteCapability(s"CAP${i}", // -> the service name (here we are in a concrete capability)
          s"org.com.myclassCAP${i}", // -> the class that is used to realize the service
          Some(s"com.cnr.startEventCAP${i}"))) // -> the listener executed when the BPMN task associated to this capability begins its execution
      1 + aux(current_idx + 1, limit)
    }
    case i => 0
  }

  aux(0, 50)
}