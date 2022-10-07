package org.icar.grounding.groundingStrategy

import org.icar.grounding.ConcreteCapability

import scala.collection.mutable.ListBuffer

class FirstWorkingGroundingStrategy extends GroundingStrategy {

  var notWorkingCapabilities = ListBuffer[ConcreteCapability]()

  override def update(): Unit = {}

  override def updateOne(cap: ConcreteCapability): Unit = {}

  def workingCapability(cap: ConcreteCapability): Unit = notWorkingCapabilities -= cap

  def notWorkingCapability(cap: ConcreteCapability): Unit = notWorkingCapabilities += cap

  override def apply(inputCapabilities: List[ConcreteCapability]): ConcreteCapability = {
    inputCapabilities.filter(cp => !notWorkingCapabilities.contains(cp)).headOption match {
      case cap if cap.isDefined => cap.get
      case _ => inputCapabilities.head
    }
  }
}