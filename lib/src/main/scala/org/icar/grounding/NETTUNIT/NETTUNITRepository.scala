package org.icar.grounding.NETTUNIT

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}

object NETTUNITRepository extends CapabilityRepository {
  add("a", ConcreteCapability("a", "aa"))
  add("b", ConcreteCapability("b", "aa"))
  add("b", ConcreteCapability("b", "aa"))
  add("c", ConcreteCapability("c", "aa"))
  add("c", ConcreteCapability("c", "cac"))
  add("c", ConcreteCapability("c", "cacccc"))
  add("d", ConcreteCapability("d", "cacccc"))
}

object repoTest {

  def main(args: Array[String]): Unit = {
    val repo = NETTUNITRepository
    val v = repo.getFromServiceName("c")
    val v1 = repo.getFromServiceName("d")
    val v2 = repo.getFromServiceName("p")
    print("ok")
  }

}
