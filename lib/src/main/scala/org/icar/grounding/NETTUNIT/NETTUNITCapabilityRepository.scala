package org.icar.grounding.NETTUNIT

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}

object NETTUNITRepository extends CapabilityRepository {
  add("a", ConcreteCapability(0,"a", "aa"))
  add("b", ConcreteCapability(1,"b", "aa"))
  add("b", ConcreteCapability(2,"b", "aa"))
  add("c", ConcreteCapability(3,"c", "aa"))
  add("c", ConcreteCapability(4,"c", "cac"))
  add("c", ConcreteCapability(5,"c", "cacccc"))
  add("d", ConcreteCapability(6,"d", "cacccc"))
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
