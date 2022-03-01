package org.icar.sublevel

object test_RawSupervisor extends App {

  val w0 = RawState(Array(false,false,false,false))
  val w1 = RawState(Array(true,false,false,false))
  val w2 = RawState(Array(true,true,false,false))
  val w3 = RawState(Array(true,true,false,true))

  def test_var_fail : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1"->RawLTLFormula(true,RawVar(2))))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_var_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawVar(1))))
    println(sup1)
    val sup1_1 = sup1.getNext(w2)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w3)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w1)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w0)
    println(sup1_4)
  }
  def test_implication_fail : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawImpl(RawVar(0),RawVar(3)))))
    println(sup1)
    val sup1_1 = sup1.getNext(w2)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w3)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w1)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w0)
    println(sup1_4)
  }
  def test_implication_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawImpl(RawVar(0),RawVar(1)))))
    println(sup1)
    val sup1_1 = sup1.getNext(w2)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w3)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w1)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w0)
    println(sup1_4)
  }
  def test_finally_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawFinally(RawVar(1)))) )
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_globally_fail : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawGlobally(RawVar(0)))) )
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_globally_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawGlobally(RawNeg(RawVar(2)))) ))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_imply_finally_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawImpl(RawVar(0),RawFinally(RawVar(3))) )))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_disj_finally_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawDisj(RawFinally(RawVar(1)),RawFinally(RawVar(3))) )))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_finally_disj_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawFinally(RawDisj(RawVar(1),RawVar(3))) )))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_conj_finally_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawConj(RawFinally(RawVar(1)),RawFinally(RawVar(3))) )))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_finally_conj_success : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawFinally(RawConj(RawVar(1),RawVar(3))) )))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }
  def test_finally_conj_fail : Unit = {
    val sup1 = RawGoalSetSupervisor(Map("1" -> RawLTLFormula(true, RawFinally(RawConj(RawVar(1),RawVar(2))) )))
    println(sup1)
    val sup1_1 = sup1.getNext(w0)
    println(sup1_1)
    val sup1_2 = sup1_1.getNext(w1)
    println(sup1_2)
    val sup1_3 = sup1_2.getNext(w2)
    println(sup1_3)
    val sup1_4 = sup1_3.getNext(w3)
    println(sup1_4)
  }

}