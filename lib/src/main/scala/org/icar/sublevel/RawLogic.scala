package org.icar.sublevel

trait RawPredicate
case class RawVar(index:Int) extends RawPredicate with RawLTL
case class RawTT() extends RawPredicate with RawLTL
case class RawFF() extends RawPredicate with RawLTL


trait RawLTL
case class RawNext(op:RawLTL) extends RawLTL
case class RawUntil(left:RawLTL, right:RawLTL) extends RawLTL
case class RawRelease(left:RawLTL, right:RawLTL) extends RawLTL
case class RawGlobally(op:RawLTL) extends RawLTL
case class RawFinally(op:RawLTL) extends RawLTL



case class RawConj[A](left:A, right:A) extends RawPredicate with RawLTL
case class RawDisj[A](left:A, right:A) extends RawPredicate with RawLTL
case class RawNeg[A](op:A) extends RawPredicate with RawLTL
case class RawImpl[A](left:A, right:A) extends RawPredicate with RawLTL
case class RawIff[A](left:A, right:A) extends RawPredicate with RawLTL



