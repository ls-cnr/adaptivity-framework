package org.icar.sublevel

import org.icar.rete.{RETE, RETEBuilder}
import org.icar.symbolic.{Axiom, CapabilityGrounding}

/******* ACTION ********/
case class RawAction(id:String,pre:RawPredicate,effects:Array[RawEvolution],invariants:List[RawPredicate],grounding:CapabilityGrounding)
case class RawEvolution(name : String, probability : Float, evo : Array[RawEvoOperator])

abstract class RawEvoOperator
case class RawAdd(add : RawVar) extends RawEvoOperator
case class RawRem(rmv : RawVar) extends RawEvoOperator


/******* STATE ********/
case class RawState(bit_descr:Array[Boolean]) {
	lazy val compact_description = calculate_compact_description
	lazy val hash : Int = bit_descr.toSeq.hashCode()

	def satisfies(v:RawVar):Boolean = bit_descr(v.index)
	def satisfies(p:RawPredicate) : Boolean = {
		p match {
			case RawVar(i) => bit_descr(i)
			case RawTT() => true
			case RawFF() => false
			case RawConj(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				satisfies(a)&&satisfies(b)
			case RawDisj(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				satisfies(a)||satisfies(b)
			case RawNeg(op) =>
				! satisfies(op.asInstanceOf[RawPredicate])
			case RawImpl(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				satisfies(RawNeg(a))||satisfies(b)
			case RawIff(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				(satisfies(a)&&satisfies(b)) || (satisfies(RawNeg(a))&&satisfies(RawNeg(b)))
			case _ => false
		}
	}

	override def toString: String = compact_description
	override def hashCode() : Int = hash
	override def equals(obj: Any): Boolean = {
		obj match {
			case that:RawState => this.hash == that.hash
			case _ => false
		}
	}

	private def calculate_compact_description = {
		var first = true
		var s ="["
		for (i<-0 until bit_descr.length)
			if (bit_descr(i)) {
				if (first)
					first = false
				else
					s+=","
				s+="x"+i
			}
		s+"]"
	}
}

object RawState {

	def empty(size:Int):RawState = RawState( Array.fill[Boolean](size)(false) )

	def factory(core:Array[Boolean],axioms:Array[Axiom],map:HL2Raw_Map) : RawState = {
		val rete : RETE = RETEBuilder.factory(axioms,map,RawState(core))
		rete.execute
		rete.memory.current
	}

	def extend(base:RawState,evo:RawEvolution) : RawState = {
		val ext: Array[Boolean] = base.bit_descr.clone()

		for (op <- evo.evo)
			op match {
				case RawAdd(RawVar(i)) => ext(i)=true
				case RawRem(RawVar(i)) => ext(i)=false
			}

		RawState(ext)
	}

	def touch(current:RawState,index:Int,value:Boolean) : RawState = {
		if (current.bit_descr(index)==value)
			current
		else {
			val ext: Array[Boolean] = current.bit_descr.clone()
			ext(index)=value
			RawState(ext)
		}
	}

}




