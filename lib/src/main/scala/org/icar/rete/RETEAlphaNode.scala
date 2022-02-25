package org.icar.rete

import org.icar.sublevel.{HL2Raw_Map, RawState, RawVar}
import org.icar.symbolic.GroundPredicate

class AlphaMemory (var tokens : Map[RawVar,Boolean]) {
	def copy : AlphaMemory = {
		val clone_tokens = for (t<-tokens) yield (t._1->t._2)
		new AlphaMemory(clone_tokens)
	}

	def add_fact_interest(v:RawVar):Boolean = {
		val interest = tokens.contains(v) && tokens(v)==false
		if (interest) tokens += (v->true)
		interest
	}
	def rmv_fact_interest(v:RawVar):Boolean = {
		val interest = tokens.contains(v) && tokens(v)==true
		if (interest) tokens += (v->false)
		interest
	}

	def setToken(i:Int,s:Boolean) = {tokens += (RawVar(i)->s)}
}


class AlphaNode(rete:RETE, ID:Int, init_list: List[RawVar], domain:HL2Raw_Map, neg : Boolean = false) extends RETENode {

	override def start(wi:RawState) : Unit = {
		for (i<-init_list if wi.bit_descr(i.index) == !neg) { // i.e: var == true for Alpha and false for NegAlpha
			val p : GroundPredicate =domain.inverse(i.index)
			val matching = MatchingToken(p.terms, List(i.index))
			subnodes.foreach( _.add_token(matching,this) )
		}
	}

	override def add_fact(index: Int, source:RETENode): Unit = {
		val v = RawVar(index)
		if (rete.memory.alpha_memory(ID).add_fact_interest(v)) {
			if (!neg) {
				val p : GroundPredicate = domain.inverse(index)
				val matching = MatchingToken(p.terms, List(index))
				subnodes.foreach( _.add_token(matching,this) )
			} else {
				subnodes.foreach( _.retract_fact(index,this) )
			}
		}
	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		val v = RawVar(index)
		if (rete.memory.alpha_memory(ID).rmv_fact_interest(v)) {
			if (neg) {
				//println("**alpha interested**")
				val p : GroundPredicate =domain.inverse(index)
				val matching = MatchingToken(p.terms, List(index))
				subnodes.foreach( _.add_token(matching,this) )
			} else {
				subnodes.foreach( _.retract_fact(index,this) )
			}
		}
	}

}
