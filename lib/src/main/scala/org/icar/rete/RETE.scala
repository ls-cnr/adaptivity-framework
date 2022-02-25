package org.icar.rete

import org.icar.sublevel.{RawAdd, RawEvolution, RawRem, RawState, RawVar}
import org.icar.symbolic.ConstantTerm

import scala.collection.immutable.TreeMap

case class MatchingToken(term_list:List[ConstantTerm], dependency:List[Int])
case class Inference(v:RawVar,dependency:List[Int])
case class TermJoin(left:Int,right:Int)

abstract class InferenceTerms
case class Match(index:Int) extends InferenceTerms
case class Fix(t:ConstantTerm) extends InferenceTerms



/******* RETE ********/
class RETE(var wi : RawState) {
	var memory = new RETEMemory(wi)
	var priority_agenda: TreeMap[Int,RawVar] = TreeMap.empty
	val root:RootNode = new RootNode

	def state : RawState = memory.current
	def extend(evo:RawEvolution) : Unit = {
		for (op <- evo.evo)
			op match {
				case RawAdd(RawVar(i)) => add_fact(i)
				case RawRem(RawVar(i)) => retract_fact(i)
			}

		execute
	}

	def add_fact(index:Int) = {
		memory.current=RawState.touch(memory.current,index,true)
		root.add_fact(index,root)
	}
	def retract_fact(index:Int) = {
		memory.current=RawState.touch(memory.current,index,false)
		root.retract_fact(index,root)
	}

	def start(wi:RawState) = root.start(wi)

	def execute = {
		while (!priority_agenda.isEmpty) {
			execute_step
		}
	}

	def execute_step = {
		val priority_number = priority_agenda.firstKey
		val v = priority_agenda(priority_number)
		priority_agenda = priority_agenda - priority_number

		//println("updating fact"+v)
		memory.current=RawState.touch(memory.current,v.index,true)
		add_fact(v.index)
	}

	def insert_deduction(a:RawVar, p:Int) = {
		priority_agenda = priority_agenda + (p->a)
	}

	def remove_deduction(a:RawVar) = {
		priority_agenda = priority_agenda.filter( _._2 != a )

		/* high-priotity */
		//println("removing "+a)
		memory.current=RawState.touch(memory.current,a.index,false)
		retract_fact(a.index)
	}
}


class RETEMemory(var current : RawState) {
	var alpha_memory : Map[Int,AlphaMemory] = Map.empty
	var beta_memory : Map[Int,BetaMemory] = Map.empty
	var p_memory : Map[Int,PMemory] = Map.empty

	def copy : RETEMemory = {
		val memory = new RETEMemory(current.copy())
		for (k<-this.alpha_memory.keys){
			val copy_alpha_memory = this.alpha_memory(k).copy
			memory.alpha_memory += (k->copy_alpha_memory)
		}
		for (k<-this.beta_memory.keys){
			val copy_beta_memory = this.beta_memory(k).copy
			memory.beta_memory += (k->copy_beta_memory)
		}
		for (k<-this.p_memory.keys){
			val copy_p_memory = this.p_memory(k).copy
			memory.p_memory += (k->copy_p_memory)
		}

		memory
	}
}

/******* NODES OF THE RETE NETWORK ********/

trait RETENode {
	var subnodes : List[RETENode] = List.empty

	def add_fact(index:Int, source:RETENode):Unit = {subnodes.foreach( _.add_fact(index,this))}
	def retract_fact(index:Int, source:RETENode):Unit = {subnodes.foreach( _.retract_fact(index,this))}

	def add_token(ass:MatchingToken, source:RETENode) = {}

	def start(wi:RawState) : Unit = { subnodes.foreach( _.start(wi) ) }
}

class RootNode extends RETENode




