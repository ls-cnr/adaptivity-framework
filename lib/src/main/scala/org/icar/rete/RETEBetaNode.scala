package org.icar.rete

import org.icar.symbolic.ConstantTerm

class BetaMemory {
	var list_of_left_matching : List[MatchingToken] = List.empty
	var list_of_right_matching : List[MatchingToken] = List.empty

	def add_left_matching(m:MatchingToken) = {list_of_left_matching = m :: list_of_left_matching}
	def add_right_matching(m:MatchingToken) = {list_of_right_matching = m :: list_of_right_matching}

	def copy : BetaMemory = {
		val beta = new BetaMemory
		beta.list_of_left_matching = for (i<-list_of_left_matching) yield i
		beta.list_of_right_matching = for (i<-list_of_right_matching) yield i
		beta
	}

}

// BetaJoinNode can manage zero or multiple joins; ex: f(x,y) ^ g(x,y) OR f(x,y) ^ g(z,h)
class BetaJoinNode(rete:RETE, ID:Int, l:RETENode, r:RETENode, joins : List[TermJoin]) extends RETENode {
	//var memory : BetaMemory = new BetaMemory

	override def add_token(ass: MatchingToken, source: RETENode): Unit = {

		/* utility functions */
		def extracting_left_terms(terms:List[ConstantTerm]) : List[ConstantTerm]= {for (join<-joins) yield terms(join.left)}
		def extracting_right_terms(terms:List[ConstantTerm]) : List[ConstantTerm]= {for (join<-joins) yield terms(join.right)}
		def compare_term_list(left:List[ConstantTerm],right:List[ConstantTerm]) : Boolean = {
			var ret = true
			for (i<-0 until left.size if ret==true)
				if (left(i) != right(i))
					ret = false
			ret
		}
		def join_from_left_to_right(left_matching : MatchingToken) : Unit = {

			val left_terms = extracting_left_terms(left_matching.term_list)
			//println(s"**left=$left_matching with $left_terms **")

			for (right_matching<-rete.memory.beta_memory(ID).list_of_right_matching) {
				val right_terms = extracting_right_terms(right_matching.term_list)
				//println(s"**right=$list_of_right_matching with $right_terms **")

				if (joins.isEmpty || compare_term_list(left_terms,right_terms)) {
					//println("**matches**")
					val join_term_list = left_matching.term_list ::: right_matching.term_list
					val join_dependency = left_matching.dependency ::: right_matching.dependency
					val join_matching = MatchingToken(join_term_list,join_dependency)
					subnodes.foreach( _.add_token(join_matching,this) )
				}
			}
		}
		def join_from_right_to_left(right_matching : MatchingToken) : Unit = {

			val right_terms = extracting_right_terms(right_matching.term_list)
			//val right_term = right_matching.term_list(left_join)
			//println(s"**right=$right_matching with $right_terms **")

			for (left_matching<-rete.memory.beta_memory(ID).list_of_left_matching) {
				val left_terms = extracting_left_terms(left_matching.term_list)
				//val left_term = left_matching.term_list(left_join)
				//println(s"**left=$left_matching with $left_terms **")
				if (compare_term_list(left_terms,right_terms)) {
					//println("**matches**")
					val join_term_list = left_matching.term_list ::: right_matching.term_list
					val join_dependency = left_matching.dependency ::: right_matching.dependency
					val join_matching = MatchingToken(join_term_list,join_dependency)
					subnodes.foreach( _.add_token(join_matching,this) )
				}
			}
		}
		/* end utility functions */

		/* function starts here */
		if (source==l) {
			//println("**beta interested (from left)**")
			rete.memory.beta_memory(ID).add_left_matching(ass)
			join_from_left_to_right(ass)
		} else if (source==r) {
			//println("**beta interested (from right)**")
			rete.memory.beta_memory(ID).add_right_matching(ass)
			join_from_right_to_left(ass)
		}

	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		if (source ==l)
			rete.memory.beta_memory(ID).list_of_left_matching = rete.memory.beta_memory(ID).list_of_left_matching.filter( !_.dependency.contains(index) )
		else if (source==r)
			rete.memory.beta_memory(ID).list_of_right_matching = rete.memory.beta_memory(ID).list_of_right_matching.filter( !_.dependency.contains(index) )

		subnodes.foreach( _.retract_fact(index,this))
	}


}


class BetaConditionNode(condition:ConstantTerm=>Boolean, arg_num:Int) extends RETENode {

	override def add_token(ass: MatchingToken, source: RETENode): Unit = {
		//println(s"**beta-condition interested $ass**")
		val term = ass.term_list(arg_num)
		if (condition(term))
			subnodes.foreach( _.add_token(ass,this) )

	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		subnodes.foreach( _.retract_fact(index,this) )
	}

}

