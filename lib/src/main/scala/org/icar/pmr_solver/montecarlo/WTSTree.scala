package org.icar.pmr_solver.montecarlo

import java.io.{File, PrintWriter}
import org.icar.rete.{RETE, RETEMemory}
import org.icar.sublevel.{R2S, RawAction, RawGoal, RawGoalModelSupervisor, RawLTL, RawState, RawTT}

class WTSTreeNode(val my_tree : WTSTree, val parent:WTSTreeNode, val state : RETEMemory, val supervisor : RawGoalModelSupervisor,val legal_actions : Array[RawAction]) {

	val id = WTSTreeNode.next_id
	val children : Array[Option[WTSTreeNode]] = Array.fill(legal_actions.length)(None)
	val r2s : Float = calculate_resistance(state.current,my_tree.specifications)
	var delta : Boolean = false

	var visit : Int = 0
	var win : Float = 0

	def get_child(index:Int) : WTSTreeNode = {
		require(index<children.size)
		if (children(index).isDefined)
			children(index).get
		else {
			children(index) = Some(calculate_node(index))
			children(index).get
		}
	}

	def isExit : Boolean = {
		var exit=true
		for (s <- supervisor.sups)
			if (s._2.next_ltl != RawTT() || !s._2.success)
				exit = false

		exit
	}

	def is_nonterminal: Boolean = !legal_actions.isEmpty && !isExit
	def is_notfullyexpanded: Boolean = children.contains(None)
	def is_root : Boolean = (parent==null)

	def untried_actions: Array[Int] = {
		val untried = for (index <- 0 until legal_actions.length if !children(index).isDefined) yield index
		untried.toArray
	}


	private def calculate_node(index: Int) : WTSTreeNode = {
		val action = legal_actions(index)
		require(action.effects.size==1)

		val trajectory = action.effects(0)

		my_tree.rete.memory = state.copy
		my_tree.rete.extend(trajectory)
		val node = my_tree.rete.state
		val next = supervisor.getNext(node)

		val updated_actions = legal_actions.filter( _!=action )
		val updated_applicable_actions : Array[RawAction] = for (a<-updated_actions if my_tree.rete.state.satisfies(a.pre)) yield a

		new WTSTreeNode(my_tree,this,my_tree.rete.memory,next,updated_applicable_actions)
	}

	private def calculate_resistance(current: RawState, specifications: Array[RawGoal]): Float = {
		var r2s : Float = 0
		for (s<-specifications) r2s += R2S.calculate_resistance(current,s.raw_ltl)

		r2s
	}

	def to_graphviz_body(pretty_string: RawState => String) : String = {
		//var string = "\""+id+"_"+pretty_string(state.current)+"\""
		var string = "N"+id
		val label = "\""+win+"/"+visit+"("+r2s+")\""
		if (isExit)
			string += s"[label=$label,style=bold,color=green];\n"
		else if (delta==true)
			string += s"[label=$label,style=bold,color=blue];\n"
		else
			string += s"[label=$label,color=black];\n"

		for (index<-0 until children.length if children(index).isDefined) {
			val mychild : WTSTreeNode = children(index).get
			val child_state: RawState = mychild.state.current

			val act = legal_actions(index)
			//string += "\""+id+"_"+pretty_string(state.current)+"\""
			string += "N"+id
			string += "->"
			//string += "\""+mychild.id+"_"+pretty_string(child_state)+"\""
			string += "N"+mychild.id
			string += "[label=\""+act.id+"\"];\n"

			string += mychild.to_graphviz_body(pretty_string)
		}

		string
	}

}

object WTSTreeNode {
	var id = 0
	def next_id : Int = {
		val ret = id
		id+=1
		ret
	}
}

class WTSTree(val rete : RETE, val available_actions : Array[RawAction], val specifications: Array[RawGoal]) {
	val root = generate_node()

	def generate_node() : WTSTreeNode = {
		val init_supervisor = RawGoalModelSupervisor.factory(rete.state,specifications)
		val applicable_actions : Array[RawAction] = for (a<-available_actions if rete.state.satisfies(a.pre)) yield a
		new WTSTreeNode(this,null,rete.memory,init_supervisor,applicable_actions)
	}

	def to_graphviz(pretty_string: RawState => String) : String = {
		var string = "digraph WTS {\n"
		string += root.to_graphviz_body(pretty_string)
		string += "}\n"

		string
	}

	def update_wts_file(file:String) : Unit = {
		val pw = new PrintWriter(new File(file))
		pw.write(to_graphviz(s=>s.toString))
		pw.close
	}

}

