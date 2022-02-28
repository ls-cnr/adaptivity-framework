package org.icar.pmr_solver.montecarlo

import org.icar.pmr_solver.best_first_planner.TerminationDescription
import org.icar.symbolic.{Domain, Problem}
import org.icar.rete.RETEBuilder
import org.icar.sublevel.{HL2Raw_Map, RawGoal, RawLTL, RawState}

import scala.util.Random

class NMCSolver(val problem: Problem,val domain: Domain) {
	val random = new Random()

	val map = new HL2Raw_Map(domain)

	val I = RawState.factory(map.state_of_world(problem.I.statements),domain.axioms,map)
	val rete = RETEBuilder.factory(domain.axioms,map,I)
	rete.execute

	val specifications: Array[RawGoal] = for (g<-problem.goal_model.goals) yield map.goal_spec(g)
	//val specifications: Array[RawLTL] = for (g<-problem.goal_model.goals) yield map.ltl_formula(g)
	val available_actions = (for (a<-problem.actions.sys_action) yield map.system_action(a)).flatten

	var tree = new WTSTree(rete,available_actions,specifications)

	var solutions = 0
	var elapsed : Long= 0

	def mcts(term:TerminationDescription) = {
		tree = new WTSTree(rete,available_actions,specifications)
		WTSTreeNode.id = 0
		solutions=0

		var iterations=0
		val start_timestamp: Long = System.currentTimeMillis

		while (!TerminationDescription.check_termination(term,start_timestamp,iterations,solutions)) {
			val focus_node = tree_policy (tree.root)
			val win = simulation_policy (focus_node)
			backpropagation (focus_node, win)
			iterations += 1
		}

		val end_timestamp: Long = System.currentTimeMillis
		elapsed = end_timestamp-start_timestamp
	}

	def tree_policy(node: WTSTreeNode):WTSTreeNode = {
		var focus = node
		var new_node = node
		while (new_node==node && focus.is_nonterminal) {
			if (focus.is_notfullyexpanded) {
				new_node = expand(focus)
			} else {
				focus = best_child(focus)
			}
		}
		if (new_node != node)
			new_node
		else
			focus
	}
	def expand(node: WTSTreeNode):WTSTreeNode = {
		val untried : Array[Int] = node.untried_actions
		val i : Int = random.nextInt(untried.size)
		val child = node.get_child(untried(i))
		if (child.isExit) solutions += 1
		child
	}
	def best_child(node: WTSTreeNode): WTSTreeNode = {
		var best = node
		var best_score : Float = -1
		for (some_child<-node.children if some_child.isDefined) {
			val child = some_child.get
			val win = child.win
			val visit = child.visit.toFloat
			val R : Float = if (child.visit>0) win/visit else 0
			if (R>best_score) {
				best_score = R
				best = child
			}
		}
		best
	}
	def simulation_policy(node: WTSTreeNode):Int = {
		var focus_node = node
		while (focus_node.is_notfullyexpanded && focus_node.is_nonterminal) {
			focus_node = expand(focus_node)
		}
		if (focus_node.isExit)
			1
		else
			0
	}
	def backpropagation(node: WTSTreeNode, win: Int): Unit = {
		node.visit += 1
		node.win += win

		if (!node.is_root)
			backpropagation(node.parent,win)
	}






	var root_iterator : Int = 0
	var frontier : List[WTSTreeNode] = List()
	var root_probability : List[Float] = List()

	def mcts_with_frontier(term:TerminationDescription) : Int = {
		tree = new WTSTree(rete,available_actions,specifications)
		WTSTreeNode.id = 0
		solutions=0

		var iterations=0
		val start_timestamp: Long = System.currentTimeMillis

		while (!TerminationDescription.check_termination(term,start_timestamp,iterations,solutions)){
			if (frontier.isEmpty)
				for (c<-tree.root.children) frontier = tree.root :: frontier

			var counter = 0
			for (f<-frontier if f==tree.root) counter += 1
			val prob = counter.toFloat / frontier.size.toFloat
			root_probability = prob :: root_probability

			val focus_node = frontier.head
			frontier = frontier.tail

			if (focus_node.is_root)
				root_iterator += 1

			val deep_node = simulation_until_delta (focus_node, focus_node.r2s)
			backpropagation_with_frontier (deep_node, focus_node.r2s-deep_node.r2s, deep_node.children.size)

			//if (it%20==0)
				//tree.update_wts_file("./data/sps_data/wts_tree.dot")
			frontier = Random.shuffle(frontier)
			iterations += 1
		}

		val end_timestamp: Long = System.currentTimeMillis
		elapsed = end_timestamp-start_timestamp
		iterations
	}
	def simulation_until_delta(node: WTSTreeNode,r2s_ref:Float):WTSTreeNode = {
		var delta:Float = 0

		var focus_node = node
		while (delta==0 && focus_node.is_nonterminal) {
			focus_node = expand_or_child(focus_node)
			delta = r2s_ref-focus_node.r2s
		}
		if (delta>0)
			focus_node.delta=true

		focus_node
	}
	def backpropagation_with_frontier(node: WTSTreeNode, delta: Float, magnitude : Int): Unit = {
		node.visit += 1

		if (delta>0){
			node.win += 1

			if (node.delta) {
				val new_nodes_in_frontier : List[WTSTreeNode] = List.fill(magnitude)(node)
				frontier = new_nodes_in_frontier ::: frontier
			}

		}
		if (!node.is_root)
			backpropagation_with_frontier(node.parent,delta,Math.max(Math.round(magnitude.toFloat/2),1))
	}
	def expand_or_child(node: WTSTreeNode) : WTSTreeNode = {
		val untried : Array[Int] = node.untried_actions
		if (untried.size>0){
			val i : Int = random.nextInt(untried.size)
			val expanded_child = node.get_child(untried(i))
			if (expanded_child.isExit) solutions += 1
			expanded_child
		} else {
			val i : Int = random.nextInt(node.children.size)
			val child = node.get_child(i)
			child
		}
	}






}


