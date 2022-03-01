package org.icar.pmr_solver.best_first_planner

import org.icar.rete.{RETE, RETEBuilder, RETEMemory}
import org.icar.sublevel._
import org.icar.symbolic.{AvailableActions, Domain, GoalModel, Problem}

/******* NOTES AND COMMENTS ********/
// Luca: to implement:
// 2. violation of temporal properties
// 3. improving EVOLUTION CONSTRAINTS: invariants associated to the evolution scenario, and therefore each frontier node has a different set of invariants


/******* SYMBOLIC SOLVER ********/
case class RawFrontierItem(score:Float, rete_memory:RETEMemory, sup:RawGoalSetSupervisor) extends Ordered[RawFrontierItem] {
	override def compare(that: RawFrontierItem) = that.score compare this.score
}

/******* STATE EVOLUTIONS ********/
class Expansion
case class RawExpansion(due_to : RawAction, from : RawState, probtrajectory : Array[RawEvoScenario], invariants: List[RawPredicate])
case class RawEvoScenario(name: String, probability : Float, dest : RawFrontierItem)




object Solver {
	def apply(problem: Problem,domain: Domain,qos : RawState => Float) : Solver = {
		val map = new HL2Raw_Map(domain)

		val I = RawState.factory(map.state_of_world(problem.I.statements),domain.axioms,map)
		val rete = RETEBuilder.factory(domain.axioms,map,I)
		rete.execute

		val specifications: Array[RawGoal] = for (g<-problem.goal_model.goals) yield map.goal_spec(g)
		val init_supervisor = RawGoalSetSupervisor.factory(rete.state,specifications)

		val available_actions = (for (a<-problem.actions.sys_action) yield map.system_action(a)).flatten
		val available_perturb = (for (a<-problem.actions.env_action) yield map.system_action(a)).flatten

		new Solver(rete,init_supervisor,available_actions,available_perturb,qos)
	}

	def mixed_factory(goal_model:GoalModel,actions:AvailableActions,I:RawState,domain: Domain,qos : RawState => Float) : Solver = {
		val map = new HL2Raw_Map(domain)
		val rete = RETEBuilder.factory(domain.axioms,map,I)
		rete.execute

		val specifications: Array[RawGoal] = for (g<-goal_model.goals) yield map.goal_spec(g)
		//val specifications: Array[RawLTL] = for (g<-goal_model.goals) yield map.ltl_formula(g)
		val init_supervisor = RawGoalSetSupervisor.factory(rete.state,specifications)

		val available_actions = (for (a<-actions.sys_action) yield map.system_action(a)).flatten
		val available_perturb = (for (a<-actions.env_action) yield map.system_action(a)).flatten

		new Solver(rete,init_supervisor,available_actions,available_perturb,qos)
	}
}

class Solver(rete:RETE, init_supervisor:RawGoalSetSupervisor, val available_actions:Array[RawAction], val available_perturb:Array[RawAction], qos : RawState => Float) {

	var opt_solution_set : Option[SolutionSet] = None;
	var num_nodes : Int = 0
	var elapsed : Long= 0

	/* solver loop with termination conditions */
	def iterate_until_termination(conf : SolverConfiguration) : Int = {
		num_nodes=0
		val start_timestamp: Long = System.currentTimeMillis

		opt_solution_set = Some( new SolutionSet(rete.memory, qos, init_supervisor, conf.sol_conf) )
		var n_iteration = 0
		var complete = false

		while (!complete && !TerminationDescription.check_termination(conf.termination,start_timestamp,n_iteration,opt_solution_set.get.full_wts.length)) {
			iteration

//			println("iteration"+n_iteration)
//			for (wts <- opt_solution_set.get.wts_list)
//				println( wts.to_graphviz(node => node.toString) )
//			println("end of iteration"+n_iteration)
//			println()

			n_iteration += 1
		}

		val end_timestamp: Long = System.currentTimeMillis
		elapsed = end_timestamp-start_timestamp

//		println("WTS list")
//		for (wts <- opt_solution_set.get.wts_list)
//			println( wts.to_graphviz(node => node.toString) )
//		println("end of WTS list")
//		println()

		n_iteration
	}

	/* Main algorithm of the Solver class */
	def iteration : Unit = {
		if (opt_solution_set.isDefined) {
			val solution_set = opt_solution_set.get
			val somenodeinfrontier: Option[RawFrontierItem] = solution_set.get_next_node

			if (somenodeinfrontier.isDefined) {
				val focus_node_rete_memory: RETEMemory = somenodeinfrontier.get.rete_memory
				val focus_node = focus_node_rete_memory.current
				val focus_node_supervisor: RawGoalSetSupervisor = somenodeinfrontier.get.sup

				val actions : Array[RawAction] = applicable_capabilities(focus_node)
				val envs = applicable_perturbations(focus_node)

				var exp_due_to_system : List[RawExpansion] = List.empty
				for (a <- actions) {
					//rete.memory = focus_node_rete_memory.copy
					exp_due_to_system = generate_system_expansion(focus_node_rete_memory,a,focus_node_supervisor) :: exp_due_to_system
					num_nodes += a.effects.length
				}

				var exp_due_to_environment : List[RawExpansion] = List.empty
				for (e <- envs) {
					//rete.memory = focus_node_rete_memory.copy
					exp_due_to_environment = generate_environment_expansion(focus_node_rete_memory,e,focus_node_supervisor) :: exp_due_to_environment
				}

				if (!exp_due_to_system.isEmpty || !exp_due_to_environment.isEmpty)
					solution_set.apply_expansions(focus_node,exp_due_to_system,exp_due_to_environment)
			}
		}
	}


	private def applicable_capabilities(node : RawState) : Array[RawAction] = {for (a<-available_actions if node.satisfies(a.pre)) yield a}
	private def applicable_perturbations(node : RawState) : Array[RawAction] = {for (a<-available_perturb if node.satisfies(a.pre)) yield a}

	private def generate_system_expansion(start_rete_memory : RETEMemory, action : RawAction, su : RawGoalSetSupervisor) : RawExpansion = {
		require(opt_solution_set.isDefined)

		val source_node : RawState = start_rete_memory.current;
		val trajectory = for (effect <- action.effects) yield calculate_evolution(start_rete_memory,effect,su)
		RawExpansion(action,source_node,trajectory,action.invariants)
	}

	private def generate_environment_expansion(start_rete_memory : RETEMemory, action : RawAction, su : RawGoalSetSupervisor) : RawExpansion = {

		val source_node = start_rete_memory.current

		val trajectory: Array[RawEvoScenario] = for (effect <- action.effects) yield calculate_evolution(start_rete_memory,effect,su)
		RawExpansion(action,source_node,trajectory,action.invariants)
	}

	private def calculate_evolution(start_rete_memory : RETEMemory, evo_description : RawEvolution, supervisor : RawGoalSetSupervisor) : RawEvoScenario = {
		require(opt_solution_set.isDefined)

		rete.memory = start_rete_memory.copy
		rete.extend(evo_description)
		val new_state = rete.state
		val next = supervisor.getNext(new_state)
		val score =qos(new_state)
		val dest = RawFrontierItem(score,rete.memory,next)
		RawEvoScenario(evo_description.name,evo_description.probability,dest)
	}





}







