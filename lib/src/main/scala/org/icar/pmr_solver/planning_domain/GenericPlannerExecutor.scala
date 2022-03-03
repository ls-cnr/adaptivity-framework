package org.icar.pmr_solver.planning_domain

import org.icar.pmr_solver.{IterationTermination, SolutionConfiguration, SolverConfiguration}
import org.icar.pmr_solver.Test_Solver_AAL4E.{map, my_domain, my_problem, qos}
import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution}
import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic.{Domain, Problem}

object GenericPlannerExecutor {

  def run_solver(my_problem: Problem, my_domain: Domain, qos: RawState => Float, map: HL2Raw_Map, conf: SolverConfiguration) : Unit  = {
    /* the solver */
    val solver = Solver(my_problem, my_domain, qos,SolverConfiguration(
      IterationTermination(50),
      SolutionConfiguration(
        allow_self_loop = false,
        allow_cap_multiple_instance = true,
        allow_loop = true,
        allow_parallel_action = true)))

    println("**Domain**")
    println("Number of predicates: " + map.inverse.size)
    println("Number of goals: " + my_problem.goal_model.goals.length)
    println("Number of actions: " + solver.available_actions.length)
    println("Number of perturbations: " + solver.available_perturb.length)

    val its = solver.iterate_until_termination()

    println("**Planning**")
    println("Number of iterations: " + its)
    println()

    println("**Solutions**")
    println("Number of generated WTS: " + solver.solution_set.wts_list.size)
    println("Number of full WTS: " + solver.solution_set.full_wts.size)
    println("Number of partial WTS: " + solver.solution_set.partial_wts.size)
    println()
    //println( solver.opt_solution_set.get.all_solutions_to_graphviz(node => node.toString) )

    if (!solver.solution_set.wts_list.isEmpty) {
      for (wts <- solver.solution_set.full_wts) {
        println(wts.to_graphviz(node => node.toString))
        val converter = new WTS2Solution(wts, my_problem.I)
        println(converter.to_graphviz())
      }
      if (solver.solution_set.full_wts.isEmpty)
        for (wts <- solver.solution_set.wts_list) {
          println(wts.wts_labelling.goal_sat_list)
          println(wts.to_decorated_graphviz(node => node.toString))
          val converter = new WTS2Solution(wts, my_problem.I)
          println(converter.to_graphviz())
        }
    }

  }

}
