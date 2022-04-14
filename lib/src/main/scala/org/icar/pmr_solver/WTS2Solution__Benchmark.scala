package org.icar.pmr_solver

import org.icar.pmr_solver.best_first_planner.{Solver, WTS2Solution, WTSGraph}
import org.icar.pmr_solver.planning_domain.{GenericPlannerExecutor, RandomlyGeneratedDomain}
import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic.{Domain, Problem}
import org.scalameter.api.{Bench, Gen}

object WTS2Solution__Benchmark
  extends Bench.LocalTime {

    var my_problem: Problem = null

    val size = Gen.range("size")(0, 10, 1)
    val ranges = for {
      i <- size
    } yield generate_wts_by_size(i)

    performance of "size" in {
      measure method "WTS2Solution" in {
        using(ranges) in {
          r => new WTS2Solution(r, my_problem.I)
        }
      }
    }

    def generate_wts_by_size(SIZE: Int): WTSGraph = {
      var wts_list: List[WTSGraph] = List.empty

      while (wts_list.isEmpty) {
        val rand_domain = new RandomlyGeneratedDomain(3 + SIZE, 3)
        val my_domain = rand_domain.my_domain
        my_problem = rand_domain.my_problem
        val map = rand_domain.map
        val qos: RawState => Float = rand_domain.qos
        val conf = SolverConfiguration(
          IterationTermination(100),
          SolutionConfiguration(
            allow_self_loop = false,
            allow_cap_multiple_instance = true,
            allow_loop = true,
            allow_parallel_action = true))

        wts_list = generate_wts_list(my_problem, my_domain, qos, map, conf)
      }

      val wts_array = wts_list.toArray
      var max: Int = 0
      var size: Int = 0
      for (ind <- 0 to wts_array.size - 1) {
        val wts = wts_array(ind)
        if (wts.transitions.size > size) {
          max = ind
          size = wts.transitions.size
        }
      }

      wts_array(max)
    }

  def generate_wts_list(my_problem: Problem, my_domain: Domain, qos: RawState => Float, map: HL2Raw_Map, conf: SolverConfiguration): List[WTSGraph] = {
    var list = List.empty[WTSGraph]
    /* the solver */
    val solver = Solver(my_problem, my_domain, qos, conf)
    val its = solver.iterate_until_termination()
    if (!solver.solution_set.wts_list.isEmpty) {
      for (wts: WTSGraph <- solver.solution_set.full_wts) {
        list = wts :: list
      }
    }

    list
  }

}