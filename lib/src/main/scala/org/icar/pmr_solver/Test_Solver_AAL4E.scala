package org.icar.pmr_solver

import org.icar.pmr_solver.best_first_planner.{IterationTermination, SolutionConfiguration, Solver, SolverConfiguration, WTS2Solution, WTSGraph}
import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic.{AbstractCapability, AddOperator, AtomTerm, AvailableActions, Conjunction, Disjunction, Domain, DomainConstant, DomainPredicate, DomainType, DomainVariable, EvoOperator, EvolutionGrounding, ExistQuantifier, Finally, GoalModel, GoalSPEC, GroundPredicate, Implication, LTLGoalSet, Predicate, Problem, RmvOperator, StateOfWorld, StringEnum_DomainType, True, VariableTerm}

object Test_Solver_AAL4E extends App {
  def qos(n: RawState): Float = 0

  val dom_types: Array[DomainType] = Array(
    StringEnum_DomainType("ROOM", Array("bedroom", "kitchen", "living_room")),
    StringEnum_DomainType("ENGAGEMENT", Array("social", "open_mind", "passive", "not_interested")),
    StringEnum_DomainType("ACTIVITY_TYPE", Array("social_activity", "cognitive_exercise", "entertainment", "nothing")),
    StringEnum_DomainType("ENT_CONTENT", Array("selected", "positive_reaction", "neutral_reaction", "negative_reaction")),
    //StringEnum_DomainType("FOUND_OUTCOME", Array("found_in_room", "not_found", "found_otherwise")),
  )

  val preds: Array[DomainPredicate] = Array(
    DomainPredicate("user_location", List(
      DomainVariable("Position", "ROOM"),
    )),
    DomainPredicate("user_localization", List(
      DomainConstant("otherwise"),
    )),
    DomainPredicate("user_engagement", List(
      DomainVariable("Engagement", "ENGAGEMENT"),
    )),
    DomainPredicate("performed", List(
      DomainVariable("Activity", "ACTIVITY_TYPE"),
    )),
    DomainPredicate("entertainment_content", List(
      DomainVariable("Content", "ENT_CONTENT"),
    )),
    DomainPredicate("activity_registered", List(
      DomainConstant("done"),
    )),


  )

  val my_domain = Domain("AAL4E", preds, dom_types, Array.empty)
  val map = new HL2Raw_Map(my_domain)


  /* capability */
  val find_user = AbstractCapability(
    id = "find_user",
    params = List(
      DomainVariable("SearchPosition", "ROOM")
    ),

    pre = True(),

    post = Disjunction(List(
      ExistQuantifier(List(VariableTerm("Position")), Predicate("user_location", List(VariableTerm("Position")))),
      GroundPredicate("user_localization",List(AtomTerm("otherwise")))
    )),

    effects = Array(
      EvolutionGrounding("found", Array[EvoOperator](
        AddOperator(Predicate("user_location", List(VariableTerm("SearchPosition"))))
      )),
      EvolutionGrounding("not_found", Array[EvoOperator](
        AddOperator(Predicate("user_localization", List(AtomTerm("otherwise"))))
      )),
    ),

    future = List.empty
  )

  val propose_engage = AbstractCapability(
    id = "propose_engage",
    params = List.empty,

    pre = GroundPredicate("user_location", List(AtomTerm("living_room"))),

    post = Disjunction(List(
      GroundPredicate("user_engagement", List(AtomTerm("social"))),
      GroundPredicate("user_engagement", List(AtomTerm("open_mind"))),
      GroundPredicate("user_engagement", List(AtomTerm("passive"))),
      GroundPredicate("user_engagement", List(AtomTerm("not_interested"))),
    )),

    effects = Array(
      EvolutionGrounding("social", Array[EvoOperator](
        AddOperator(Predicate("user_engagement", List(AtomTerm("social"))))
      )),
      EvolutionGrounding("open_mind", Array[EvoOperator](
        AddOperator(Predicate("user_engagement", List(AtomTerm("open_mind"))))
      )),
      EvolutionGrounding("passive", Array[EvoOperator](
        AddOperator(Predicate("user_engagement", List(AtomTerm("passive"))))
      )),
      EvolutionGrounding("not_interested", Array[EvoOperator](
        AddOperator(Predicate("user_engagement", List(AtomTerm("not_interested"))))
      )),
    ),

    future = List.empty
  )

  val provide_social_activity = AbstractCapability(
    id = "provide_social_activity",

    params = List.empty,

    pre = Conjunction(List(
      GroundPredicate("user_location", List(AtomTerm("living_room"))),
      GroundPredicate("user_engagement", List(AtomTerm("social"))),
    )),

    post = Predicate("performed", List(AtomTerm("social_activity"))),

    effects = Array(
      EvolutionGrounding("social", Array[EvoOperator](
        AddOperator(Predicate("performed", List(AtomTerm("social_activity"))))
      )),
    ),

    future = List.empty
  )

  val provide_cognitive_exercise = AbstractCapability(
    id = "provide_cognitive_exercise",

    params = List.empty,

    pre = Conjunction(List(
      GroundPredicate("user_location", List(AtomTerm("living_room"))),
      GroundPredicate("user_engagement", List(AtomTerm("open_mind"))),
    )),

    post = GroundPredicate("performed", List(AtomTerm("cognitive_exercise"))),

    effects = Array(
      EvolutionGrounding("open_mind", Array[EvoOperator](
        AddOperator(Predicate("performed", List(AtomTerm("cognitive_exercise"))))
      )),
    ),

    future = List.empty
  )

  val provide_entertainment = AbstractCapability(
    id = "provide_entertainment",

    params = List.empty,

    pre = Conjunction(List(
      GroundPredicate("user_location", List(AtomTerm("living_room"))),
      GroundPredicate("user_engagement", List(AtomTerm("passive"))),
      GroundPredicate("entertainment_content", List(AtomTerm("selected"))),
    )),

    post = GroundPredicate("performed", List(AtomTerm("entertainment"))),

    effects = Array(
      EvolutionGrounding("passive", Array[EvoOperator](
        AddOperator(Predicate("performed", List(AtomTerm("entertainment"))))
      )),
    ),

    future = List.empty
  )

  val select_content = AbstractCapability(
    id = "select_content",

    params = List.empty,

    pre = GroundPredicate("user_engagement", List(AtomTerm("passive"))),

    post = GroundPredicate("entertainment_content", List(AtomTerm("selected"))),

    effects = Array(
      EvolutionGrounding("select", Array[EvoOperator](
        AddOperator(Predicate("entertainment_content", List(AtomTerm("selected"))))
      )),
    ),

    future = List.empty
  )

  val log_activity = AbstractCapability(
    id = "log_activity",

    params = List.empty,

    pre = Disjunction(List(
      GroundPredicate("user_localization", List(AtomTerm("otherwise"))),
      GroundPredicate("user_engagement", List(AtomTerm("not_interested"))),
      GroundPredicate("performed", List(AtomTerm("social_activity"))),
      GroundPredicate("performed", List(AtomTerm("cognitive_exercise"))),
      GroundPredicate("performed", List(AtomTerm("entertainment"))),
    )),

    post = GroundPredicate("activity_registered", List(AtomTerm("done"))),

    effects = Array(
      EvolutionGrounding("select", Array[EvoOperator](
        AddOperator(Predicate("activity_registered", List(AtomTerm("done"))))
      )),
    ),

    future = List.empty
  )

  val sys_action = Array(find_user, propose_engage, provide_social_activity, provide_cognitive_exercise, provide_entertainment, select_content, log_activity)
  val env_action: Array[AbstractCapability] = Array.empty

  /* the problem */
  val initial = StateOfWorld(List())

  val goalset = LTLGoalSet(Array(
    Finally(Conjunction(List(
      GroundPredicate("activity_registered", List(AtomTerm("done"))),

      Disjunction(List(
        GroundPredicate("user_localization", List(AtomTerm("otherwise"))),
        GroundPredicate("user_engagement", List(AtomTerm("not_interested"))),
        GroundPredicate("performed", List(AtomTerm("social_activity"))),
        GroundPredicate("performed", List(AtomTerm("cognitive_exercise"))),
        GroundPredicate("performed", List(AtomTerm("entertainment"))),
      ))
    )))
  ))

  val goalset2 = GoalModel(Array(
    GoalSPEC("1",True(),Finally(Disjunction(List(
        GroundPredicate("user_engagement", List(AtomTerm("not_interested"))),
        GroundPredicate("user_engagement", List(AtomTerm("social"))),
        GroundPredicate("user_engagement", List(AtomTerm("open_mind"))),
        GroundPredicate("user_engagement", List(AtomTerm("passive"))),
      )))),

//    GoalSPEC("2",True(),Finally(Disjunction( List(
//      GroundPredicate("performed", List(AtomTerm("social_activity"))),
//      GroundPredicate("performed", List(AtomTerm("cognitive_exercise"))),
//      GroundPredicate("performed", List(AtomTerm("entertainment"))),
//    )))),

    GoalSPEC("2.1",True(),
      Implication(GroundPredicate("user_engagement", List(AtomTerm("social"))),Finally(GroundPredicate("performed", List(AtomTerm("social_activity")))))
    ),
    GoalSPEC("2.2",True(),
      Implication(GroundPredicate("user_engagement", List(AtomTerm("open_mind"))),Finally(GroundPredicate("performed", List(AtomTerm("cognitive_exercise")))))
    ),
    GoalSPEC("2.3",True(),
      Implication(GroundPredicate("user_engagement", List(AtomTerm("passive"))),Finally(GroundPredicate("performed", List(AtomTerm("entertainment"))))),
    ),

    GoalSPEC("3.1",True(),Implication(GroundPredicate("user_localization", List(AtomTerm("otherwise"))),Finally(GroundPredicate("activity_registered", List(AtomTerm("done")))))),
    GoalSPEC("3.2",True(),Implication(GroundPredicate("user_engagement", List(AtomTerm("not_interested"))),Finally(GroundPredicate("activity_registered", List(AtomTerm("done")))))),
    GoalSPEC("3.3",True(),Implication(GroundPredicate("performed", List(AtomTerm("social_activity"))),Finally(GroundPredicate("activity_registered", List(AtomTerm("done")))))),
    GoalSPEC("3.4",True(),Implication(GroundPredicate("performed", List(AtomTerm("cognitive_exercise"))),Finally(GroundPredicate("activity_registered", List(AtomTerm("done")))))),
    GoalSPEC("3.5",True(),Implication(GroundPredicate("performed", List(AtomTerm("entertainment"))),Finally(GroundPredicate("activity_registered", List(AtomTerm("done")))))),

   ))

  val available = AvailableActions(sys_action, env_action)
  val my_problem = Problem(initial, goalset2, available)

  /* the solver */
  val solver = Solver(my_problem, my_domain, qos)

  println("**Domain**")
  println("Number of predicates: " + map.inverse.size)
  println("Number of goals: " + goalset.goals.length)
  println("Number of actions: " + solver.available_actions.length)
  println("Number of perturbations: " + solver.available_perturb.length)

  val its = solver.iterate_until_termination(SolverConfiguration(
    IterationTermination(50),
    SolutionConfiguration(
      allow_self_loop = false,
      allow_cap_multiple_instance = true,
      allow_loop = true,
      allow_parallel_action = true)))

  if (solver.opt_solution_set.isDefined) {
    println("**Planning**")
    println("Number of iterations: " + its)
    println()

    println("**Solutions**")
    println("Number of generated WTS: " + solver.opt_solution_set.get.wts_list.size)
    println("Number of full WTS: " + solver.opt_solution_set.get.full_wts.size)
    println("Number of partial WTS: " + solver.opt_solution_set.get.partial_wts.size)
    println()
    //println( solver.opt_solution_set.get.all_solutions_to_graphviz(node => node.toString) )

    if (!solver.opt_solution_set.get.wts_list.isEmpty) {
      for (wts <- solver.opt_solution_set.get.full_wts) {
        println(wts.to_graphviz(node => node.toString))
        val converter = new WTS2Solution(wts, initial)
        println(converter.to_graphviz())
      }
      if (solver.opt_solution_set.get.full_wts.isEmpty)
        for (wts <- solver.opt_solution_set.get.wts_list) {
          println(wts.to_graphviz(node => node.toString))
          val converter = new WTS2Solution(wts, initial)
          println(converter.to_graphviz())
        }
    }
  }
}


