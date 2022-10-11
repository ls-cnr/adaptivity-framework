package org.icar.pmr_solver.planning_domain

import org.icar.sublevel.{HL2Raw_Map, RawPredicate, RawState}
import org.icar.symbolic.{AbstractCapability, AddOperator, AtomTerm, AvailableActions, Disjunction, Domain, DomainPredicate, DomainType, DomainVariable, EvoOperator, EvolutionGrounding, ExistQuantifier, Finally, GoalModel, GoalSPEC, GroundPredicate, HL_LTLFormula, Predicate, Problem, RmvOperator, StateOfWorld, StringEnum_DomainType, True, VariableTerm}

class RandomlyGeneratedDomain(SIZE_ARGS: Int, SIZE_PREDS: Int) {
  def qos(n: RawState): Float = 0

  var cap_id = 0
  var goal_id = 0

  //val SIZE = 3
  val range_args: Array[Int] = (0 to SIZE_ARGS).toArray
  val range_preds: Array[Int] = (0 to SIZE_PREDS).toArray

  val args: Array[String] = for (a_index <- range_args) yield "v" + a_index.toString
  val types: Array[DomainType] = for (p_index <- range_preds) yield StringEnum_DomainType("T" + p_index.toString, args)
  val preds = for (p_index <- range_preds) yield DomainPredicate("P" + p_index.toString, List(DomainVariable("Arg", types(p_index).name)))

  val my_domain = Domain("RandomSpace", preds, types, Array.empty)
  val map = new HL2Raw_Map(my_domain)

  val main_rand = scala.util.Random
  var list_of_caps: List[AbstractCapability] = List.empty
  for (p_index <- range_preds)
    for (a_index <- range_args)
      if (main_rand.nextInt(100) > 20) {
        for (ind <- 1 to 2) {
          list_of_caps = forward_random_capability(p_index, a_index, ind) :: list_of_caps
          //list_of_caps = backward_random_capability(p_index,a_index,ind) :: list_of_caps
        }
      } else {
        list_of_caps = forward_nondet_capability(p_index, a_index) :: list_of_caps
      }



  //for (index <- 1 to 5) list_of_caps = nondet_random_capability() :: list_of_caps

  val sys_action = list_of_caps.reverse.toArray
  val env_action: Array[AbstractCapability] = Array.empty

  val initial = StateOfWorld(List(GroundPredicate(preds(0).functor, List(AtomTerm(args(0))))))

  var list_of_goals: List[GoalSPEC] = List.empty
  for (index <- 1 to 2) list_of_goals = generate_random_goal() :: list_of_goals
  val goalset = GoalModel(list_of_goals.toArray)

  val available = AvailableActions(sys_action, env_action)
  val my_problem = Problem(initial, goalset, available)

  val domaingraph = domain_to_graphviz(map, my_problem.goal_model, my_problem.actions.sys_action)
  //println(domaingraph)

  def generate_random_goal(): GoalSPEC = {
    val rand = scala.util.Random

    goal_id += 1

    val half_args = (SIZE_ARGS / 2).intValue() + 1
    val half_preds = (SIZE_PREDS / 2).intValue() + 1

    val p_index_pre = rand.nextInt(half_preds)
    val a_index_pre = rand.nextInt(half_args)
    val p_index_post = Math.min(rand.nextInt(half_preds) + half_preds, SIZE_PREDS - 1)
    val a_index_post = Math.min(rand.nextInt(half_args) + half_args, SIZE_ARGS - 1)

    GoalSPEC("G" + goal_id.toString,
      pre = GroundPredicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre)))),
      post = Finally(GroundPredicate(preds(p_index_post).functor, List(AtomTerm(args(a_index_post)))))
    )

  }

  def nondet_random_capability(): AbstractCapability = {
    cap_id += 1

    val rand = scala.util.Random

    val half_args = (SIZE_ARGS / 2).intValue() + 1
    val half_preds = (SIZE_PREDS / 2).intValue() + 1

    val p_index_pre = rand.nextInt(half_preds)
    val a_index_pre = rand.nextInt(half_args)
    val p_index_post1 = Math.min(rand.nextInt(half_preds) + half_preds, SIZE_PREDS)
    val a_index_post1 = Math.min(rand.nextInt(half_args) + half_args, SIZE_ARGS)
    val p_index_post2 = rand.nextInt(SIZE_PREDS)
    val a_index_post2 = rand.nextInt(SIZE_ARGS)
    // NON DETERMINISTIC
    AbstractCapability(
      id = "NON" + cap_id,

      params = List(),
      pre = GroundPredicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre)))),
      post = Disjunction(List(
        GroundPredicate(preds(p_index_post1).functor, List(AtomTerm(args(a_index_post1)))),
        GroundPredicate(preds(p_index_post2).functor, List(AtomTerm(args(a_index_post2)))),
      )),
      effects = Array(
        EvolutionGrounding("uno", Array[EvoOperator](
          RmvOperator(Predicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre))))),
          AddOperator(Predicate(preds(p_index_post1).functor, List(AtomTerm(args(a_index_post1)))))
        )),
        EvolutionGrounding("due", Array[EvoOperator](
          RmvOperator(Predicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre))))),
          AddOperator(Predicate(preds(p_index_post2).functor, List(AtomTerm(args(a_index_post2)))))
        )),
      ),
      future = List.empty
    )
  }

  def forward_nondet_capability(p_index_pre: Int, a_index_pre: Int): AbstractCapability = {
    cap_id += 1

    val rand = scala.util.Random

    val half_args = (SIZE_ARGS / 2).intValue() + 1
    val half_preds = (SIZE_PREDS / 2).intValue() + 1

    val p_index_post1 = Math.min(rand.nextInt(half_preds) + half_preds, SIZE_PREDS)
    val a_index_post1 = Math.min(rand.nextInt(half_args) + half_args, SIZE_ARGS)
    val p_index_post2 = rand.nextInt(SIZE_PREDS)
    val a_index_post2 = rand.nextInt(SIZE_ARGS)
    // NON DETERMINISTIC
    AbstractCapability(
      id = "NON" + cap_id,

      params = List(),
      pre = GroundPredicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre)))),
      post = Disjunction(List(
        GroundPredicate(preds(p_index_post1).functor, List(AtomTerm(args(a_index_post1)))),
        GroundPredicate(preds(p_index_post2).functor, List(AtomTerm(args(a_index_post2)))),
      )),
      effects = Array(
        EvolutionGrounding("uno", Array[EvoOperator](
          RmvOperator(Predicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre))))),
          AddOperator(Predicate(preds(p_index_post1).functor, List(AtomTerm(args(a_index_post1)))))
        )),
        EvolutionGrounding("due", Array[EvoOperator](
          RmvOperator(Predicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre))))),
          AddOperator(Predicate(preds(p_index_post2).functor, List(AtomTerm(args(a_index_post2)))))
        )),
      ),
      future = List.empty
    )
  }

  def forward_random_capability(p_index_pre: Int, a_index_pre: Int, index: Int): AbstractCapability = {
    cap_id += 1

    var p_index_post = p_index_pre
    var a_index_post = a_index_pre

    if (index == 1) {
      p_index_post += 1
      if (p_index_post > SIZE_PREDS) p_index_post = 0
    } else if (index == 2) {
      a_index_post += 1
      if (a_index_post > SIZE_ARGS) a_index_post = 0
    }

    // DETERMINISTIC
    AbstractCapability(
      id = "CAP" + cap_id,

      params = List(),
      pre = GroundPredicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre)))),
      post = GroundPredicate(preds(p_index_post).functor, List(AtomTerm(args(a_index_post)))),
      effects = Array(
        EvolutionGrounding("uno", Array[EvoOperator](
          RmvOperator(Predicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre))))),
          AddOperator(Predicate(preds(p_index_post).functor, List(AtomTerm(args(a_index_post)))))
        )),
      ),
      future = List.empty
    )

  }

  def backward_random_capability(p_index_post: Int, a_index_post: Int, index: Int): AbstractCapability = {
    cap_id += 1

    var p_index_pre = p_index_post
    var a_index_pre = a_index_post

    if (index == 1) {
      p_index_pre -= 1
      if (p_index_pre < 0) p_index_pre = SIZE_PREDS - 1
    } else {
      a_index_pre -= 1
      if (a_index_pre < 0) a_index_pre = SIZE_ARGS - 1
    }

    AbstractCapability(
      id = "CAP" + (cap_id).toString,

      params = List(),
      pre = GroundPredicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre)))),
      post = GroundPredicate(preds(p_index_post).functor, List(AtomTerm(args(a_index_post)))),
      effects = Array(
        EvolutionGrounding("uno", Array[EvoOperator](
          RmvOperator(Predicate(preds(p_index_pre).functor, List(AtomTerm(args(a_index_pre))))),
          AddOperator(Predicate(preds(p_index_post).functor, List(AtomTerm(args(a_index_post)))))
        )),
      ),
      future = List.empty
    )

  }

  def domain_to_graphviz(map: HL2Raw_Map, goal_model: GoalModel, sys_action: Array[AbstractCapability]): String = {
    var string = "digraph DOMAIN {\n"

    for (p_index <- range_preds) {
      string += "subgraph  {rank=same; "
      for (v_index <- range_args) {
        val p = GroundPredicate(preds(p_index).functor, List(AtomTerm(args(v_index))))
        string += "\"N" + map.direct(p) + "\";"
      }
      string += "};\n"
    }



    //for (node <- map.direct)
    //string += "\"N"+node._2+"\";\n"

    for (act <- sys_action) {
      val raw_pre = map.predicate_formula(act.pre)
      val raw_post = map.predicate_formula(act.post)

      string += arrow(raw_pre, raw_post, act.id, "black")
    }

    for (g <- goal_model.goals) {
      val raw_pre = map.predicate_formula(g.pre)
      val g_post: HL_LTLFormula = g.post
      val _finally: Finally = g.post.asInstanceOf[Finally]
      val _ground_pred: GroundPredicate = _finally.formula.asInstanceOf[GroundPredicate]
      val raw_post = map.predicate_formula(_ground_pred)

      string += arrow(raw_pre, raw_post, g.id, "red")
    }

    string + "}\n"
  }

  def arrow(raw_pre: RawPredicate, raw_post: RawPredicate, label: String, color: String): String = {
    var string = ""

    for (startnode <- map.inverse) {
      val startstate = RawState(map.state_of_world(List(startnode)))
      if (startstate.satisfies(raw_pre)) {

        for (destnode <- map.inverse) {
          val deststate = RawState(map.state_of_world(List(destnode)))
          if (deststate.satisfies(raw_post)) {
            string += "\"N" + map.direct(startnode) + "\""
            string += "->"
            string += "\"N" + map.direct(destnode) + "\""
            string += "[label=\"" + label + "\",color=" + color + "];\n"
          }
        }
      }
    }

    string
  }

}
