package org.icar.pmr_solver.planning_domain

import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic.{AbstractCapability, AddOperator, AtomTerm, AvailableActions, Disjunction, Domain, DomainPredicate, DomainType, DomainVariable, EvoOperator, EvolutionGrounding, ExistQuantifier, Finally, GoalModel, GoalSPEC, GroundPredicate, Predicate, Problem, RmvOperator, StateOfWorld, StringEnum_DomainType, True, VariableTerm}

object IDS_like_domain {
  /* the domain */
  //val axioms: Array[Assumption] =Array(Assumption("ready(X) :- available(X), registered(X)."))
  def qos(n:RawState):Float=0

  val dom_types : Array[DomainType] = Array(
    StringEnum_DomainType("doc_type",Array("issue_list","request","tech_rep")),
    StringEnum_DomainType("doc_state",Array("received","registered","worked","accepted","rejected","to_revise"))
  )

  val preds : Array[DomainPredicate] = Array(
    DomainPredicate("document",List(
      DomainVariable("TYPE","doc_type"),
      DomainVariable("STATE","doc_state")
    ))
  )

  val my_domain = Domain("IDS",preds,dom_types,Array.empty)

  /* capability */
  val register = AbstractCapability(
    id = "register",
    isHuman = false,
    params = List(DomainVariable("TYPE","doc_type")),

    pre = ExistQuantifier(
      List(VariableTerm("TYPE")),
      Predicate("document", List(VariableTerm("TYPE"), AtomTerm("received")))
    ),

    post = ExistQuantifier(
      List(VariableTerm("TYPE")),
      Predicate("document", List(VariableTerm("TYPE"), AtomTerm("registered")))
    ),

    effects = Array(
      EvolutionGrounding("base",Array[EvoOperator](
        AddOperator(Predicate("document", List( VariableTerm("TYPE"), AtomTerm("registered")))),
        RmvOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("received"))))
      ))),

    future = List.empty
  )

  val work = AbstractCapability(
    id = "work",
    isHuman = false,
    params = List(DomainVariable("TYPE","doc_type")),

    pre = Disjunction ( List(
      ExistQuantifier(
        List(VariableTerm("TYPE")),
        Predicate("document", List(VariableTerm("TYPE"), AtomTerm("registered")))
      ),
      ExistQuantifier(
        List(VariableTerm("TYPE")),
        Predicate("document", List(VariableTerm("TYPE"), AtomTerm("to_revise")))
      ))
    ),

    post = ExistQuantifier(
      List(VariableTerm("TYPE")),
      Predicate("document", List(VariableTerm("TYPE"), AtomTerm("worked")))
    ),

    effects = Array(
      EvolutionGrounding("base",Array[EvoOperator](
        AddOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("worked")))),
        RmvOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("to_revise")))),
        RmvOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("registered"))))
      ))),

    future = List.empty
  )

  val supervise = AbstractCapability(
    id = "supervise",
    isHuman = false,
    params = List(DomainVariable("TYPE","doc_type")),

    pre = ExistQuantifier(
      List(VariableTerm("TYPE")),
      Predicate("document", List(VariableTerm("TYPE"), AtomTerm("worked")))
    ),

    post = Disjunction ( List(
      ExistQuantifier(
        List(VariableTerm("TYPE")),
        Predicate("document", List(VariableTerm("TYPE"), AtomTerm("accepted")))
      ),
      ExistQuantifier(
        List(VariableTerm("TYPE")),
        Predicate("document", List(VariableTerm("TYPE"), AtomTerm("rejected")))
      ),
      ExistQuantifier(
        List(VariableTerm("TYPE")),
        Predicate("document", List(VariableTerm("TYPE"), AtomTerm("to_revise")))
      )
    )),

    effects = Array(
      EvolutionGrounding("ok",Array[EvoOperator](
        AddOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("accepted")))),
        RmvOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("worked"))))
      )),
      EvolutionGrounding("no",Array[EvoOperator](
        AddOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("rejected")))),
        RmvOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("worked"))))
      )),
      EvolutionGrounding("change",Array[EvoOperator](
        AddOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("to_revise")))),
        RmvOperator(Predicate("document", List(VariableTerm("TYPE"), AtomTerm("worked"))))
      ))
    ),

    future = List.empty
  )

  val sys_action = Array(register,work,supervise)//,work,request_again,supervise) //


  //
  //	/* perturbations */
  //	val evo_lose = Array(
  //		ProbabilisticEvolutionGrounding("base",0.01f,Array[EvoOperator](RemoveEvoOperator(GroundPredicate("available", AtomTerm("doc"))), AddEvoOperator(GroundPredicate("unavailable", AtomTerm("doc")))))
  //	)
  //	val pre_lose = TweetyFormula.fromFormula(Literal(org.icar.fol.Predicate("available", AtomTerm("doc"))))
  //	val lose_doc = EnvironmentAction("lose",pre_lose,evo_lose)
  //
  val env_action : Array[AbstractCapability] = Array.empty// Array(lose_doc) //

  val map = new HL2Raw_Map(my_domain)

  /* the problem */
  val initial = StateOfWorld(List(
    GroundPredicate("document", List(AtomTerm("tech_rep"),AtomTerm("received")))
  ))
  val accepted = GroundPredicate("document", List(AtomTerm("tech_rep"),AtomTerm("accepted")))
  val rejected = GroundPredicate("document", List(AtomTerm("tech_rep"),AtomTerm("rejected")))

  val goalmodel = GoalModel(Array(

    GoalSPEC("1",True(),Finally(Disjunction(List(accepted,rejected))))

  ))
  val available = AvailableActions(sys_action,env_action)
  val my_problem = Problem(initial,goalmodel,available)

}
