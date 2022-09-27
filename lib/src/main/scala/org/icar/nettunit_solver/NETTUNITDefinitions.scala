package org.icar.nettunit_solver

import org.icar.sublevel.HL2Raw_Map
import org.icar.symbolic.{AbstractCapability, AddOperator, AtomTerm, Disjunction, Domain, DomainConstant, DomainPredicate, DomainType, DomainVariable, EvoOperator, EvolutionGrounding, ExistQuantifier, GroundPredicate, Predicate, StringEnum_DomainType, True, VariableTerm}

class NETTUNITDefinitions {
  val dom_types: Array[DomainType] = Array(

    StringEnum_DomainType("ALARM_STATE", Array("attention", "pre_alert", "alert")),
    StringEnum_DomainType("AUTHORITY_ROLE", Array("prefect", "mayor", "questor", "municipality")),
    StringEnum_DomainType("COMPETENT_BODY_ROLE", Array("commander_fire_brigade", "mayor", "questor", "118", "ASP", "ARPA", "civil_protection")),
    StringEnum_DomainType("EVENT_TYPE", Array("fire", "explosion", "smoke_diffusion")),

  )
  val preds: Array[DomainPredicate] = Array(
    DomainPredicate("alarm_state", List(
      DomainVariable("state", "ALARM_STATE"),
    )),

    DomainPredicate("informed_authority", List(
      DomainVariable("role", "AUTHORITY_ROLE"),
      DomainVariable("state", "ALARM_STATE"),
    )),

    DomainPredicate("informed", List(
      DomainVariable("role", "COMPETENT_BODY_ROLE"),
      DomainVariable("state", "ALARM_STATE"),
    )),

    DomainPredicate("internal_plan_active", List(
      DomainConstant("done"),
    )),
  )

  val my_domain = Domain("NETTUNIT", preds, dom_types, Array.empty)
  val map = new HL2Raw_Map(my_domain)

  /* capability */
  val send_team_to_evaluate = AbstractCapability(
    id = "send_team_to_evaluate",
    params = List(
      DomainVariable("SearchPosition", "ROOM")
    ),

    pre = True(),

    post = Disjunction(List(
      ExistQuantifier(List(VariableTerm("Position")), Predicate("user_location", List(VariableTerm("Position")))),
      GroundPredicate("user_localization", List(AtomTerm("otherwise")))
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

  val activate_internal_security_plan = AbstractCapability(
    id = "activate_internal_security_plan",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )

  val notify_competent_body_internal_plan = AbstractCapability(
    id = "notify_competent_body_internal_plan",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty)

  val inform_technical_rescue_organisation_internal_plan = AbstractCapability(
    id = "inform_technical_rescue_organisation_internal_plan",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val fire_brigade_assessment = AbstractCapability(
    id = "fire_brigade_assessment",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val prepare_tech_report = AbstractCapability(
    id = "prepare_tech_report",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val coordinate_firefighter_intervention = AbstractCapability(
    id = "coordinate_firefighter_intervention",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val declare_pre_alert_state = AbstractCapability(
    id = "declare_pre_alert_state",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val inform_technical_rescue_organisation_alert = AbstractCapability(
    id = "inform_technical_rescue_organisation_alert",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val evaluate_fire_radiant_energy = AbstractCapability(
    id = "evaluate_fire_radiant_energy",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val declare_alarm_state = AbstractCapability(
    id = "declare_alarm_state",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val ensure_presence_of_qualified_personnel = AbstractCapability(
    id = "ensure_presence_of_qualified_personnel",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val ensure_presence_of_representative = AbstractCapability(
    id = "ensure_presence_of_representative",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )
  val do_crossborder_communication = AbstractCapability(
    id = "do_crossborder_communication",
    params = List(),

    pre = True(),

    post = True(),

    effects = Array(),

    future = List.empty
  )

}
