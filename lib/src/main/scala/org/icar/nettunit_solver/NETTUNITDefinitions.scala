package org.icar.nettunit_solver

import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic._

object NETTUNITDefinitions {
  def qos(n: RawState): Float = 0

  val dom_types: Array[DomainType] = Array(
    StringEnum_DomainType("TARGET_LOCATION", Array("refinery", "alt_location1", "alt_location2")),
    StringEnum_DomainType("ALARM_STATE", Array("attention", "pre_alert", "alert")),
    StringEnum_DomainType("AUTHORITY_ROLE", Array("prefect", "mayor", "questor", "municipality")),
    StringEnum_DomainType("COMPETENT_BODY_ROLE", Array("commander_fire_brigade", "mayor", "questor", "n118", "ASP", "ARPA", "civil_protection")),
    StringEnum_DomainType("EVENT_TYPE", Array("fire", "explosion", "smoke_diffusion")),
    StringEnum_DomainType("INTERNAL_PLAN_STATE", Array("active", "over")),
  )

  val preds: Array[DomainPredicate] = Array(
    DomainPredicate("emergency_location", List(
      DomainVariable("location", "TARGET_LOCATION"),
    )),
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
    DomainPredicate("fire_brigade_assessment_done", List(
      DomainVariable("state", "ALARM_STATE"),
    )),
    DomainPredicate("internal_plan_active", List(
      DomainVariable("state", "INTERNAL_PLAN_STATE"),
    )),
    DomainPredicate("fire_extinguished", List()),
    DomainPredicate("coordinated_firefighter_intervention", List()),
    DomainPredicate("second_explosion", List()),
    DomainPredicate("evaluated_fire_radiant_energy", List()),
    DomainPredicate("crossborder_inform", List()),
    DomainPredicate("assessed_emergency", List()),
    DomainPredicate("tech_report", List(
      DomainVariable("event", "EVENT_TYPE"),
      DomainVariable("state", "ALARM_STATE"),
    )),
    DomainPredicate("notified_authorities", List(
      DomainVariable("state", "ALARM_STATE"),
    )),

  )

  val my_domain = Domain("NETTUNIT", preds, dom_types, Array.empty)
  val map = new HL2Raw_Map(my_domain)

  /* capability */
  val send_team_to_evaluate = AbstractCapability(
    /*
    GOAL send_team_to_evaluate : WHEN emergency_location(refinery) THEN THE safety_manager ROLE SHALL ADDRESS alarm_state(attention)
    */
    id = "send_team_to_evaluate",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("emergency_location", List(AtomTerm("refinery"))),
      Negation(GroundPredicate("assessed_emergency", List()))
    )),
    post = GroundPredicate("alarm_state", List(AtomTerm("attention"))),


    effects = Array(
      EvolutionGrounding("attention_state", Array[EvoOperator](
        AddOperator(Predicate("alarm_state", List(AtomTerm("attention")))),
      )),
    ),
    future = List.empty
  )

  val activate_internal_security_plan = AbstractCapability(
    /*
    GOAL activate_internal_security_plan : WHEN alarm_state(attention) OR alarm_state(pre_alert) THEN THE plant_operator ROLE SHALL ADDRESS internal_plan_active(true)
    */
    id = "activate_internal_security_plan",

    params = List(),
    pre = GroundPredicate("alarm_state", List(AtomTerm("attention"))),
    post = GroundPredicate("internal_plan_active", List(AtomTerm("active"))),
    effects = Array(
      EvolutionGrounding("activate_internal_plan", Array[EvoOperator](
        AddOperator(Predicate("internal_plan_active", List(AtomTerm("active"))))
      )),
    ),
    future = List.empty
  )

  val notify_competent_body_internal_plan = AbstractCapability(
    /*
    GOAL notify_competent_body_internal_plan : WHEN internal_plan_active(true) AND alarm_state(attention)
    THEN THE nettunit ROLE SHALL ADDRESS informed_authority(prefect, attention) AND informed_authority(mayor, attention) AND notified_authorities(attention)
     */
    id = "notify_competent_body_internal_plan",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("internal_plan_active", List(AtomTerm("active"))),
      GroundPredicate("alarm_state", List(AtomTerm("attention")))
    )),
    post = Conjunction(List(
      GroundPredicate("informed_authority", List(AtomTerm("prefect"), AtomTerm("attention"))),
      GroundPredicate("informed_authority", List(AtomTerm("mayor"), AtomTerm("attention"))),
    )),
    effects = Array(
      EvolutionGrounding("mayor", Array[EvoOperator](
        AddOperator(Predicate("informed_authority", List(AtomTerm("mayor"), AtomTerm("attention")))),
        AddOperator(Predicate("informed_authority", List(AtomTerm("prefect"), AtomTerm("attention"))))
      )),
    ),

    future = List.empty)

  val inform_technical_rescue_organisation_internal_plan = AbstractCapability(
    /*
    GOAL inform_technical_rescue_organisation_internal_plan : WHEN notified_authorities(attention) AND internal_plan_active(true)
    AND alarm_state(attention) THEN THE nettunit ROLE SHALL ADDRESS informed(commander_fire_brigade, attention)
     */
    id = "inform_technical_rescue_organisation_internal_plan",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("internal_plan_active", List(AtomTerm("active"))),
      GroundPredicate("alarm_state", List(AtomTerm("attention"))),
    )),
    post = GroundPredicate("informed", List(AtomTerm("commander_fire_brigade"), AtomTerm("attention"))),
    effects = Array(
      EvolutionGrounding("fire_brigade", Array[EvoOperator](
        AddOperator(Predicate("informed", List(AtomTerm("commander_fire_brigade"), AtomTerm("attention"))))
      ))
    ),
    future = List.empty
  )
  val decide_response_type = AbstractCapability(
    /*
    GOAL decide_response_type : WHEN internal_plan_active(done) AND alarm_state(attention) AND
    informed(commander_fire_brigade, attention) THEN THE nettunit ROLE SHALL ADDRESS fire_brigade_assessment_done(attention)
     */
    id = "decide_response_type",

    params = List(),
    pre = GroundPredicate("informed", List(AtomTerm("commander_fire_brigade"), AtomTerm("attention"))),
    post = GroundPredicate("fire_brigade_assessment_done", List(AtomTerm("attention"))),
    effects = Array(
      EvolutionGrounding("assess", Array[EvoOperator](
        AddOperator(Predicate("fire_brigade_assessment_done", List(AtomTerm("attention"))))
      )),
    ),
    future = List.empty
  )
  val prepare_tech_report = AbstractCapability(
    /*
    GOAL prepare_tech_report : WHEN NOT fire_extinguished AND fire_brigade_assessment_done(attention) THEN THE commander_fire_brigade ROLE SHALL ADDRESS tech_report(event,attention)
     */
    id = "prepare_tech_report",

    params = List(),
    pre = GroundPredicate("fire_brigade_assessment_done", List(AtomTerm("attention"))),
    post = GroundPredicate("tech_report", List(AtomTerm("fire"), AtomTerm("attention"))),
    effects = Array(EvolutionGrounding("report", Array[EvoOperator](
      AddOperator(Predicate("tech_report", List(AtomTerm("fire"), AtomTerm("attention"))))
    ))),
    future = List.empty
  )
  val keep_update_involved_personnel = AbstractCapability(
    /*
    GOAL coordinate_firefighter_intervention : WHEN fire_brigade_assessment_done(attention) AND NOT fire_extinguished THEN THE nettunit ROLE SHALL ADDRESS coordinated_firefighter_intervention
     */
    id = "keep_update_involved_personnel",

    params = List(),
    pre = GroundPredicate("tech_report", List(AtomTerm("fire"), AtomTerm("attention"))),
    post = Conjunction(List(
      GroundPredicate("coordinated_firefighter_intervention", List()),
      GroundPredicate("second_explosion", List())
    )),
    effects = Array(
      EvolutionGrounding("report", Array[EvoOperator](
        AddOperator(Predicate("coordinated_firefighter_intervention", List())),
        AddOperator(Predicate("second_explosion", List()))
      )),
    ),

    future = List.empty
  )
  val declare_pre_alert_state = AbstractCapability(
    /*
    GOAL declare_pre_alert_state : WHEN alarm_state(attention) AND internal_plan_active(done) AND second_explosion THEN THE nettunit ROLE SHALL ADDRESS alarm_state(pre_alert)
     */
    id = "declare_pre_alert_state",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("coordinated_firefighter_intervention", List()),
      GroundPredicate("second_explosion", List())
    )),
    post = GroundPredicate("alarm_state", List(AtomTerm("pre_alert"))),
    effects = Array(
      EvolutionGrounding("pre_alert", Array[EvoOperator](
        AddOperator(Predicate("alarm_state", List(AtomTerm("pre_alert")))),
      )),
    ),

    future = List.empty
  )
  val inform_technical_rescue_organisation_alert = AbstractCapability(
    /*GOAL inform_technical_rescue_organisation_alert : WHEN alarm_state(pre_alert)
    THEN THE nettunit ROLE SHALL ADDRESS informed(n118, pre_alert) AND informed(ASP, pre_alert) AND informed(ARPA, pre_alert)
     */
    id = "inform_technical_rescue_organisation_alert",
    params = List(),

    pre = GroundPredicate("alarm_state", List(AtomTerm("pre_alert"))),
    post = Conjunction(List(
      GroundPredicate("informed", List(AtomTerm("n118"), AtomTerm("pre_alert"))),
      GroundPredicate("informed", List(AtomTerm("ASP"), AtomTerm("pre_alert"))),
      GroundPredicate("informed", List(AtomTerm("ARPA"), AtomTerm("pre_alert")))
    )),
    effects = Array(
      EvolutionGrounding("n118", Array[EvoOperator](
        AddOperator(Predicate("informed", List(AtomTerm("n118"), AtomTerm("pre_alert")))),
        AddOperator(Predicate("informed", List(AtomTerm("ASP"), AtomTerm("pre_alert")))),
        AddOperator(Predicate("informed", List(AtomTerm("ARPA"), AtomTerm("pre_alert"))))
      ))
    ),
    future = List.empty
  )
  val evaluate_fire_radiant_energy = AbstractCapability(
    /*
    GOAL evaluate_fire_radiant_energy : WHEN informed(ARPA, pre_alert) AND alarm_state(pre_alert) THEN THE ARPA ROLE SHALL ADDRESS evaluated_fire_radiant_energy
     */
    id = "evaluate_fire_radiant_energy",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("informed", List(AtomTerm("ARPA"), AtomTerm("pre_alert"))),
      GroundPredicate("alarm_state", List(AtomTerm("pre_alert")))
    )),
    post = GroundPredicate("evaluated_fire_radiant_energy", List()),
    effects = Array(
      EvolutionGrounding("evaluate", Array[EvoOperator](
        AddOperator(Predicate("evaluated_fire_radiant_energy", List()))
      ))
    ),

    future = List.empty
  )
  val declare_alarm_state = AbstractCapability(
    /*GOAL declare_alarm_state : WHEN evaluated_fire_radiant_energy AND alarm_state(pre_alert) AND NOT fire_extinguished
    THEN THE commander_fire_brigade ROLE SHALL ADDRESS alarm_state(alert)
     */
    id = "declare_alarm_state",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("evaluated_fire_radiant_energy", List()),
      GroundPredicate("alarm_state", List(AtomTerm("pre_alert"))),
      Negation(GroundPredicate("fire_extinguished", List())), //TODO verifica
    )),
    post = GroundPredicate("alarm_state", List(AtomTerm("alert"))),
    effects = Array(
      EvolutionGrounding("pre_alert", Array[EvoOperator](
        AddOperator(Predicate("alarm_state", List(AtomTerm("alert")))),
      )),
    ),


    future = List.empty
  )
  val ensure_presence_of_qualified_personnel = AbstractCapability(
    /*
    GOAL ensure_presence_of_qualified_personnel : WHEN alarm_state(alert)
    THEN THE nettunit ROLE SHALL ADDRESS informed(civil_protection,alert) AND informed_authority(municipality,alert)
     */
    id = "ensure_presence_of_qualified_personnel",

    params = List(),
    pre = GroundPredicate("alarm_state", List(AtomTerm("alert"))),
    post = Conjunction(List(
      GroundPredicate("informed", List(AtomTerm("civil_protection"), AtomTerm("alert"))),
      GroundPredicate("informed_authority", List(AtomTerm("municipality"), AtomTerm("alert"))),
    )),
    effects = Array(
      EvolutionGrounding("municipality", Array[EvoOperator](
        AddOperator(Predicate("informed_authority", List(AtomTerm("municipality"), AtomTerm("alert")))),
        AddOperator(Predicate("informed", List(AtomTerm("civil_protection"), AtomTerm("alert"))))
      )),
    ),

    future = List.empty
  )
  val ensure_presence_of_representative = AbstractCapability(
    /**
     * GOAL ensure_presence_of_representative : WHEN alarm_state(alert)
     * THEN THE nettunit ROLE SHALL ADDRESS informed_authority(questor,alert)
     */
    id = "ensure_presence_of_representative",

    params = List(),
    pre = GroundPredicate("alarm_state", List(AtomTerm("alert"))),
    post = Conjunction(List(
      GroundPredicate("informed_authority", List(AtomTerm("questor"), AtomTerm("alert"))),
    )),
    effects = Array(
      EvolutionGrounding("questor", Array[EvoOperator](
        AddOperator(Predicate("informed_authority", List(AtomTerm("questor"), AtomTerm("alert"))))
      )),
    ),

    future = List.empty
  )
  val do_crossborder_communication = AbstractCapability(
    /*
    GOAL do_crossborder_communication : WHEN evaluated_fire_radiant_energy AND alarm_state(alert)
    THEN THE nettunit ROLE SHALL ADDRESS crossborder_inform
     */
    id = "do_crossborder_communication",

    params = List(),
    pre = GroundPredicate("alarm_state", List(AtomTerm("alert"))),
    post = GroundPredicate("crossborder_inform", List()),
    effects = Array(
      EvolutionGrounding("inform_tunisian_bodies", Array[EvoOperator](
        AddOperator(Predicate("crossborder_inform", List()))
      )),
    ),

    future = List.empty
  )

  val sys_action = Array(
    send_team_to_evaluate,
    activate_internal_security_plan,
    notify_competent_body_internal_plan,
    inform_technical_rescue_organisation_internal_plan,
    decide_response_type,
    prepare_tech_report,
    keep_update_involved_personnel,
    declare_pre_alert_state,
    inform_technical_rescue_organisation_alert,
    evaluate_fire_radiant_energy,
    declare_alarm_state,
    ensure_presence_of_qualified_personnel,
    ensure_presence_of_representative,
    do_crossborder_communication
  )
  val env_action: Array[AbstractCapability] = Array.empty

  /* the problem */
  val initial = StateOfWorld(List(GroundPredicate("emergency_location", List(AtomTerm("refinery")))))

  val availableActions = AvailableActions(sys_action, env_action)
}
