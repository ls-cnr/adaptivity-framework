package org.DEMO

import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic._

object NETTUNITDefinitionsDEMO {
  def qos(n: RawState): Float = 0

  val dom_types: Array[DomainType] = Array(
    StringEnum_DomainType("TARGET_LOCATION", Array("volcano", "refinery", "alt_location1", "alt_location2")),
    StringEnum_DomainType("EMG_COMPETENT_BODY_ROLE", Array("pcrs", "irib", "inm", "pcct", "comune")),

    StringEnum_DomainType("EMG_RESPONSE_TYPE_RESPONSIBLE", Array("pcct", "comune")),
    /*StringEnum_DomainType("ALARM_STATE", Array("attention", "pre_alert", "alert")),
    StringEnum_DomainType("AUTHORITY_ROLE", Array("prefect", "mayor", "questor", "municipality")),
    StringEnum_DomainType("COMPETENT_BODY_ROLE", Array("commander_fire_brigade", "mayor", "questor", "n118", "ASP", "ARPA", "civil_protection")),
    StringEnum_DomainType("EVENT_TYPE", Array("fire", "explosion", "smoke_diffusion")),
    StringEnum_DomainType("INTERNAL_PLAN_STATE", Array("active", "over")),*/
  )

  val preds: Array[DomainPredicate] = Array(
    DomainPredicate("emergency_location", List(
      DomainVariable("location", "TARGET_LOCATION"),
    )),
    DomainPredicate("identified_incident", List(
      DomainVariable("location", "TARGET_LOCATION"),
    )),
    DomainPredicate("involved_pertinent_bodies", List(
      DomainVariable("role", "EMG_COMPETENT_BODY_ROLE"),
    )),
    DomainPredicate("scenario_evaluated", List()),
    DomainPredicate("obtained_airborne_estimate", List()),
    DomainPredicate("updated_airborne_dispersion_data", List()),
    DomainPredicate("obtained_health_risk_estimate", List()),
    DomainPredicate("updated_health_risk_data", List()),
    DomainPredicate("involved_local_authorities", List()),
    DomainPredicate("assessed_emergency", List()),


    DomainPredicate("involved_competent_roles", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE"),
    )),

    DomainPredicate("decided_response_type", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE"),
    )),

    DomainPredicate("informed_citizens", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE"),
    )),

    DomainPredicate("inform_via_app", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE"),
    )),

    DomainPredicate("assessed_event_severity", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE"),
    )),


  )

  val my_domain = Domain("NETTUNIT", preds, dom_types, Array.empty)
  val map = new HL2Raw_Map(my_domain)


  /*GOAL managing_emergency IS AND OF {
        GOAL identifying_incident: WHEN emergency_location(volcano) AND NOT assessed_emergency THEN THE ingv ROLE SHALL ADDRESS FINALLY identified_incident
        GOAL involve_pertinent_roles_pcrs: WHEN identified_incident THEN THE pcrs ROLE SHALL ADDRESS involved_pertinent_bodies_pcrs
        GOAL evaluate_incident_scenario: WHEN involved_pertinent_bodies_pcrs THEN THE pcrs ROLE SHALL ADDRESS FINALLY scenario_evaluated
        GOAL ask_for_airborne_dispersion_estimate: WHEN scenario_evaluated THEN THE pcrs ROLE SHALL ADDRESS FINALLY obtained_airborne_estimate
        GOAL involve_pertinent_roles_inm: WHEN obtained_airborne_estimate THEN THE inm ROLE SHALL ADDRESS FINALLY involved_pertinent_bodies_inm
        GOAL update_airborne_dispersion_data: WHEN involved_pertinent_bodies_inm THEN THE inm ROLE SHALL ADDRESS FINALLY updated_airborne_dispersion_data
        GOAL ask_for_health_risk_estimate: WHEN updated_airborne_dispersion_data THEN THE pcrs ROLE SHALL ADDRESS FINALLY obtained_health_risk_estimate
        GOAL involve_pertinent_roles_irib: WHEN obtained_health_risk_estimate THEN THE irib ROLE SHALL ADDRESS FINALLY involved_pertinent_bodies_irib
        GOAL update_health_risk_data: WHEN involved_pertinent_bodies_irib THEN THE irib ROLE SHALL ADDRESS FINALLY updated_health_risk_data
        GOAL inform_involved_local_authorities: WHEN updated_health_risk_data THEN THE pcrs ROLE SHALL ADDRESS FINALLY involved_local_authorities
    }
  */

  /* capability */
  val identifying_incident = AbstractCapability(

    /*
    GOAL identifying_incident: WHEN emergency_location(volcano) AND NOT assessed_emergency THEN THE ingv ROLE SHALL ADDRESS FINALLY identified_incident
    */
    id = "identifying_incident",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("emergency_location", List(AtomTerm("volcano"))),
      Negation(GroundPredicate("assessed_emergency", List()))
    )),
    post = GroundPredicate("identified_incident", List(AtomTerm("volcano"))),

    effects = Array(
      EvolutionGrounding("identified", Array[EvoOperator](
        AddOperator(Predicate("identified_incident", List(AtomTerm("volcano")))),
      )),
    ),
    future = List.empty
  )

  val involve_pertinent_roles_pcrs = AbstractCapability(
    /*
    GOAL involve_pertinent_roles_pcrs: WHEN identified_incident(volcano) THEN THE pcrs ROLE SHALL ADDRESS involved_pertinent_bodies(pcrs)
    */
    id = "involve_pertinent_roles_pcrs",

    params = List(),

    pre = GroundPredicate("identified_incident", List(AtomTerm("volcano"))),
    post = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("pcrs"))),

    effects = Array(
      EvolutionGrounding("involve_bodies", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies", List(AtomTerm("pcrs")))),
      )),
    ),
    future = List.empty
  )

  val evaluate_incident_scenario = AbstractCapability(
    /*
    GOAL evaluate_incident_scenario: WHEN involved_pertinent_bodies_pcrs THEN THE pcrs ROLE SHALL ADDRESS FINALLY scenario_evaluated
    */
    id = "evaluate_incident_scenario",

    params = List(),
    pre = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("pcrs"))),
    post = GroundPredicate("scenario_evaluated", List()),

    effects = Array(
      EvolutionGrounding("evaluated", Array[EvoOperator](
        AddOperator(Predicate("scenario_evaluated", List())),
      )),
    ),
    future = List.empty
  )

  val ask_for_airborne_dispersion_estimate = AbstractCapability(
    /*
    GOAL ask_for_airborne_dispersion_estimate: WHEN scenario_evaluated THEN THE pcrs ROLE SHALL ADDRESS FINALLY obtained_airborne_estimate
    */
    id = "ask_for_airborne_dispersion_estimate",

    params = List(),
    pre = GroundPredicate("scenario_evaluated", List()),
    post = GroundPredicate("obtained_airborne_estimate", List()),

    effects = Array(
      EvolutionGrounding("get_estimate", Array[EvoOperator](
        AddOperator(Predicate("obtained_airborne_estimate", List())),
      )),
    ),
    future = List.empty
  )

  val involve_pertinent_roles_inm = AbstractCapability(
    /*GOAL involve_pertinent_roles_inm: WHEN obtained_airborne_estimate THEN THE inm ROLE SHALL ADDRESS FINALLY involved_pertinent_bodies_inm*/
    id = "involve_pertinent_roles_inm",

    params = List(),
    pre = GroundPredicate("obtained_airborne_estimate", List()),
    post = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("inm"))),

    effects = Array(
      EvolutionGrounding("involve_bodies", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies", List(AtomTerm("inm")))),
      )),
    ),
    future = List.empty
  )

  val update_airborne_dispersion_data = AbstractCapability(
    /*
    GOAL update_airborne_dispersion_data: WHEN involved_pertinent_bodies_inm THEN THE inm ROLE SHALL ADDRESS FINALLY updated_airborne_dispersion_data
    */
    id = "update_airborne_dispersion_data",

    params = List(),
    pre = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("inm"))),
    post = GroundPredicate("updated_airborne_dispersion_data", List()),

    effects = Array(
      EvolutionGrounding("update", Array[EvoOperator](
        AddOperator(Predicate("updated_airborne_dispersion_data", List())),
      )),
    ),
    future = List.empty
  )

  val ask_for_health_risk_estimate = AbstractCapability(
    /*
    GOAL ask_for_health_risk_estimate: WHEN updated_airborne_dispersion_data THEN THE pcrs ROLE SHALL ADDRESS FINALLY obtained_health_risk_estimate
    */
    id = "ask_for_health_risk_estimate",

    params = List(),
    pre = GroundPredicate("updated_airborne_dispersion_data", List()),
    post = GroundPredicate("obtained_health_risk_estimate", List()),

    effects = Array(
      EvolutionGrounding("get_estimate_health", Array[EvoOperator](
        AddOperator(Predicate("obtained_health_risk_estimate", List())),
      )),
    ),
    future = List.empty
  )

  val involve_pertinent_roles_irib = AbstractCapability(
    /*
    GOAL involve_pertinent_roles_irib: WHEN obtained_health_risk_estimate THEN THE irib ROLE SHALL ADDRESS FINALLY involved_pertinent_bodies_irib
    */
    id = "involve_pertinent_roles_irib",

    params = List(),
    pre = GroundPredicate("obtained_health_risk_estimate", List()),
    post = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("irib"))),

    effects = Array(
      EvolutionGrounding("involve_irib", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies", List(AtomTerm("irib")))),
      )),
    ),
    future = List.empty
  )

  val update_health_risk_data = AbstractCapability(
    /*
    GOAL update_health_risk_data: WHEN involved_pertinent_bodies_irib THEN THE irib ROLE SHALL ADDRESS FINALLY updated_health_risk_data
    */
    id = "update_health_risk_data",

    params = List(),
    pre = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("irib"))),
    post = GroundPredicate("updated_health_risk_data", List()),

    effects = Array(
      EvolutionGrounding("update_risk_data", Array[EvoOperator](
        AddOperator(Predicate("updated_health_risk_data", List())),
      )),
    ),
    future = List.empty
  )

  val inform_involved_local_authorities = AbstractCapability(
    /*
    GOAL inform_involved_local_authorities: WHEN updated_health_risk_data THEN THE pcrs ROLE SHALL ADDRESS FINALLY involved_local_authorities
    */
    id = "inform_involved_local_authorities",

    params = List(),
    pre = GroundPredicate("updated_health_risk_data", List()),
    post = GroundPredicate("involved_local_authorities", List()),

    effects = Array(
      EvolutionGrounding("involve_auths", Array[EvoOperator](
        AddOperator(Predicate("involved_local_authorities", List())),
      )),
    ),
    future = List.empty
  )

  /*----------------------------------------------------------------------------*/
  /*                         SUBPROCESS COMUNI                                  */
  /*----------------------------------------------------------------------------*/

  val comune_involve_competent_roles = AbstractCapability(
    /*
    GOAL comune_involve_competent_roles: WHEN involved_local_authorities THEN THE comune ROLE SHALL ADDRESS FINALLY involved_competent_roles(comune)
    */
    id = "comune_involve_competent_roles",

    params = List(),
    pre = GroundPredicate("involved_local_authorities", List()),
    post = GroundPredicate("involved_competent_roles", List(AtomTerm("comune"))),

    effects = Array(
      EvolutionGrounding("involve_comune", Array[EvoOperator](
        AddOperator(Predicate("involved_competent_roles", List(AtomTerm("comune")))),
      )),
    ),
    future = List.empty
  )

  val comune_decide_response_type = AbstractCapability(
    /*
    GOAL comune_decide_response_type: WHEN involved_competent_roles(comune) THEN THE comune ROLE SHALL ADDRESS FINALLY decided_response_type(comune)
    */
    id = "comune_decide_response_type",

    params = List(),
    pre = GroundPredicate("involved_competent_roles", List(AtomTerm("comune"))),
    post = GroundPredicate("decided_response_type", List(AtomTerm("comune"))),

    effects = Array(
      EvolutionGrounding("decide_response", Array[EvoOperator](
        AddOperator(Predicate("decided_response_type", List(AtomTerm("comune")))),
      )),
    ),
    future = List.empty
  )

  val comune_inform_citizen = AbstractCapability(
    /*
    GOAL comune_inform_citizen: WHEN decided_response_type(comune) THEN THE comune ROLE SHALL ADDRESS FINALLY informed_citizens(comune)
    */
    id = "comune_inform_citizen",

    params = List(),
    pre = GroundPredicate("decided_response_type", List(AtomTerm("comune"))),
    post = GroundPredicate("informed_citizens", List(AtomTerm("comune"))),

    effects = Array(
      EvolutionGrounding("inform_citizen", Array[EvoOperator](
        AddOperator(Predicate("informed_citizens", List(AtomTerm("comune")))),
      )),
    ),
    future = List.empty
  )

  val comune_monitor_event_severity = AbstractCapability(
    /*
    GOAL comune_monitor_event_severity: WHEN informed_citizens(comune) THEN THE comune ROLE SHALL ADDRESS FINALLY assessed_event_severity_comune
    */
    id = "comune_monitor_event_severity",

    params = List(),
    pre = GroundPredicate("informed_citizens", List(AtomTerm("comune"))),
    post = GroundPredicate("assessed_event_severity", List(AtomTerm("comune"))),

    effects = Array(
      EvolutionGrounding("assess_event", Array[EvoOperator](
        AddOperator(Predicate("assessed_event_severity", List(AtomTerm("comune")))),
      )),
    ),
    future = List.empty
  )

  val comune_inform_citizen_via_app = AbstractCapability(
    /*
    GOAL comune_inform_citizen_via_app: WHEN assessed_event_severity_comune THEN THE comune ROLE SHALL ADDRESS FINALLY  inform_via_app(comune)
    */
    id = "comune_inform_citizen_via_app",

    params = List(),
    pre = GroundPredicate("assessed_event_severity", List(AtomTerm("comune"))),
    post = GroundPredicate("inform_via_app", List(AtomTerm("comune"))),

    effects = Array(
      EvolutionGrounding("inform", Array[EvoOperator](
        AddOperator(Predicate("inform_via_app", List(AtomTerm("comune")))),
      )),
    ),
    future = List.empty
  )

  /*----------------------------------------------------------------------------*/
  /*                         SUBPROCESS PCCT                                    */
  /*----------------------------------------------------------------------------*/
  val pcct_involve_competent_roles = AbstractCapability(
    /*
    GOAL pcct_involve_competent_roles: WHEN involved_local_authorities THEN THE pcct ROLE SHALL ADDRESS FINALLY involved_competent_roles(pcct)
    */
    id = "pcct_involve_competent_roles",

    params = List(),
    pre = GroundPredicate("involved_local_authorities", List()),
    post = GroundPredicate("involved_competent_roles", List(AtomTerm("pcct"))),

    effects = Array(
      EvolutionGrounding("involve_pcct", Array[EvoOperator](
        AddOperator(Predicate("involved_competent_roles", List(AtomTerm("pcct")))),
      )),
    ),
    future = List.empty
  )

  val pcct_decide_response_type = AbstractCapability(
    /*
    GOAL pcct_decide_response_type: WHEN involved_competent_roles(pcct) THEN THE pcct ROLE SHALL ADDRESS FINALLY decided_response_type(pcct)
    */
    id = "pcct_decide_response_type",

    params = List(),
    pre = GroundPredicate("involved_competent_roles", List(AtomTerm("pcct"))),
    post = GroundPredicate("decided_response_type", List(AtomTerm("pcct"))),

    effects = Array(
      EvolutionGrounding("decide_response", Array[EvoOperator](
        AddOperator(Predicate("decided_response_type", List(AtomTerm("pcct")))),
      )),
    ),
    future = List.empty
  )

  val pcct_inform_citizen = AbstractCapability(
    /*
    GOAL pcct_inform_citizen: WHEN decided_response_type(pcct) THEN THE pcct ROLE SHALL ADDRESS FINALLY informed_citizens(pcct)
    */
    id = "pcct_inform_citizen",

    params = List(),
    pre = GroundPredicate("decided_response_type", List(AtomTerm("pcct"))),
    post = GroundPredicate("informed_citizens", List(AtomTerm("pcct"))),

    effects = Array(
      EvolutionGrounding("inform_citizen", Array[EvoOperator](
        AddOperator(Predicate("informed_citizens", List(AtomTerm("pcct")))),
      )),
    ),
    future = List.empty
  )

  val pcct_monitor_event_severity = AbstractCapability(
    /*
    GOAL pcct_monitor_event_severity: WHEN informed_citizens(pcct) THEN THE pcct ROLE SHALL ADDRESS FINALLY assessed_event_severity_pcct
    */
    id = "pcct_monitor_event_severity",

    params = List(),
    pre = GroundPredicate("informed_citizens", List(AtomTerm("pcct"))),
    post = GroundPredicate("assessed_event_severity", List(AtomTerm("pcct"))),

    effects = Array(
      EvolutionGrounding("assess_event", Array[EvoOperator](
        AddOperator(Predicate("assessed_event_severity", List(AtomTerm("pcct")))),
      )),
    ),
    future = List.empty
  )

  val pcct_inform_citizen_via_app = AbstractCapability(
    /*
    GOAL pcct_inform_citizen_via_app: WHEN assessed_event_severity_pcct THEN THE pcct ROLE SHALL ADDRESS FINALLY  inform_via_app(pcct)
    */
    id = "pcct_inform_citizen_via_app",

    params = List(),
    pre = GroundPredicate("assessed_event_severity", List(AtomTerm("pcct"))),
    post = GroundPredicate("inform_via_app", List(AtomTerm("pcct"))),

    effects = Array(
      EvolutionGrounding("inform", Array[EvoOperator](
        AddOperator(Predicate("inform_via_app", List(AtomTerm("pcct")))),
      )),
    ),
    future = List.empty
  )

  val sys_action = Array(
    identifying_incident,
    involve_pertinent_roles_pcrs,
    evaluate_incident_scenario,
    ask_for_airborne_dispersion_estimate,
    involve_pertinent_roles_inm,
    update_airborne_dispersion_data,
    ask_for_health_risk_estimate,
    involve_pertinent_roles_irib,
    update_health_risk_data,
    inform_involved_local_authorities,

    comune_involve_competent_roles,
    comune_decide_response_type,
    comune_inform_citizen,
    comune_monitor_event_severity,
    comune_inform_citizen_via_app,

    pcct_involve_competent_roles,
    pcct_decide_response_type,
    pcct_inform_citizen,
    pcct_monitor_event_severity,
    pcct_inform_citizen_via_app,

  )

  val env_action: Array[AbstractCapability] = Array.empty

  /* the problem */
  val initial = StateOfWorld(List(GroundPredicate("emergency_location", List(AtomTerm("refinery")))))

  val initial_comunepcct = StateOfWorld(List(GroundPredicate("involved_local_authorities", List())))

  val availableActions = AvailableActions(sys_action, env_action)
}
