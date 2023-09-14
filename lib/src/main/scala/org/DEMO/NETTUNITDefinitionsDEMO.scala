package org.DEMO

import org.icar.sublevel.{HL2Raw_Map, RawState}
import org.icar.symbolic._

object NETTUNITDefinitionsDEMO {
  def qos(n: RawState): Float = 0

  val dom_types: Array[DomainType] = Array(
    StringEnum_DomainType("TARGET_LOCATION", Array("volcano", "alt_location1", "alt_location2")),
    StringEnum_DomainType("EMG_COMPETENT_BODY_ROLE", Array("pcrs", "irib", "inm", "pcct", "ct_mayor", "comune", "pctn", "hama", "regions_tn", "ariana")),
    StringEnum_DomainType("EMG_COMPETENT_BODY_ROLE_TN", Array("pctn", "hama", "regions_tn", "ariana", "inm")),

    StringEnum_DomainType("EMG_RESPONSE_TYPE_RESPONSIBLE", Array("pcct", "comune")),
    StringEnum_DomainType("EMG_RESPONSE_TYPE_RESPONSIBLE_TN", Array("ariana", "regions_tn", "inm")),
    /*StringEnum_DomainType("ALARM_STATE", Array("attention", "pre_alert", "alert")),
    StringEnum_DomainType("AUTHORITY_ROLE", Array("prefect", "mayor", "questor", "municipality")),
    StringEnum_DomainType("COMPETENT_BODY_ROLE", Array("commander_fire_brigade", "mayor", "questor", "n118", "ASP", "ARPA", "civil_protection")),
    StringEnum_DomainType("EVENT_TYPE", Array("fire", "explosion", "smoke_diffusion")),
    StringEnum_DomainType("INTERNAL_PLAN_STATE", Array("active", "over")),*/
  )

  /*GOAL process_ariana IS AND OF {
      GOAL ariana_involve_competent_roles: WHEN involved_local_authorities_tn THEN THE ariana ROLE SHALL ADDRESS FINALLY involved_competent_roles_tn(ariana)
      GOAL ariana_decide_response_type: WHEN involved_competent_roles_tn(ariana) THEN THE ariana ROLE SHALL ADDRESS FINALLY decided_response_type_tn(ariana)
      GOAL ariana_inform_citizen: WHEN decided_response_type_tn(ariana) THEN THE ariana ROLE SHALL ADDRESS FINALLY informed_citizens_tn(ariana)
      GOAL ariana_monitor_event_severity: WHEN informed_citizens_tn(ariana) THEN THE ariana ROLE SHALL ADDRESS FINALLY assessed_event_severity_tn(ariana)
      GOAL ariana_inform_citizen_via_app: WHEN assessed_event_severity_tn(ariana) THEN THE ariana ROLE SHALL ADDRESS FINALLY  inform_via_app_tn(ariana)
  }*/
  val preds: Array[DomainPredicate] = Array(
    /** ******************************************** */
    /*TN*/
    /** ******************************************** */
    DomainPredicate("emergency_location_tn", List(
      DomainVariable("location", "TARGET_LOCATION"),
    )),
    DomainPredicate("identified_incident_tn", List(
      DomainVariable("location", "TARGET_LOCATION"),
    )),
    DomainPredicate("involved_pertinent_bodies_tn", List(
      DomainVariable("role", "EMG_COMPETENT_BODY_ROLE_TN"),
    )),
    DomainPredicate("scenario_evaluated_tn", List()),
    DomainPredicate("obtained_airborne_estimate_tn", List()),
    DomainPredicate("updated_airborne_dispersion_data_tn", List()),
    DomainPredicate("obtained_health_risk_estimate_tn", List()),
    DomainPredicate("updated_health_risk_data_tn", List()),
    DomainPredicate("involved_local_authorities_tn", List()),
    DomainPredicate("assessed_emergency_tn", List()),

    //subprocesses
    DomainPredicate("involved_competent_roles_tn", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE_TN"),
    )),

    DomainPredicate("decided_response_type_tn", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE_TN"),
    )),

    DomainPredicate("informed_citizens_tn", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE_TN"),
    )),

    DomainPredicate("inform_via_app_tn", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE_TN"),
    )),

    DomainPredicate("assessed_event_severity_tn", List(
      DomainVariable("role", "EMG_RESPONSE_TYPE_RESPONSIBLE_TN"),
    )),

    /** ******************************************** */
    /*ITALY*/
    /** ******************************************** */
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
    DomainPredicate("involved_crossborder_authorities", List()),
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

  /*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          tunisia
    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*/
  //main process
  /*
  GOAL identifying_incident: WHEN emergency_location(volcano) AND NOT assessed_emergency THEN THE ingv ROLE SHALL ADDRESS FINALLY identified_incident
  */
  /* capability */
  val identifying_incident_tn = AbstractCapability(
    id = "identifying_incident_tn",

    params = List(),
    pre = Conjunction(List(
      GroundPredicate("emergency_location_tn", List(AtomTerm("volcano"))),
      Negation(GroundPredicate("assessed_emergency_tn", List()))
    )),
    post = GroundPredicate("identified_incident_tn", List(AtomTerm("volcano"))),

    effects = Array(
      EvolutionGrounding("identified_tn", Array[EvoOperator](
        AddOperator(Predicate("identified_incident_tn", List(AtomTerm("volcano")))),
      )),
    ),
    future = List.empty
  )

  val involve_pertinent_roles_pctn = AbstractCapability(
    /*
    GOAL involve_pertinent_roles_pcrs: WHEN identified_incident(volcano) THEN THE pcrs ROLE SHALL ADDRESS involved_pertinent_bodies(pcrs)
    */
    id = "involve_pertinent_roles_pctn_tn",

    params = List(),

    pre = GroundPredicate("identified_incident_tn", List(AtomTerm("volcano"))),
    post = GroundPredicate("involved_pertinent_bodies_tn", List(AtomTerm("pctn"))),

    effects = Array(
      EvolutionGrounding("involve_bodies_tn", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies_tn", List(AtomTerm("pctn")))),
      )),
    ),
    future = List.empty
  )

  val evaluate_incident_scenario_tn = AbstractCapability(
    /*
    GOAL evaluate_incident_scenario: WHEN involved_pertinent_bodies_pcrs THEN THE pcrs ROLE SHALL ADDRESS FINALLY scenario_evaluated
    */
    id = "evaluate_incident_scenario_tn",

    params = List(),
    pre = GroundPredicate("involved_pertinent_bodies_tn", List(AtomTerm("pctn"))),
    post = GroundPredicate("scenario_evaluated_tn", List()),

    effects = Array(
      EvolutionGrounding("evaluated_tn", Array[EvoOperator](
        AddOperator(Predicate("scenario_evaluated_tn", List())),
      )),
    ),
    future = List.empty
  )

  val ask_for_airborne_dispersion_estimate_tn = AbstractCapability(
    /*
    GOAL ask_for_airborne_dispersion_estimate: WHEN scenario_evaluated THEN THE pcrs ROLE SHALL ADDRESS FINALLY obtained_airborne_estimate
    */
    id = "ask_for_airborne_dispersion_estimate_tn",

    params = List(),
    pre = GroundPredicate("scenario_evaluated_tn", List()),
    post = GroundPredicate("obtained_airborne_estimate_tn", List()),

    effects = Array(
      EvolutionGrounding("get_estimate_tn", Array[EvoOperator](
        AddOperator(Predicate("obtained_airborne_estimate_tn", List())),
      )),
    ),
    future = List.empty
  )

  val involve_pertinent_roles_inm_tn = AbstractCapability(
    /*GOAL involve_pertinent_roles_inm: WHEN obtained_airborne_estimate THEN THE inm ROLE SHALL ADDRESS FINALLY involved_pertinent_bodies_inm*/
    id = "involve_pertinent_roles_inm",

    params = List(),
    pre = GroundPredicate("obtained_airborne_estimate_tn", List()),
    post = GroundPredicate("involved_pertinent_bodies_tn", List(AtomTerm("inm"))),

    effects = Array(
      EvolutionGrounding("involve_bodies_tn", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies_tn", List(AtomTerm("inm")))),
      )),
    ),
    future = List.empty
  )

  val update_airborne_dispersion_data_tn = AbstractCapability(
    /*
    GOAL update_airborne_dispersion_data: WHEN involved_pertinent_bodies_inm THEN THE inm ROLE SHALL ADDRESS FINALLY updated_airborne_dispersion_data
    */
    id = "update_airborne_dispersion_data_tn",

    params = List(),
    pre = GroundPredicate("involved_pertinent_bodies_tn", List(AtomTerm("inm"))),
    post = GroundPredicate("updated_airborne_dispersion_data_tn", List()),

    effects = Array(
      EvolutionGrounding("update", Array[EvoOperator](
        AddOperator(Predicate("updated_airborne_dispersion_data_tn", List())),
      )),
    ),
    future = List.empty
  )

  val ask_for_health_risk_estimate_tn = AbstractCapability(
    /*
    GOAL ask_for_health_risk_estimate: WHEN updated_airborne_dispersion_data THEN THE pcrs ROLE SHALL ADDRESS FINALLY obtained_health_risk_estimate
    */
    id = "ask_for_health_risk_estimate_tn",

    params = List(),
    pre = GroundPredicate("updated_airborne_dispersion_data_tn", List()),
    post = GroundPredicate("obtained_health_risk_estimate_tn", List()),

    effects = Array(
      EvolutionGrounding("get_estimate_health_tn", Array[EvoOperator](
        AddOperator(Predicate("obtained_health_risk_estimate_tn", List())),
      )),
    ),
    future = List.empty
  )

  val involve_pertinent_roles_hama = AbstractCapability(
    /*
    GOAL involve_pertinent_roles_irib: WHEN obtained_health_risk_estimate THEN THE irib ROLE SHALL ADDRESS FINALLY involved_pertinent_bodies_irib
    */
    id = "involve_pertinent_roles_hama",

    params = List(),
    pre = GroundPredicate("obtained_health_risk_estimate_tn", List()),
    post = GroundPredicate("involved_pertinent_bodies_tn", List(AtomTerm("hama"))),

    effects = Array(
      EvolutionGrounding("involve_hama", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies_tn", List(AtomTerm("hama")))),
      )),
    ),
    future = List.empty
  )

  val update_health_risk_data_tn = AbstractCapability(
    /*
    GOAL update_health_risk_data: WHEN involved_pertinent_bodies_irib THEN THE irib ROLE SHALL ADDRESS FINALLY updated_health_risk_data
    */
    id = "update_health_risk_data_tn",

    params = List(),
    pre = GroundPredicate("involved_pertinent_bodies_tn", List(AtomTerm("hama"))),
    post = GroundPredicate("updated_health_risk_data_tn", List()),

    effects = Array(
      EvolutionGrounding("update_risk_data_tn", Array[EvoOperator](
        AddOperator(Predicate("updated_health_risk_data_tn", List())),
      )),
    ),
    future = List.empty
  )

  val inform_involved_local_authorities_tn = AbstractCapability(
    /*
    GOAL inform_involved_local_authorities: WHEN updated_health_risk_data THEN THE pcrs ROLE SHALL ADDRESS FINALLY involved_local_authorities
    */
    id = "inform_involved_local_authorities_tn",

    params = List(),
    pre = GroundPredicate("updated_health_risk_data_tn", List()),
    post = GroundPredicate("involved_local_authorities_tn", List()),

    effects = Array(
      EvolutionGrounding("involve_auths_tn", Array[EvoOperator](
        AddOperator(Predicate("involved_local_authorities_tn", List())),
      )),
    ),
    future = List.empty
  )


  /*----------------------------------------------------------------------------*/
  /*                         SUBPROCESS regions tn                                  */
  /*----------------------------------------------------------------------------*/

  val regions_tn_involve_competent_roles = AbstractCapability(
    /*
    GOAL comune_involve_competent_roles: WHEN involved_local_authorities THEN THE comune ROLE SHALL ADDRESS FINALLY involved_competent_roles(comune)
    */
    id = "regions_tn_involve_competent_roles",

    params = List(),
    pre = GroundPredicate("involved_local_authorities_tn", List()),
    post = GroundPredicate("involved_competent_roles_tn", List(AtomTerm("regions_tn"))),

    effects = Array(
      EvolutionGrounding("involve_regions_tn", Array[EvoOperator](
        AddOperator(Predicate("involved_competent_roles_tn", List(AtomTerm("regions_tn")))),
      )),
    ),
    future = List.empty
  )

  val regions_tn_decide_response_type = AbstractCapability(
    /*
    GOAL comune_decide_response_type: WHEN involved_competent_roles(comune) THEN THE comune ROLE SHALL ADDRESS FINALLY decided_response_type(comune)
    */
    id = "regions_tn_decide_response_type",

    params = List(),
    pre = GroundPredicate("involved_competent_roles_tn", List(AtomTerm("regions_tn"))),
    post = GroundPredicate("decided_response_type_tn", List(AtomTerm("regions_tn"))),

    effects = Array(
      EvolutionGrounding("decide_response_tn_regions", Array[EvoOperator](
        AddOperator(Predicate("decided_response_type_tn", List(AtomTerm("regions_tn")))),
      )),
    ),
    future = List.empty
  )

  val regions_tn_inform_citizen = AbstractCapability(
    /*
    GOAL comune_inform_citizen: WHEN decided_response_type(comune) THEN THE comune ROLE SHALL ADDRESS FINALLY informed_citizens(comune)
    */
    id = "regions_tn_inform_citizen",

    params = List(),
    pre = GroundPredicate("decided_response_type_tn", List(AtomTerm("regions_tn"))),
    post = GroundPredicate("informed_citizens_tn", List(AtomTerm("regions_tn"))),

    effects = Array(
      EvolutionGrounding("inform_citizen_tn_regions", Array[EvoOperator](
        AddOperator(Predicate("informed_citizens_tn", List(AtomTerm("regions_tn")))),
      )),
    ),
    future = List.empty
  )

  val regions_tn_monitor_event_severity = AbstractCapability(
    /*
    GOAL comune_monitor_event_severity: WHEN informed_citizens(comune) THEN THE comune ROLE SHALL ADDRESS FINALLY assessed_event_severity_comune
    */
    id = "regions_tn_monitor_event_severity_tn",

    params = List(),
    pre = GroundPredicate("informed_citizens_tn", List(AtomTerm("regions_tn"))),
    post = GroundPredicate("assessed_event_severity_tn", List(AtomTerm("regions_tn"))),

    effects = Array(
      EvolutionGrounding("assess_event_tn_regions", Array[EvoOperator](
        AddOperator(Predicate("assessed_event_severity_tn", List(AtomTerm("regions_tn")))),
      )),
    ),
    future = List.empty
  )

  val regions_tn_inform_citizen_via_app = AbstractCapability(
    /*
    GOAL comune_inform_citizen_via_app: WHEN assessed_event_severity_comune THEN THE comune ROLE SHALL ADDRESS FINALLY  inform_via_app(comune)
    */
    id = "regions_tn_inform_citizen_via_app_tn",

    params = List(),
    pre = GroundPredicate("assessed_event_severity_tn", List(AtomTerm("regions_tn"))),
    post = GroundPredicate("inform_via_app_tn", List(AtomTerm("regions_tn"))),

    effects = Array(
      EvolutionGrounding("inform_tn_regions", Array[EvoOperator](
        AddOperator(Predicate("inform_via_app_tn", List(AtomTerm("regions_tn")))),
      )),
    ),
    future = List.empty
  )

  /*----------------------------------------------------------------------------*/
  /*                         SUBPROCESS ARIANA                                    */
  /*----------------------------------------------------------------------------*/
  val ariana_involve_competent_roles = AbstractCapability(
    /*
    GOAL pcct_involve_competent_roles: WHEN involved_local_authorities THEN THE pcct ROLE SHALL ADDRESS FINALLY involved_competent_roles(pcct)
    */
    id = "ariana_involve_competent_roles",

    params = List(),
    pre = GroundPredicate("involved_local_authorities_tn", List()),
    post = GroundPredicate("involved_competent_roles_tn", List(AtomTerm("ariana"))),

    effects = Array(
      EvolutionGrounding("involve_ariana", Array[EvoOperator](
        AddOperator(Predicate("involved_competent_roles_tn", List(AtomTerm("ariana")))),
      )),
    ),
    future = List.empty
  )

  val ariana_decide_response_type = AbstractCapability(
    /*
    GOAL pcct_decide_response_type: WHEN involved_competent_roles(pcct) THEN THE pcct ROLE SHALL ADDRESS FINALLY decided_response_type(pcct)
    */
    id = "ariana_decide_response_type",

    params = List(),
    pre = GroundPredicate("involved_competent_roles_tn", List(AtomTerm("ariana"))),
    post = GroundPredicate("decided_response_type_tn", List(AtomTerm("ariana"))),

    effects = Array(
      EvolutionGrounding("decide_response_tn_ariana", Array[EvoOperator](
        AddOperator(Predicate("decided_response_type_tn", List(AtomTerm("ariana")))),
      )),
    ),
    future = List.empty
  )

  val ariana_inform_citizen = AbstractCapability(
    /*
    GOAL pcct_inform_citizen: WHEN decided_response_type(pcct) THEN THE pcct ROLE SHALL ADDRESS FINALLY informed_citizens(pcct)
    */
    id = "ariana_inform_citizen",

    params = List(),
    pre = GroundPredicate("decided_response_type_tn", List(AtomTerm("ariana"))),
    post = GroundPredicate("informed_citizens_tn", List(AtomTerm("ariana"))),

    effects = Array(
      EvolutionGrounding("inform_citizen_tn_ariana", Array[EvoOperator](
        AddOperator(Predicate("informed_citizens_tn", List(AtomTerm("ariana")))),
      )),
    ),
    future = List.empty
  )

  val ariana_monitor_event_severity = AbstractCapability(
    /*
    GOAL pcct_monitor_event_severity: WHEN informed_citizens(pcct) THEN THE pcct ROLE SHALL ADDRESS FINALLY assessed_event_severity_pcct
    */
    id = "ariana_monitor_event_severity",

    params = List(),
    pre = GroundPredicate("informed_citizens_tn", List(AtomTerm("ariana"))),
    post = GroundPredicate("assessed_event_severity_tn", List(AtomTerm("ariana"))),

    effects = Array(
      EvolutionGrounding("assess_event_ariana", Array[EvoOperator](
        AddOperator(Predicate("assessed_event_severity_tn", List(AtomTerm("ariana")))),
      )),
    ),
    future = List.empty
  )

  val ariana_inform_citizen_via_app = AbstractCapability(
    /*
    GOAL ariana_inform_citizen_via_app: WHEN assessed_event_severity_pcct THEN THE pcct ROLE SHALL ADDRESS FINALLY  inform_via_app(pcct)
    */
    id = "ariana_inform_citizen_via_app",

    params = List(),
    pre = GroundPredicate("assessed_event_severity_tn", List(AtomTerm("ariana"))),
    post = GroundPredicate("inform_via_app_tn", List(AtomTerm("ariana"))),

    effects = Array(
      EvolutionGrounding("inform_ariana", Array[EvoOperator](
        AddOperator(Predicate("inform_via_app_tn", List(AtomTerm("ariana")))),
      )),
    ),
    future = List.empty
  )


  val identifying_incident = AbstractCapability(
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

  val involve_pertinent_roles_ct_mayor = AbstractCapability(
    id = "involve_pertinent_roles_ct_mayor",

    params = List(),

    pre = GroundPredicate("scenario_evaluated", List()),
    post = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("ct_mayor"))),

    effects = Array(
      EvolutionGrounding("involve_bodies", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies", List(AtomTerm("ct_mayor")))),
      )),
    ),
    future = List.empty
  )

  val involve_pertinent_roles_inm = AbstractCapability(
    id = "involve_pertinent_roles_inm",

    params = List(),
    pre = GroundPredicate("scenario_evaluated", List()),
    post = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("inm"))),

    effects = Array(
      EvolutionGrounding("involve_bodies", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies", List(AtomTerm("inm")))),
      )),
    ),
    future = List.empty
  )

  val update_airborne_dispersion_data = AbstractCapability(
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

  val involve_pertinent_roles_irib = AbstractCapability(
    id = "involve_pertinent_roles_irib",

    params = List(),
    pre = GroundPredicate("updated_airborne_dispersion_data", List()),
    post = GroundPredicate("involved_pertinent_bodies", List(AtomTerm("irib"))),

    effects = Array(
      EvolutionGrounding("involve_irib", Array[EvoOperator](
        AddOperator(Predicate("involved_pertinent_bodies", List(AtomTerm("irib")))),
      )),
    ),
    future = List.empty
  )

  val update_health_risk_data = AbstractCapability(
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

  val inform_involved_crossborder_authorities = AbstractCapability(
    id = "inform_involved_crossborder_authorities",

    params = List(),
    pre = GroundPredicate("updated_health_risk_data", List()),
    post = GroundPredicate("involved_crossborder_authorities", List()),

    effects = Array(
      EvolutionGrounding("involve_auths", Array[EvoOperator](
        AddOperator(Predicate("involved_crossborder_authorities", List())),
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
    involve_pertinent_roles_ct_mayor,
    //ask_for_airborne_dispersion_estimate,
    involve_pertinent_roles_inm,
    update_airborne_dispersion_data,
    //ask_for_health_risk_estimate,
    involve_pertinent_roles_irib,
    update_health_risk_data,
    inform_involved_local_authorities,
    inform_involved_crossborder_authorities,



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
    //----
    identifying_incident_tn,
    involve_pertinent_roles_pctn,
    evaluate_incident_scenario_tn,
    ask_for_airborne_dispersion_estimate_tn,
    involve_pertinent_roles_inm_tn,
    update_airborne_dispersion_data_tn,
    ask_for_health_risk_estimate_tn,
    involve_pertinent_roles_hama,
    update_health_risk_data_tn,
    inform_involved_local_authorities_tn,

    regions_tn_involve_competent_roles,
    regions_tn_decide_response_type,
    regions_tn_inform_citizen,
    regions_tn_monitor_event_severity,
    regions_tn_inform_citizen_via_app,

    ariana_involve_competent_roles,
    ariana_decide_response_type,
    ariana_inform_citizen,
    ariana_monitor_event_severity,
    ariana_inform_citizen_via_app,

  )

  val env_action: Array[AbstractCapability] = Array.empty

  /* the problem */
  val initial = StateOfWorld(List(GroundPredicate("emergency_location", List(AtomTerm("volcano")))))
  //val initial = Conjunction(List(GroundPredicate("emergency_location", List(AtomTerm("volcano"))),GroundPredicate("emergency_location_tn", List(AtomTerm("volcano")))))

  val initial_comunepcct = StateOfWorld(List(GroundPredicate("involved_local_authorities", List())))

  val initial_arianaregions = StateOfWorld(List(GroundPredicate("involved_local_authorities_tn", List())))



  val availableActions = AvailableActions(sys_action, env_action)
}
