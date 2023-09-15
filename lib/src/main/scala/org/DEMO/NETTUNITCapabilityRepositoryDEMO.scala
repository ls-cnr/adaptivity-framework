package org.DEMO

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}

object NETTUNITRepositoryDEMO extends CapabilityRepository {


  add("identifying_incident",
    ConcreteCapability(0,
      "Identifying Incident",
      true,
      "identifying_incident",
      "nettunit.handler.demo.it.identifying_incident",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_pcrs",
    ConcreteCapability(1,
      "Involve Pertinent Roles Pcrs",
      false,
      "involve_pertinent_roles_pcrs",
      "nettunit.handler.demo.it.involve_pertinent_roles_pcrs",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("evaluate_incident_scenario",
    ConcreteCapability(2,
      "Evaluate Incident Scenario",
      false,
      "evaluate_incident_scenario",
      "nettunit.handler.demo.it.evaluate_incident_scenario",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_ct_mayor",
    ConcreteCapability(1,
      "Involve Pertinent Roles CT Mayor",
      false,
      "involve_pertinent_roles_ct_mayor",
      "nettunit.handler.demo.it.involve_pertinent_roles_ct_mayor",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("involve_pertinent_roles_ct_vicemayor",
    ConcreteCapability(1,
      "Involve Pertinent Roles CT Vice Mayor",
      false,
      "involve_pertinent_roles_ct_mayor",
      "nettunit.handler.demo.it.involve_pertinent_roles_ct_vice_mayor",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_inm",
    ConcreteCapability(4,
      "Involve Pertinent Roles Inm",
      false,
      "involve_pertinent_roles_inm",
      "nettunit.handler.demo.it.involve_pertinent_roles_inm",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("update_airborne_dispersion_data",
    ConcreteCapability(5,
      "Update Airborne Dispersion Data",
      false,
      "update_airborne_dispersion_data",
      "nettunit.handler.demo.it.update_airborne_dispersion_data",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_irib",
    ConcreteCapability(7,
      "Involve Pertinent Roles Irib",
      false,
      "involve_pertinent_roles_irib",
      "nettunit.handler.demo.it.involve_pertinent_roles_irib",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("update_health_risk_data",
    ConcreteCapability(8,
      "Update Health Risk Data",
      false,
      "update_health_risk_data",
      "nettunit.handler.demo.it.update_health_risk_data",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("inform_pcct",
    ConcreteCapability(9,
      "Inform PCCT",
      false,
      "inform_pcct",
      "nettunit.handler.demo.it.inform_pcct",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("inform_territory",
    ConcreteCapability(9,
      "Inform Territory",
      false,
      "inform_territory",
      "nettunit.handler.demo.it.inform_territory",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("inform_involved_crossborder_authorities",
    ConcreteCapability(9,
      "Inform Involved Crossborder Authorities",
      false,
      "inform_involved_crossborder_authorities",
      "nettunit.handler.demo.it.inform_involved_crossborder_authorities",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )





  add("comune_involve_competent_roles",
    ConcreteCapability(10,
      "Comune Involve Competent Roles",
      false,
      "comune_involve_competent_roles",
      "nettunit.handler.demo.it.comune_involve_competent_roles",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_decide_response_type",
    ConcreteCapability(11,
      "Comune Decide Response Type",
      false,
      "comune_decide_response_type",
      "nettunit.handler.demo.it.comune_decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_inform_citizen",
    ConcreteCapability(12,
      "Comune Inform Citizen",
      false,
      "comune_inform_citizen",
      "nettunit.handler.demo.it.comune_inform_citizen",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_monitor_event_severity",
    ConcreteCapability(13,
      "Comune Monitor Event Severity",
      false,
      "comune_monitor_event_severity",
      "nettunit.handler.demo.it.comune_monitor_event_severity",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_inform_citizen_via_app",
    ConcreteCapability(14,
      "Comune Inform Citizen Via App",
      false,
      "comune_inform_citizen_via_app",
      "nettunit.handler.demo.it.comune_inform_citizen_via_app",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )




  add("pcct_involve_competent_roles",
    ConcreteCapability(15,
      "Pc Ct Involve Competent Roles",
      false,
      "pcct_involve_competent_roles",
      "nettunit.handler.demo.it.pcct_involve_competent_roles",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_decide_response_type",
    ConcreteCapability(16,
      "Pc Ct Decide Response Type",
      false,
      "pcct_decide_response_type",
      "nettunit.handler.demo.it.pcct_decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_inform_citizen",
    ConcreteCapability(17,
      "Pc Ct Inform Citizen",
      false,
      "pcct_inform_citizen",
      "nettunit.handler.demo.it.pcct_inform_citizen",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_monitor_event_severity",
    ConcreteCapability(18,
      "Pc Ct Monitor Event Severity",
      false,
      "pcct_monitor_event_severity",
      "nettunit.handler.demo.it.pcct_monitor_event_severity",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_inform_citizen_via_app",
    ConcreteCapability(19,
      "Pc Ct Inform Citizen Via App",
      false,
      "pcct_inform_citizen_via_app",
      "nettunit.handler.demo.it.pcct_inform_citizen_via_app",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )



  /** ******************************************** */
  /** ******************************************** */
  /*TN*/
  /** ******************************************** */
  /** ******************************************** */
  add("identifying_incident_tn",
    ConcreteCapability(20,
      "Identifying Incident TN",
      false,
      "identifying_incident_tn",
      "nettunit.handler.demo.tn.identifying_incident_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_pctn",
    ConcreteCapability(21,
      "Involve Pertinent Roles Pctn",
      false,
      "involve_pertinent_roles_pctn",
      "nettunit.handler.demo.tn.involve_pertinent_roles_pctn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("evaluate_incident_scenario_tn",
    ConcreteCapability(22,
      "Evaluate Incident Scenario tn",
      false,
      "evaluate_incident_scenario_tn",
      "nettunit.handler.demo.tn.evaluate_incident_scenario_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ask_for_airborne_dispersion_estimate_tn",
    ConcreteCapability(23,
      "Ask For Airborne Dispersion Estimate_tn",
      false,
      "ask_for_airborne_dispersion_estimate_tn",
      "nettunit.handler.demo.tn.ask_for_airborne_dispersion_estimate_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_inm_tn",
    ConcreteCapability(24,
      "Involve Pertinent Roles Inm tn",
      false,
      "involve_pertinent_roles_inm_tn",
      "nettunit.handler.demo.tn.involve_pertinent_roles_inm_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("update_airborne_dispersion_data_tn",
    ConcreteCapability(25,
      "Update Airborne Dispersion Data tn",
      false,
      "update_airborne_dispersion_data_tn",
      "nettunit.handler.demo.tn.update_airborne_dispersion_data_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ask_for_health_risk_estimate_tn",
    ConcreteCapability(26,
      "Ask For Health Risk Estimate tn",
      false,
      "ask_for_health_risk_estimate_tn",
      "nettunit.handler.demo.tn.ask_for_health_risk_estimate_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_hama",
    ConcreteCapability(27,
      "Involve Pertinent Roles hama",
      false,
      "involve_pertinent_roles_hama",
      "nettunit.handler.demo.tn.involve_pertinent_roles_hama",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("update_health_risk_data_tn",
    ConcreteCapability(28,
      "Update Health Risk Data tn",
      false,
      "update_health_risk_data_tn",
      "nettunit.handler.demo.tn.update_health_risk_data_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("inform_involved_local_authorities_tn",
    ConcreteCapability(29,
      "Inform Involved Local Authorities tn",
      false,
      "inform_involved_local_authorities_tn",
      "nettunit.handler.demo.tn.inform_involved_local_authorities_tn",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  //subprocess regions
  add("regions_tn_involve_competent_roles_tn",
    ConcreteCapability(30,
      "regions tn Involve Competent Roles tn",
      false,
      "regions_tn_involve_competent_roles",
      "nettunit.handler.demo.tn.regions_tn_involve_competent_roles",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("regions_tn_decide_response_type",
    ConcreteCapability(31,
      "regions tn Decide Response Type",
      false,
      "regions_tn_decide_response_type",
      "nettunit.handler.demo.tn.regions_tn_decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("regions_tn_inform_citizen",
    ConcreteCapability(32,
      "regions Inform Citizen",
      false,
      "regions_tn_inform_citizen",
      "nettunit.handler.demo.tn.regions_tn_inform_citizen",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("regions_tn_monitor_event_severity",
    ConcreteCapability(33,
      "regions Monitor Event Severity",
      false,
      "regions_tn_monitor_event_severity",
      "nettunit.handler.demo.tn.regions_tn_monitor_event_severity",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("regions_tn_inform_citizen_via_app",
    ConcreteCapability(34,
      "regions Inform Citizen Via App",
      false,
      "regions_tn_inform_citizen_via_app",
      "nettunit.handler.demo.tn.regions_tn_inform_citizen_via_app",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  //subprocess ariana
  add("ariana_involve_competent_roles",
    ConcreteCapability(35,
      "ariana Involve Competent Roles",
      false,
      "ariana_involve_competent_roles",
      "nettunit.handler.demo.tn.ariana_involve_competent_roles",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ariana_decide_response_type",
    ConcreteCapability(36,
      "ariana Decide Response Type",
      false,
      "ariana_decide_response_type",
      "nettunit.handler.demo.tn.ariana_decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ariana_inform_citizen",
    ConcreteCapability(37,
      "ariana Inform Citizen",
      false,
      "ariana_inform_citizen",
      "nettunit.handler.demo.tn.ariana_inform_citizen",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ariana_monitor_event_severity",
    ConcreteCapability(38,
      "ariana Monitor Event Severity",
      false,
      "ariana_monitor_event_severity",
      "nettunit.handler.demo.tn.ariana_monitor_event_severity",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ariana_inform_citizen_via_app",
    ConcreteCapability(39,
      "ariana Inform Citizen Via App",
      false,
      "ariana_inform_citizen_via_app",
      "nettunit.handler.demo.tn.ariana_inform_citizen_via_app",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )


}