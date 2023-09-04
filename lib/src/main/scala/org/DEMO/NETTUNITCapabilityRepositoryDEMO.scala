package org.DEMO

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}

object NETTUNITRepositoryDEMO extends CapabilityRepository {
  add("identifying_incident",
    ConcreteCapability(0,
      "Identifying Incident",
      false,
      "identifying_incident",
      "nettunit.handler.identifying_incident",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_pcrs",
    ConcreteCapability(1,
      "Involve Pertinent Roles Pcrs",
      false,
      "involve_pertinent_roles_pcrs",
      "nettunit.handler.involve_pertinent_roles_pcrs",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("evaluate_incident_scenario",
    ConcreteCapability(2,
      "Evaluate Incident Scenario",
      false,
      "evaluate_incident_scenario",
      "nettunit.handler.evaluate_incident_scenario",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ask_for_airborne_dispersion_estimate",
    ConcreteCapability(3,
      "Ask For Airborne Dispersion Estimate",
      false,
      "ask_for_airborne_dispersion_estimate",
      "nettunit.handler.ask_for_airborne_dispersion_estimate",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_inm",
    ConcreteCapability(4,
      "Involve Pertinent Roles Inm",
      false,
      "involve_pertinent_roles_inm",
      "nettunit.handler.involve_pertinent_roles_inm",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("update_airborne_dispersion_data",
    ConcreteCapability(5,
      "Update Airborne Dispersion Data",
      false,
      "update_airborne_dispersion_data",
      "nettunit.handler.update_airborne_dispersion_data",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("ask_for_health_risk_estimate",
    ConcreteCapability(6,
      "Ask For Health Risk Estimate",
      false,
      "ask_for_health_risk_estimate",
      "nettunit.handler.ask_for_health_risk_estimate",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("involve_pertinent_roles_irib",
    ConcreteCapability(7,
      "Involve Pertinent Roles Irib",
      false,
      "involve_pertinent_roles_irib",
      "nettunit.handler.involve_pertinent_roles_irib",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("update_health_risk_data",
    ConcreteCapability(8,
      "Update Health Risk Data",
      false,
      "update_health_risk_data",
      "nettunit.handler.update_health_risk_data",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("inform_involved_local_authorities",
    ConcreteCapability(9,
      "Inform Involved Local Authorities",
      false,
      "inform_involved_local_authorities",
      "nettunit.handler.inform_involved_local_authorities",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_involve_competent_roles",
    ConcreteCapability(10,
      "Comune Involve Competent Roles",
      false,
      "comune_involve_competent_roles",
      "nettunit.handler.comune_involve_competent_roles",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_decide_response_type",
    ConcreteCapability(11,
      "Comune Decide Response Type",
      false,
      "comune_decide_response_type",
      "nettunit.handler.comune_decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_inform_citizen",
    ConcreteCapability(12,
      "Comune Inform Citizen",
      false,
      "comune_inform_citizen",
      "nettunit.handler.comune_inform_citizen",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_monitor_event_severity",
    ConcreteCapability(13,
      "Comune Monitor Event Severity",
      false,
      "comune_monitor_event_severity",
      "nettunit.handler.comune_monitor_event_severity",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("comune_inform_citizen_via_app",
    ConcreteCapability(14,
      "Comune Inform Citizen Via App",
      false,
      "comune_inform_citizen_via_app",
      "nettunit.handler.comune_inform_citizen_via_app",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_involve_competent_roles",
    ConcreteCapability(15,
      "Pc Ct Involve Competent Roles",
      false,
      "pcct_involve_competent_roles",
      "nettunit.handler.pcct_involve_competent_roles",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_decide_response_type",
    ConcreteCapability(16,
      "Pc Ct Decide Response Type",
      false,
      "pcct_decide_response_type",
      "nettunit.handler.pcct_decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_inform_citizen",
    ConcreteCapability(17,
      "Pc Ct Inform Citizen",
      false,
      "pcct_inform_citizen",
      "nettunit.handler.pcct_inform_citizen",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_monitor_event_severity",
    ConcreteCapability(18,
      "Pc Ct Monitor Event Severity",
      false,
      "pcct_monitor_event_severity",
      "nettunit.handler.pcct_monitor_event_severity",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  add("pcct_inform_citizen_via_app",
    ConcreteCapability(19,
      "Pc Ct Inform Citizen Via App",
      false,
      "pcct_inform_citizen_via_app",
      "nettunit.handler.pcct_inform_citizen_via_app",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )


}