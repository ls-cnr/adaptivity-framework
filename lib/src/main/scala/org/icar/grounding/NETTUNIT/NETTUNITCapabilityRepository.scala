package org.icar.grounding.NETTUNIT

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}

object NETTUNITRepository extends CapabilityRepository {
  add("send_team_to_evaluate",
    ConcreteCapability(0,
      "Send team to evaluate",
      true,
      "send_team_to_evaluate",
      "nettunit.handler.send_team_to_evaluate",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )






  add("activate_internal_security_plan",
    ConcreteCapability(1,
      "Activate internal plan",
      true,
      "activate_internal_security_plan",
      "nettunit.handler.activate_internal_security_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )





  add("notify_competent_body_internal_plan",
    ConcreteCapability(2,
      "Notify competent body",
      false,
      "notify_competent_body_internal_plan",
      "nettunit.handler.notify_competent_body_internal_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("notify_competent_body_internal_plan",
    ConcreteCapability(14,
      "Notify competent body",
      false,
      "notify_competent_body_internal_plan",
      "nettunit.handler.alternative_services.notify_competent_body_internal_plan_alt", //Alternative service implementation
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )


  add("inform_technical_rescue_organisation_internal_plan",
    ConcreteCapability(3,
      "Inform technical rescue orgs.",
      false,
      "inform_technical_rescue_organisation_internal_plan",
      "nettunit.handler.inform_technical_rescue_organisation_internal_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("inform_technical_rescue_organisation_internal_plan",
    ConcreteCapability(15,
      "[ALT] Inform technical rescue orgs.",
      false,
      "inform_technical_rescue_organisation_internal_plan",
      "nettunit.handler.alternative_services.inform_technical_rescue_organisation_internal_plan_alt", //Alternative service implementation
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("decide_response_type",
    ConcreteCapability(4,
      "Decide response type",
      true,
      "decide_response_type",
      "nettunit.handler.decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("prepare_tech_report",
    ConcreteCapability(5,
      "Prepare tech report",
      false,
      "prepare_tech_report",
      "nettunit.handler.prepare_tech_report",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("keep_update_involved_personnel",
    ConcreteCapability(6,
      "Keep update involved personnel",
      false,
      "keep_update_involved_personnel",
      "nettunit.handler.keep_update_involved_personnel",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("declare_pre_alert_state",
    ConcreteCapability(7,
      "Declare pre-alert state",
      true,
      "declare_pre_alert_state",
      "nettunit.handler.declare_pre_alert_state",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("inform_technical_rescue_organisation_alert",
    ConcreteCapability(8,
      "Inform technical rescue orgs.",
      false,
      "inform_technical_rescue_organisation_alert",
      "nettunit.handler.inform_technical_rescue_organisation_alert",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("evaluate_fire_radiant_energy",
    ConcreteCapability(9,
      "Evaluate fire radiant energy",
      true,
      "evaluate_fire_radiant_energy",
      "nettunit.handler.evaluate_fire_radiant_energy",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("declare_alarm_state",
    ConcreteCapability(10,
      "Declare alarm state",
      true,
      "declare_alarm_state",
      "nettunit.handler.declare_alarm_state",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("ensure_presence_of_qualified_personnel",
    ConcreteCapability(11,
      "Ensure presence qualified personnel",
      false,
      "ensure_presence_of_qualified_personnel",
      "nettunit.handler.ensure_presence_of_qualified_personnel",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("ensure_presence_of_representative",
    ConcreteCapability(12,
      "Ensure presence representative",
      false,
      "ensure_presence_of_representative",
      "nettunit.handler.ensure_presence_of_representative",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("do_crossborder_communication",
    ConcreteCapability(13,
      "Do crossborder communication",
      false,
      "do_crossborder_communication",
      "nettunit.handler.do_crossborder_communication",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
}