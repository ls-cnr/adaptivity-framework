package org.icar.grounding.NETTUNIT

import org.icar.grounding.{CapabilityRepository, ConcreteCapability}

object NETTUNITRepository extends CapabilityRepository {
  add("send_team_to_evaluate",
    ConcreteCapability(0,
      "send_team_to_evaluate",
      "nettunit.handler.send_team_to_evaluate",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("activate_internal_security_plan",
    ConcreteCapability(1,
      "activate_internal_security_plan",
      "nettunit.handler.activate_internal_security_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("notify_competent_body_internal_plan",
    ConcreteCapability(2,
      "notify_competent_body_internal_plan",
      "nettunit.handler.notify_competent_body_internal_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("inform_technical_rescue_organisation_internal_plan",
    ConcreteCapability(3,
      "inform_technical_rescue_organisation_internal_plan",
      "nettunit.handler.inform_technical_rescue_organisation_internal_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("decide_response_type",
    ConcreteCapability(4,
      "decide_response_type",
      "nettunit.handler.decide_response_type",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("prepare_tech_report",
    ConcreteCapability(5,
      "prepare_tech_report",
      "nettunit.handler.prepare_tech_report",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("keep_update_involved_personnel",
    ConcreteCapability(6,
      "keep_update_involved_personnel",
      "nettunit.handler.keep_update_involved_personnel",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("declare_pre_alert_state",
    ConcreteCapability(7,
      "declare_pre_alert_state",
      "nettunit.handler.declare_pre_alert_state",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("inform_technical_rescue_organisation_alert",
    ConcreteCapability(8,
      "inform_technical_rescue_organisation_alert",
      "nettunit.handler.inform_technical_rescue_organisation_alert",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("evaluate_fire_radiant_energy",
    ConcreteCapability(9,
      "evaluate_fire_radiant_energy",
      "nettunit.handler.evaluate_fire_radiant_energy",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("declare_alarm_state",
    ConcreteCapability(10,
      "declare_alarm_state",
      "nettunit.handler.declare_alarm_state",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("ensure_presence_of_qualified_personnel",
    ConcreteCapability(11,
      "ensure_presence_of_qualified_personnel",
      "nettunit.handler.ensure_presence_of_qualified_personnel",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("ensure_presence_of_representative",
    ConcreteCapability(12,
      "ensure_presence_of_representative",
      "nettunit.handler.ensure_presence_of_representative",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
  add("do_crossborder_communication",
    ConcreteCapability(13,
      "do_crossborder_communication",
      "nettunit.handler.do_crossborder_communication",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )
}