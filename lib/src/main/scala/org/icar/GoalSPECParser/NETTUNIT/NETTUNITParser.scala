package org.icar.GoalSPECParser.NETTUNIT

import org.icar.GoalSPECParser.GoalParserImpl
import org.icar.symbolic.GoalModel

import scala.io.Source

object NETTUNITParser extends GoalParserImpl {

  def loadGoalModelFromFile(fname: String): GoalModel = {
    val fileContent = Source.fromFile(fname).getLines().filterNot(x => x.isEmpty).mkString("\n")
    val ll = parseAll(goal, fileContent)
    GoalModel(ll.get.toArray)
  }

  /**
   *
   * @param goalModelString the goal model as a unique string
   * @return
   */
  def loadGoalModel(goalModelString: String) = GoalModel(parseAll(goal, goalModelString).get.toArray)

  def main(args: Array[String]): Unit = {
    val fname = "/Users/dguastel/Desktop/goaltreeNETTUNIT.txt"
    val fileContent = Source.fromFile(fname).getLines().filterNot(x => x.isEmpty).mkString("\n")
    val ll = parseAll(goal, fileContent)
    val goals = ll.get

    val goalModel = GoalModel(ll.get.toArray)
    println(ll)
  }

  def old_parser_tests(): Unit = {

    val goalExample_1 = "GOAL accept_new_issues_goal : WHEN MESSAGE issue_vote_list(?IssueList) RECEIVED FROM THE user ROLE THEN THE issue_manager ROLE SHALL ADDRESS FINALLY accepted(?IssueList)"

    val declare_alarm_state = "GOAL Declare_alarm_state : WHEN potential_incident OR declared_incident OR updated_event THEN THE prefect ROLE SHALL ADDRESS FINALLY (alarm_state(attention) OR alarm_state(pre_alert) OR alarm_state(alert) OR alarm_state(normality))"
    val Evaluate_event_severity = "GOAL Evaluate_event_severity : WHEN true THEN THE dummy_group GROUP SHALL ADDRESS true"
    val Decide_response_type = "GOAL Decide_response_type : WHEN true THEN THE dummy_group GROUP SHALL ADDRESS true"

    val Evaluate_incident_scenario = s"GOAL Evaluate_incident_scenario IS AND OF {\n${Evaluate_event_severity}\n${Decide_response_type}}"
    val Support_stragic_decisions = s"GOAL Support_stragic_decisions IS AND OF {\n${declare_alarm_state}\n${Evaluate_incident_scenario}\n}"

    val composite_goal = "GOAL Support_stragic_decisions IS AND OF { GOAL Declare_alarm_state : WHEN potential_incident THEN THE prefect ROLE SHALL ADDRESS FINALLY (alarm_state(attention)) }"
    val inner = "GOAL Declare_alarm_state : WHEN potential_incident THEN THE prefect ROLE SHALL ADDRESS FINALLY (alarm_state(attention))"
    val composite_goal2 = s"GOAL Support_stragic_decisions IS AND OF { ${inner} }"

    println(s"composite: ${Support_stragic_decisions}\n")

    println(parseAll(final_condition, "alarm_state('pre_alert') OR alarm_state(pre_alert2) AND NOT try_this(5.4,boh)"))

    val fc = parseAll(final_condition, "(alarm_state(attention) OR alarm_state(pre_alert) OR alarm_state(alert) OR alarm_state(normality))")

    val finally_1 = "FINALLY (alarm_state(attention) OR alarm_state(pre_alert) OR alarm_state(alert) OR alarm_state(normality))"
    val ll_finally = parseAll(LTL_statement, finally_1)

    val ll = parseAll(goal, Support_stragic_decisions)
    println(parseAll(goal, Support_stragic_decisions))
    println("ok")
  }
}