package org.icar.GoalSPECParser

import scala.util.parsing.combinator.JavaTokenParsers

class GoalParserSkeleton extends JavaTokenParsers {
  /*---------------------------------------------GOALS---------------------------------------------*/

  def goal: Parser[Any] = alt_decomposition | and_decomposition | leaf_goal

  def alt_decomposition: Parser[Any] = "GOAL" ~> ident <~ "IS" <~ "OR" ~> "OF" ~> "{" ~> goal_list <~ "}"

  def and_decomposition: Parser[Any] = "GOAL" ~> ident <~ "IS" <~ "AND" ~> "OF" ~> "{" ~> goal_list <~ "}"

  def goal_list: Parser[Any] = goal ~ goal_list | goal

  def leaf_goal: Parser[Any] = social_goal | system_goal

  def system_goal: Parser[Any] = "GOAL" ~> ident <~ ":" ~> trigger_condition <~ "THEN" ~> actor_list <~ "SHALL" ~> "ADDRESS" ~> final_condition

  def social_goal: Parser[Any] = "SOCIAL GOAL" ~> ident <~ ":" ~> trigger_condition ~ individual_actor <~ "SHALL" ~> "ADDRESS" ~> final_condition

  /*---------------------------------------------ACTORS---------------------------------------------*/
  def actor_list: Parser[Any] = actor ~ "AND" ~ actor | actor

  def actor: Parser[Any] = individual_actor | collective_actor

  def individual_actor: Parser[Any] = "THE_SYSTEM" | "THE" ~> ident ~ "ROLE";

  def collective_actor: Parser[Any] = "THE" ~> ident ~ "GROUP";

  /*---------------------------------------------TRIGGER CONDITION---------------------------------------------*/

  def trigger_condition: Parser[Any] = observed_event | message_received_event;

  def observed_event: Parser[Any] = "WHEN" ~> state;

  def message_received_event: Parser[Any] = "WHEN MESSAGE" ~> atom <~ "RECEIVED" <~ "FROM" ~> actor; //ident == msg_label

  def final_condition: Parser[Any] = quantification ~ LTL_statement | LTL_statement

  /*---------------------------------------------STATE---------------------------------------------*/

  def state: Parser[Any] =
    atom ~ bin_op ~ state | // MODIFICA apportata qui
      "NOT" ~ atom |
      "(" ~ atom ~ ")" |
      atom

  /*---------------------------------------------STATEMENT---------------------------------------------*/

  def LTL_statement: Parser[Any] =
    "(" ~> LTL_statement <~ ")" |
      ltl_op ~ LTL_statement |
      "not" ~ LTL_statement |
      state

  def ltl_op: Parser[Any] = "FINALLY" | "GLOBALLY" | "NEXT" | "UNTIL" | "RELEASE"

  /*---------------------------------------------OPs---------------------------------------------*/

  def quantifier_op: Parser[Any] = "FOREACH" | "FORALL"

  def expr_op: Parser[Any] = "==" | "<" | "<=" | ">" | ">="

  def expression: Parser[Any] = operand ~ expr_op ~ operand | operand

  def variable_list: Parser[Any] = variable <~ "," ~> variable_list | variable

  def variable: Parser[Any] = "?" ~ ident

  def operand: Parser[Any] = variable | constant

  def argument: Parser[Any] = variable | constant | "_"

  def argument_list: Parser[Any] = argument <~ "," ~> argument_list | argument

  /*---------------------------------------------FOL QUANTIFIER---------------------------------------------*/

  def quantification: Parser[Any] = universal_quantification | existential_quantification

  def universal_quantification: Parser[Any] = "FOREACH" ~> argument_list ~ state

  def existential_quantification: Parser[Any] = "FORALL" ~> argument_list ~ state

  def predicate: Parser[Any] = ident ~ "(" ~ opt(argument_list) ~ ")"

  def constant: Parser[Any] =
    floatingPointNumber |
      stringLiteral

  def atom: Parser[Any] =
    "true" |
      "false" |
      predicate //|
      //expression

  def bin_op: Parser[Any] = "NOT" | "AND" | "OR" | "->" | "<->"

}

object testParser extends GoalParserSkeleton {
  def main(args: Array[String]): Unit = {

    val goalExample_1 = "GOAL accept_new_issues_goal : WHEN MESSAGE issue_vote_list(?IssueList) RECEIVED FROM THE user ROLE THEN THE issue_manager ROLE SHALL ADDRESS FINALLY accepted(?IssueList)"

    val declare_alarm_state = "GOAL Declare_alarm_state : WHEN potential_incident OR declared_incident OR updated_event THEN THE prefect ROLE SHALL ADDRESS FINALLY (alarm_state(attention) OR alarm_state(pre_alert) OR alarm_state(alert) OR alarm_state(normality))"
    val Evaluate_event_severity = "GOAL Evaluate_event_severity : WHEN true THEN THE dummy_group GROUP SHALL ADDRESS true"
    val Decide_response_type = "GOAL Decide_response_type : WHEN true THEN THE dummy_group GROUP SHALL ADDRESS true"

    val Evaluate_incident_scenario = s"GOAL Evaluate_incident_scenario IS AND OF {\n${Evaluate_event_severity}\n${Decide_response_type}}"
    val Support_stragic_decisions = s"GOAL Support_stragic_decisions IS AND OF {\n${declare_alarm_state}\n${Evaluate_incident_scenario}\n}"


    val composite_goal = "GOAL Support_stragic_decisions IS AND OF { GOAL Declare_alarm_state : WHEN potential_incident THEN THE prefect ROLE SHALL ADDRESS FINALLY (alarm_state(attention)) }"
    val inner = "GOAL Declare_alarm_state : WHEN potential_incident THEN THE prefect ROLE SHALL ADDRESS FINALLY (alarm_state(attention))"
    val composite_goal2 = s"GOAL Support_stragic_decisions IS AND OF { ${inner} }"


    val vv = parseAll(final_condition, "alarm_state('pre_alert') OR alarm_state('pre_alert2')")
    println(parseAll(final_condition, "alarm_state('pre_alert') OR alarm_state('pre_alert2')"))

    println(s"composite: ${Support_stragic_decisions}\n")
    println(parseAll(goal, Support_stragic_decisions))

  }
}
