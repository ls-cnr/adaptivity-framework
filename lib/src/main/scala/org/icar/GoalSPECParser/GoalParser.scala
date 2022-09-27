package org.icar.GoalSPECParser

import org.icar.GoalSPECParser.testParserImpl.parseAll
import org.icar.symbolic._

import java.time.{Duration, Period, ZonedDateTime}
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

class GoalParserImpl extends JavaTokenParsers {

  def tc: Parser[Any] = "DURATION" ~ ',' ~ duration | "StartTC" ~ ',' ~ time | "EndTC" ~ ',' ~ time

  def inter_goal_TDependency: Parser[Any] = "TDEPENDENCY" ~ ident ~ "FROM GOAL" ~ ident ~ "TO GOAL" ~ ident ~ "IMPLIES" ~ td

  def td: Parser[Any] = tdType ~ '[' ~ startTime ~ ',' ~ endTime ~ ']'

  def tdType: Parser[String] = "SF" | "FS" | "SS" | "FF"

  //NOTE: duration is in ISO8601 format. Ex.
  // - P1Y - 1 year
  // - P2M4D - 2 months and 4 days
  // - P3Y6M4DT12H30M5S - 3 years, 7 months, 4 days, 12 hours, 30 minutes, and 5 seconds
  def duration: Parser[Duration] = stringLiteral ^^ { x => Duration.parse(x) }

  def startTime: Parser[Period] = time

  def endTime: Parser[Period] = time

  def time: Parser[Period] = stringLiteral ^^ { x => Period.parse(x) }

  def dateTime: Parser[ZonedDateTime] = stringLiteral ^^ { x => ZonedDateTime.parse(x) }

  /*---------------------------------------------GOALS---------------------------------------------*/

  def goal: Parser[List[GoalSPEC]] = alt_decomposition | and_decomposition | leaf_goal ^^ { x => List(x) }

  def alt_decomposition: Parser[List[GoalSPEC]] = "GOAL" ~> ident ~> "IS" ~> "OR" ~> "OF" ~> "{" ~> spec_list <~ "}"

  def and_decomposition: Parser[List[GoalSPEC]] = "GOAL" ~> ident ~> "IS" ~> "AND" ~> "OF" ~> "{" ~> spec_list <~ "}"

  def spec_list: Parser[List[GoalSPEC]] = goal ~ spec_list ^^ { case g ~ gl => g.concat(gl) } |
    tc ~ spec_list ^^ { case _ ~ gl => gl } |
    inter_goal_TDependency ~ spec_list ^^ { case _ ~ gl => gl } |
    goal |
    tc ^^ { _ => List() } |
    inter_goal_TDependency ^^ { _ => List() }

  def leaf_goal: Parser[GoalSPEC] = social_goal | system_goal

  def system_goal: Parser[GoalSPEC] = "GOAL" ~ ident ~ ":" ~ trigger_condition ~ "THEN" ~ actor_list ~ "SHALL ADDRESS" ~ final_condition ^^ {
    case _ ~ goalname ~ _ ~ tc ~ _ ~ actor_list ~ _ ~ fc =>
      GoalSPEC(goalname, tc.asInstanceOf[HL_PredicateFormula], fc.asInstanceOf[HL_LTLFormula])
  }

  def social_goal: Parser[GoalSPEC] = "SOCIAL GOAL" ~ ident ~ ":" ~ trigger_condition ~ "THEN" ~ actor_list ~ "SHALL ADDRESS" ~ final_condition ^^ {
    case _ ~ goalname ~ _ ~ tc ~ _ ~ actor_list ~ _ ~ fc =>
      GoalSPEC(goalname, tc.asInstanceOf[HL_PredicateFormula], fc.asInstanceOf[HL_LTLFormula])
  }

  /*---------------------------------------------ACTORS---------------------------------------------*/
  def actor_list: Parser[Any] = actor ~ "AND" ~ actor | actor

  //label, è una stringa.
  def actor: Parser[Any] = individual_actor | collective_actor

  def individual_actor: Parser[Any] = "THE_SYSTEM" | "THE" ~> ident ~ "ROLE"

  def collective_actor: Parser[Any] = "THE" ~> ident ~ "GROUP"

  /*---------------------------------------------TRIGGER CONDITION---------------------------------------------*/

  def trigger_condition: Parser[HL_LTLFormula] = observed_event //| message_received_event; ??????

  def observed_event: Parser[HL_LTLFormula] = "WHEN" ~> state

  def message_received_event: Parser[Any] = "RECEIVED MESSAGE" ~> operand <~ "FROM" ~> actor

  def final_condition: Parser[HL_LTLFormula] = quantification |
    '(' ~> quantification <~ ')' |
    LTL_statement |
    '(' ~> LTL_statement <~ ')'

  /*---------------------------------------------STATEMENT---------------------------------------------*/
  def LTL_statement: Parser[HL_LTLFormula] =
    "(" ~> LTL_statement <~ ")" |
      ltl_op ~ LTL_statement ^^ {
        case op ~ s => op match {
          case "FINALLY" => Finally(s)
          case "GLOBALLY" => Globally(s)
          case "NEXT" => Next(s)
          case "NOT" => Negation(s)
        }
      } |
      state ~ "UNTIL" ~ LTL_statement ^^ { case l ~ _ ~ r => Until(l, r) } | //TODO VERIFICARE
      state ~ "RELEASE" ~ LTL_statement ^^ { case l ~ _ ~ r => Release(l, r) } | //TODO VERIFICARE
      state

  def ltl_op: Parser[Any] = "FINALLY" | "GLOBALLY" | "NEXT" | "UNTIL" | "RELEASE"

  def state: Parser[HL_LTLFormula] =
    atom ~ bin_op ~ state ^^ { case head ~ op ~ (tail: HL_LTLFormula) =>
      op match {
        case "AND" => Conjunction(List(head, tail))
        case "OR" => Disjunction(List(head, tail))
        case "->" => Implication(head, tail)
        case "<->" => BiImplication(head, tail)
      }
    } |
      "NOT" ~> state ^^ (x => Negation(x)) |
      "(" ~> state <~ ")" |
      atom
  //"NOT" ~> atom ^^ (x => Negation(x)) |
  //"(" ~> atom <~ ")" |
  //atom

  def atom: Parser[HL_LTLFormula] =
    "true" ^^ { _ => True() } |
      "false" ^^ { _ => False() } |
      predicate

  /*---------------------------------------------OPs---------------------------------------------*/

  def expr_op: Parser[Any] = "==" | "<" | "<=" | ">" | ">="

  def expression: Parser[Any] = operand ~ expr_op ~ operand | operand

  def variable: Parser[VariableTerm] = "?" ~> ident ^^ { v => VariableTerm(v) }

  def operand: Parser[Term] = variable | constant

  def argument: Parser[Term] = variable | constant //| "_"

  def argument_list: Parser[List[Term]] = repsep(argument, ",")

  def variable_list: Parser[List[VariableTerm]] = repsep(variable, ",")

  /*---------------------------------------------FOL QUANTIFIER---------------------------------------------*/

  def quantification: Parser[HL_LTLFormula] = universal_quantification | existential_quantification

  def universal_quantification: Parser[UnivQuantifier] = "FOREACH" ~> variable_list ~ LTL_statement ^^ {
    case (terms: List[Term]) ~ (formula: HL_PredicateFormula) => UnivQuantifier(terms, formula)
  }

  def existential_quantification: Parser[ExistQuantifier] = "FORALL" ~> variable_list ~ LTL_statement ^^ {
    case (terms: List[Term]) ~ (formula: HL_PredicateFormula) => ExistQuantifier(terms, formula)
  }

  def predicate: Parser[Predicate] =
    ident ~ "(" ~ argument_list <~ ")" ^^ { case f ~ _ ~ t => Predicate(f, t) } |
      ident ^^ { x => Predicate(x, List()) }

  def constant: Parser[ConstantTerm] =
    floatingPointNumber ^^ (x => NumeralTerm(x.toDouble)) |
      ident ^^ (x => StringTerm(x)) |
      "'" ~> ident <~ "'" ^^ (x => StringTerm(x)) |
      "\"" ~> ident <~ "\"" ^^ (x => StringTerm(x))

  def bin_op: Parser[Any] = "NOT" | "AND" | "OR" | "->" | "<->"
}

object testParserImpl extends GoalParserImpl {

  def main(args: Array[String]): Unit = {
    val fname = "/Users/dguastel/Desktop/goaltreeNETTUNIT.txt"
    val fileContent = Source.fromFile(fname).getLines().filterNot(x => x.isEmpty).mkString("\n")
    val ll = parseAll(goal, fileContent)
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
