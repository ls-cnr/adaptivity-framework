package org.icar.symbolic

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Predicate logic formula parsers
 *
 * The parser accepts formulas as the following
 * (not checked(user,entertainment) and not sleeping(user)) -> (not ill(user) and ( entertainment_time(user) and not passed_entertainment_time(user) ) and location(user,living_room))
 *
 * It builds a HL_PredicateFormula object
 *
 */
class FormulaParser extends JavaTokenParsers {

	def formula : Parser[HL_PredicateFormula] = literal~bin_op~formula ^^ { case p~op~f =>
		op match {
			case "and" =>

				f match {
					case Conjunction(formulas) =>
						val a: List[HL_PredicateFormula] = for (f <- formulas) yield f.asInstanceOf[HL_PredicateFormula]
						val adds: List[HL_PredicateFormula] = p :: a
						Conjunction[HL_PredicateFormula](adds)

					case _ =>
						Conjunction[HL_PredicateFormula](List(p,f))
				}
			case "or" =>

				f match {
					case Disjunction(formulas) =>
						val a: List[HL_PredicateFormula] = for (f <- formulas) yield f.asInstanceOf[HL_PredicateFormula]
						val adds: List[HL_PredicateFormula] = p :: a
						Disjunction[HL_PredicateFormula](adds)

					case _ =>
						Disjunction[HL_PredicateFormula](List(p,f))
				}

			case "->" =>
				Implication[HL_PredicateFormula](p,f)

			case "<->" =>
				BiImplication[HL_PredicateFormula](p,f)
		}} |
		"not" ~> "(" ~> formula <~ ")" ^^ {case f => Negation(f)} |
		literal ^^ {case p => p}

	def bin_op : Parser[Any] = "and" | "or" | "->" | "<->"

	def literal : Parser[HL_PredicateFormula] = predicate ^^ {case p => p} | "not"~>predicate ^^ {case p => Negation(p)} | "(" ~> formula <~ ")" ^^ { case f => f }

	def predicate : Parser[Predicate] = ident~"("~opt(term_list)~")" ^^ {
		case func~p_open~terms~p_close => if (terms.isDefined) Predicate(func,terms.get) else Predicate(func,List[Term]())
	}


	def term_list : Parser[List[Term]] = repsep(term,",")

	def term  : Parser[Term] = constant | atom

	def constant : Parser[ConstantTerm] =
		floatingPointNumber ^^ (x => NumeralTerm(x.toDouble)) |
			stringLiteral ^^ (x => StringTerm(x))
	def atom : Parser[ConstantTerm] =
		ident ^^ (x=>AtomTerm(x)) |
			"true" ^^ (x=>TruthTerm()) |
			"false" ^^ (x=>FalsityTerm())
}


object TestFolParser extends FormulaParser {
	def main(args : Array[String]): Unit = {
		println(parseAll(formula,"(not checked(user,entertainment) and not sleeping(user)) -> (not ill(user) and ( entertainment_time(user) and not passed_entertainment_time(user) ) and location(user,living_room))"))
	}
}