package org.icar.symbolic

trait HL_PredicateFormula
trait HL_GroundLiteral
trait HL_LTLFormula
trait Axiom


/******* PREDICATE AND TEMPORAL ********/
/*** Definition is at the end of the file ***
case class GroundPredicate extends HL_PredicateFormula with HL_LTLFormula
case class Predicate extends HL_PredicateFormula with HL_LTLFormula
*/
case class True() extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "\u22A4"
}
case class False() extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "\u22A5"
}

case class ExistQuantifier(vars: List[VariableTerm], formula : HL_PredicateFormula) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "\u2203 "+vars.mkString(",")+":"+formula
}

case class UnivQuantifier(vars : List[VariableTerm], formula : HL_PredicateFormula) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "\u2200 "+vars.mkString(",")+":"+formula
}

case class Negation[A](formula : A) extends HL_PredicateFormula with HL_LTLFormula {
	//override def toString: String = "-"+formula
	override def toString: String = "!("+formula+")"
}
case class Conjunction[A](formulas : List[A]) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "("+formulas.mkString(" and ")+")"
}
case class Disjunction[A](formulas : List[A]) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "("+formulas.mkString(" or ")+")"
}
case class ExclDisj[A](formulas : List[A]) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "("+formulas.mkString(" xor ")+")"
}
case class Implication[A](l:A, r:A) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "("+l+" -> "+r+")"
}
case class BiImplication[A](l:A, r:A) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString: String = "("+l+" <-> "+r+")"
}

case class Globally(formula : HL_LTLFormula) extends HL_LTLFormula {
	override def toString: String = "("+"G "+formula+")"
}
case class Finally(formula : HL_LTLFormula) extends HL_LTLFormula {
	override def toString: String = "("+"F "+formula+")"
}
case class Next(formula : HL_LTLFormula) extends HL_LTLFormula {
	override def toString: String = "("+"X "+formula+")"
}
case class Until(left : HL_LTLFormula, right : HL_LTLFormula) extends HL_LTLFormula {
	override def toString: String = "("+left+" U "+right+")"
}
case class Release(left : HL_LTLFormula, right : HL_LTLFormula) extends HL_LTLFormula {
	override def toString: String = "("+left+" R "+right+")"
}



/******* PREDICATE TERMS ********/
sealed abstract class Term
abstract class ConstantTerm extends Term {
	override def hashCode(): Int = toString.hashCode
}
case class AnonymousTerm() extends Term {
	override def toString: String = "_"
}
case class VariableTerm(name : String) extends Term {
	override def toString: String = s"var($name)"
}
case class AtomTerm(atom : String) extends ConstantTerm {
	override def toString: String = atom
}
case class NumeralTerm(num : Double) extends ConstantTerm {
	override def toString: String = num.toString
}
case class IntegerTerm(num : Int) extends ConstantTerm {
	override def toString: String = num.toString
}
case class TruthTerm() extends ConstantTerm {
	override def toString: String = "true"
}
case class FalsityTerm() extends ConstantTerm {
	override def toString: String = "false"
}
case class StringTerm(str : String) extends ConstantTerm {
	override def toString: String = s"'$str'"
}



/******* PRODUCTION RULES ********/
case class Rule(consequent:Predicate, rhr:RuleAntecedent) extends Axiom
case class RuleAntecedent(terms:List[RuleCondition])

abstract class RuleCondition
case class PredicateCondition(p:Predicate) extends RuleCondition
case class NegateCondition(p:Predicate) extends RuleCondition

abstract class TestCondition extends RuleCondition
case class EqualTestCondition(test:ConstantTerm) extends TestCondition
case class LessTestCondition() extends TestCondition
case class GreaterTestCondition() extends TestCondition




/******* ATOMIC FORMULAS: PREDICATES AND GROUND PREDICATES ********/
case class Predicate(functional:String, terms: List[Term] ) extends HL_PredicateFormula with HL_LTLFormula {
	override def toString : String = functional+"("+term_list_string+")"
	private def term_list_string : String = {
		var a_string: String = ""
		for (i <- terms.indices) {
			a_string += terms(i).toString
			if (i<terms.length-1)
				a_string += ","
		}
		a_string
	}

	def isGround : Boolean = {
		var ground = true
		for (t<-terms if t.isInstanceOf[VariableTerm])
			ground = false
		ground
	}

	def get_grounded : Option[GroundPredicate] = {
		if (isGround) {
			var array : List[ConstantTerm] = List.empty
			for (t<-terms)
				t match {
					case AtomTerm(a) => array = AtomTerm(a) :: array
					case StringTerm(s) => array = StringTerm(s) :: array
					case NumeralTerm(n) => array = NumeralTerm(n) :: array
					case IntegerTerm(i) => array = IntegerTerm(i) :: array
					case TruthTerm() => array = TruthTerm() :: array
					case FalsityTerm() => array = FalsityTerm() :: array
					case _ => array = FalsityTerm() :: array
				}
			Some(GroundPredicate(this.functional,array.reverse))

		} else {
			None
		}
	}


	def to_ground(assignments : Map[VariableTerm,ConstantTerm]):GroundPredicate = {
		val ground_terms = for (t<-terms) yield replace_var(t,assignments)
		GroundPredicate(functional,ground_terms)
	}


	private def replace_var(t: Term,assignments : Map[VariableTerm,ConstantTerm]):ConstantTerm = {
		t match {
			case AtomTerm(_) => t.asInstanceOf[AtomTerm]
			case StringTerm(_) => t.asInstanceOf[StringTerm]
			case NumeralTerm(_) => t.asInstanceOf[NumeralTerm]
			case IntegerTerm(_) => t.asInstanceOf[IntegerTerm]
			case TruthTerm() => TruthTerm()
			case FalsityTerm() => FalsityTerm()

			case VariableTerm(name) =>
				assignments(VariableTerm(name))
			case _=> FalsityTerm()
		}
	}
}

case class NegatedGroundPredicate(p:GroundPredicate)  extends HL_GroundLiteral
case class GroundPredicate(functional:String, terms: List[ConstantTerm] ) extends HL_PredicateFormula with HL_GroundLiteral with HL_LTLFormula {

	override def toString : String = functional+"("+term_list_string+")"
	def term_list_string : String = {
		var a_string: String = ""

		for (i <- terms.indices) {
			if (terms(i).isInstanceOf[NumeralTerm]) {
				val n = terms(i).asInstanceOf[NumeralTerm]
				a_string += n.num.toInt
			} else
				a_string += terms(i).toString
			if (i<terms.length-1)
				a_string += ","

		}
		a_string
	}

	override def hashCode(): Int = {
		var h = functional.hashCode
		for (t<-terms) h+= t.hashCode()
		h
	}

	def as_pred : Predicate = Predicate(functional,for(t<-terms) yield t.asInstanceOf[Term])
}




object HL_PredicateFormula {
	def substitution(f:HL_PredicateFormula, assigned:Map[String,ConstantTerm]) : HL_PredicateFormula = {
		f match {
			case p : Predicate =>
				val p1 = pred_substitution(p,assigned)
				val opt_p2 = p1.get_grounded
				if (opt_p2.isDefined)
					opt_p2.get
				else
					p1
			case p:GroundPredicate => p
			case True() => True()
			case False() => False()
			case Disjunction(sf) =>
				val subterms = for (t<-sf) yield substitution(t.asInstanceOf[HL_PredicateFormula],assigned)
				Disjunction(subterms)
			case Conjunction(sf) =>
				val subterms = for (t<-sf) yield substitution(t.asInstanceOf[HL_PredicateFormula],assigned)
				Conjunction(subterms)
			case Negation(op) => Negation(substitution(op.asInstanceOf[HL_PredicateFormula],assigned))
			case ExistQuantifier(vars,f) =>
				f match {
					case p:Predicate =>
						var new_vars : List[VariableTerm] = List.empty
						for (v<-vars) if (!assigned.contains(v.name)) new_vars = v :: new_vars
						if (new_vars.nonEmpty)
							ExistQuantifier(new_vars.reverse,substitution(p,assigned))
						else
							substitution(p,assigned)
					case True() => True()
					case False() => False()
					case Conjunction(sf) =>
						val sub_sf = for (s<-sf) yield substitution(ExistQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
						Conjunction(sub_sf)
					case Disjunction(sf) =>
						val sub_sf = for (s<-sf) yield substitution(ExistQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
						Disjunction(sub_sf)
					case Negation(op) =>
						val sub_op = substitution(ExistQuantifier(vars,op.asInstanceOf[HL_PredicateFormula]),assigned)
						Negation(sub_op)
					case _ => True()

				}

			case UnivQuantifier(vars,f) =>
				f match {
					case p:Predicate =>
						var new_vars : List[VariableTerm] = List.empty
						for (v<-vars) if (!assigned.contains(v.name)) new_vars = v :: new_vars
						if (new_vars.nonEmpty)
							UnivQuantifier(new_vars,substitution(p,assigned))
						else
							substitution(p,assigned)
					case True() => True()
					case False() => False()
					case Conjunction(sf) =>
						val sub_sf = for (s<-sf) yield substitution(UnivQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
						Conjunction(sub_sf)
					case Disjunction(sf) =>
						val sub_sf = for (s<-sf) yield substitution(UnivQuantifier(vars,s.asInstanceOf[HL_PredicateFormula]),assigned)
						Disjunction(sub_sf)
					case Negation(op) =>
						val sub_op = substitution(UnivQuantifier(vars,op.asInstanceOf[HL_PredicateFormula]),assigned)
						Negation(sub_op)
					case _ => True()

				}

			case _ =>  True()
		}
	}

	def pred_substitution(p:Predicate, assigned:Map[String,ConstantTerm]):Predicate = {
		var terms_array : List[Term]=List.empty
		for (t<-p.terms) {
			t match {
				case VariableTerm(name) =>
					if (assigned.contains(name))
						terms_array = assigned(name) :: terms_array
					else
						terms_array = VariableTerm(name) :: terms_array
				case AtomTerm(n) => terms_array = AtomTerm(n) :: terms_array
				case NumeralTerm(n) => terms_array = NumeralTerm(n) :: terms_array
				case StringTerm(s) => terms_array = StringTerm(s) :: terms_array
				case IntegerTerm(i)=> terms_array = IntegerTerm(i) :: terms_array
				case _ =>
			}
		}
		Predicate(p.functional,terms_array.reverse)
	}
}
