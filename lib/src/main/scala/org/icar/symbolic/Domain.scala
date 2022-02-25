package org.icar.symbolic

/******* PLANNING DOMAIN ********/

/**
 * Domain specifies the planning domain specifications in terms of predicates, types and axioms
 *
 * Example:
 * {{{
 * val preds = Array( DomainPredicate("f",List(DomainVariable("N1","doc_num"))) )
 * val dom_types = Array( IntegerRange_DomainType("doc_num",0,3) )
 * val domain_rules = ...
 * Domain("PAPER",preds,dom_types,domain_rules)
 * }}}
 *
 * @param id unique id for the domain
 * @param space array of predicates that generate the planning space
 * @param types array of types (classes of objects) that are ued in the domain
 * @param axioms array of invariant rules (assumptions) that hold in the domain
 */
case class Domain (id:String, space : Array[DomainPredicate], types: Array[DomainType], axioms : Array[Axiom]) {

	def get_predicate_arg_type(functional:String,pos:Int) : DomainArgument = {
		var t:DomainArgument=NullDomainType()
		for (p<-space)
			if (p.functor==functional && p.args.isDefinedAt(pos))
				t = p.args(pos)
		t
	}

}


/******* DOMAIN TYPES ********/

/**
 * the domain could contain typed variables
 * This abstract class identifies a domain type.
 * So far we have integers, enums and strings
 *
 * @param name type name
 */
abstract class DomainType(val name:String) {
	def range : List[ConstantTerm]
}

/**
 * an integer that ranges from a min value to a max value
 *
 * @param name type name
 * @param min min allowed value
 * @param max max allowed value
 */
case class IntegerRange_DomainType(override val name:String, min : Int, max : Int) extends DomainType(name) {
	override def range: List[ConstantTerm] = {
		val numeric_range = (min to max).toList
		for (n <- numeric_range) yield IntegerTerm(n)
	}
}

/**
 * an integer that may assume only specified values
 *
 * @param name type name
 * @param varrange list of ammissible values
 */
case class IntegerEnum_DomainType(override val name:String, varrange:List[Int]) extends DomainType(name) {
	override def range: List[ConstantTerm] = {
		for (n <- varrange) yield IntegerTerm(n)
	}
}

/**
 * a string type that may assume only specified values
 *
 * @param name type name
 * @param enumer list of ammissible values
 */
case class StringEnum_DomainType(override val name:String, enumer : Array[String]) extends DomainType(name) {
	override def range: List[ConstantTerm] = {
		val array = for (e<-enumer) yield AtomTerm(e)
		array.toList
	}
}



/******* DOMAIN PREDICATES ********/

/**
 * An ammissible predicate for the specified domain
 *
 * Example
 * {{{
 *  DomainPredicate("f",List(
 *		DomainVariable("N1","doc_num"),
 *		DomainVariable("N2","doc_num"),
 *		DomainVariable("TYPE","doc_type")
 *	))
 * }}}
 *
 * @param functor signature for the predicate
 * @param args arguments of the predicate
 */
case class DomainPredicate(functor : String, args : List[DomainArgument])

/**
 * The argument of a domain predicate may be a variable or a constant
 */
abstract class DomainArgument {
	def range(types: Array[DomainType]) : List[ConstantTerm]
}

/**
 * A variable to be used in a predicate argument
 *
 * Example: DomainVariable("number","doc_num")
 *
 * @param name variable identifier
 * @param category label used to associate the variable to a domain type registered into the domain
 */
case class DomainVariable(name:String, category : String) extends DomainArgument {
	override def range(types: Array[DomainType]): List[ConstantTerm] =   {
		val tpe = types.find(_.name == category)
		if (tpe.isDefined)
			tpe.get.range
		else
			List.empty
	}
}

/**
 * a generic constant term to be used in a predicate argument
 *
 * Example: DomainConstant("threshold")
 *
 * @param name name of the constant
 */
case class DomainConstant(name : String) extends DomainArgument {
	override def range(types: Array[DomainType]): List[ConstantTerm] = List(AtomTerm(name))
}
case class DomainConstantString(str : String) extends DomainArgument {
	override def range(types: Array[DomainType]): List[ConstantTerm] = List(StringTerm(str))
}
case class NullDomainType() extends DomainArgument {
	override def range(types: Array[DomainType]) : List[ConstantTerm] = List.empty
}




