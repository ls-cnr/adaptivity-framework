package org.icar.symbolic


/******* GOAL SET: LTL SYNTAX DEFINITION ********/
case class GoalModel(goals:Array[GoalSPEC])

/******* GOALSPEC ********/
/*
pre should be a predicate logic formula (ex: a and b)
post can be a linear temporal logic formula
 */
case class GoalSPEC(id:String, pre:HL_LTLFormula, post:HL_LTLFormula)

