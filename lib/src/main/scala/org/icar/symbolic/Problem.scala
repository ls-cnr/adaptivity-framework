package org.icar.symbolic

import org.icar.symbolic.{AbstractCapability, StateOfWorld}

/******* PLANNING PROBLEM ********/
case class Problem(I : StateOfWorld, goal_model : LTLGoalSet, actions : AvailableActions)


/******* GOAL SET: LTL SYNTAX DEFINITION ********/
case class LTLGoalSet(goals:Array[HL_LTLFormula])


/******* PLANNING ACTIONS ********/
case class AvailableActions(sys_action : Array[AbstractCapability], env_action : Array[AbstractCapability])













