package org.icar.symbolic

import org.icar.symbolic.{AbstractCapability, StateOfWorld}

/******* PLANNING PROBLEM ********/
case class Problem(I : StateOfWorld, goal_model : GoalModel, actions : AvailableActions)




/******* PLANNING ACTIONS ********/
case class AvailableActions(sys_action : Array[AbstractCapability], env_action : Array[AbstractCapability])













