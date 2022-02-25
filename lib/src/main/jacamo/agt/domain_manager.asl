{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
{ include("$moiseJar/asl/org-obedient.asl") }


/* application domain goals */
//!start.

// when a scheme is created goal init is waiting), I commit to init
//goalState(s1,init,[],[],waiting)[artifact_id(cobj_8),artifact_name(cobj_8,s1),percept_type(obs_prop),source(percept),workspace(cobj_8,musa,cobj_2)]
+goalState(_,init,_,_,waiting)[artifact_id(_),artifact_name(_,s1),percept_type(obs_prop),source(percept),workspace(_,_,_)] <- 
	.print("goal state init waiting");
	.print("I commit mission init");
   commitMission(init).


//+!start <-
//  .wait(5000);
//  .print("sto partendo");
//  commitMission(init).




+!init <- .print("job done").
+!goal_injection <- .print("goal injected").




// signals
+normFailure(N)  <- .print("norm failure event: ", N).
+destroyed(Art)  <- .print("Artifact ",Art," destroyed").
